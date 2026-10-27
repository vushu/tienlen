let socket = null;


let receivedData = null;
let selectedCards = [];
let sortDirection = 0;   // 0 = server order, 1 = ascending, -1 = descending
let leftSeats = new Set();   // seats of players who left mid-game
let playerNames = {};        // seat -> alias (from the server's names roster)
let amSpectator = false;     // true while watching without a seat
let lobbySeated = null;      // seated count from the last lobby message

// Human-readable messages for the server's error codes.
function friendlyError(code) {
    const messages = {
        not_your_turn: "It's not your turn yet — wait for the other players.",
        illegal_move: "That move isn't allowed. Check that your cards beat what's on the table.",
        no_game: 'No game is running. Press "New Game" to start one.',
        not_enough_players: 'Not enough players yet — at least 2 must be seated.',
        not_a_player: 'You are spectating — only seated players can do that.',
        not_a_spectator: 'Only spectators can take that action.',
        lobby_full: 'The lobby is full (4 players).',
        seat_taken: 'That seat was just taken by someone else.',
        name_empty: 'The name cannot be empty.',
        name_too_long: 'That name is too long — 12 characters maximum.',
        name_taken: 'That name is already taken by someone in the room.',
        unknown_message: 'Unknown message sent to the server.',
        unknown_move: 'Unknown move.'
    };
    return messages[code] || code;
}

// Ask for an alias (first visit). Stored per tab session and re-sent on
// every connect so the server can validate uniqueness.
function askPlayerName() {
    const name = prompt('Pick a name (max 12 characters, unique in the room):');
    if (name && name.trim()) {
        sessionStorage.setItem('tienlen_name', name.trim().slice(0, 12));
    }
    return sessionStorage.getItem('tienlen_name');
}

// Ask for the room code (used before the first connection and whenever
// the stored one turns out to be stale).
function askRoomCode() {
    const code = prompt('Enter the room code for this game:');
    if (code) {
        rememberRoomCode(code.trim());
    }
    return localStorage.getItem(roomKey());
}

// The room code is stored per room (keyed by the page's base path) in
// localStorage: it must survive home-screen app restarts (sessionStorage
// is wiped on every cold launch) and tabs on different rooms must not
// fight over one stored code.
function roomKey() {
    return 'tienlen_room:' + location.pathname.replace(/\/[^/]*$/, '');
}

// Keep the code in a cookie too: the manifest request (Add to Home
// Screen) carries cookies but not the URL fragment, and the server
// bakes the code into the manifest's start_url from it.
function rememberRoomCode(code) {
    localStorage.setItem(roomKey(), code);
    document.cookie = 'tienlen_room=' + encodeURIComponent(code)
        + ';path=' + location.pathname.replace(/\/[^/]*$/, '/')
        + ';max-age=31536000;SameSite=Strict';
    // Reflect the code in the URL (?code=...): the address bar then
    // doubles as a shareable link, and a refresh keeps the code.
    const params = new URLSearchParams(location.search);
    params.set('code', code);
    history.replaceState(null, '', location.pathname + '?' + params.toString());
}

// Room code from the URL query (?code=): shareable links like
// https://host/tienlen/family/?code=tu4u (same style as the admin
// page's ?key=). The code is stored; it stays in the URL so the
// address bar is a shareable link and refreshes keep the code.
function roomCodeFromUrl() {
    const params = new URLSearchParams(location.search);
    const code = (params.get('code') || '').trim();
    if (code) {
        rememberRoomCode(code);
        return true;
    }
    return false;
}

// Probe the stored room code over plain HTTP: a WebSocket handshake
// failure is invisible to browsers (no error message, just close code
// 1006), but fetch() sees the 401 reliably. Returns true when the code
// is accepted (or no code is needed).
async function checkRoomCode() {
    const room = localStorage.getItem(roomKey()) || '';
    if (!room) return true;   // nothing stored: the server may not require one
    const base = location.pathname.replace(/\/[^/]*$/, '');
    try {
        const r = await fetch(base + '/check?room=' + encodeURIComponent(room), { method: 'HEAD' });
        return r.ok || r.status === 404;   // 404: older server without the endpoint
    } catch (e) {
        return true;   // network hiccup: let the WebSocket try anyway
    }
}

function connectSocket() {
    // Session token: survives page refreshes (sessionStorage) so the
    // server can reclaim our seat and hand instead of seating us anew.
    if (!sessionStorage.getItem('tienlen_token')) {
        sessionStorage.setItem('tienlen_token', crypto.randomUUID());
    }
    const token = sessionStorage.getItem('tienlen_token');
    // Room code (if the server requires one): remembered per room, in
    // localStorage so home-screen app launches keep it. A #code URL
    // fragment counts as having entered it. Ask BEFORE the first
    // connection: a 401 rejection mid-handshake makes browsers treat it
    // as failed HTTP authentication and aborts the WebSocket entirely
    // (no retry), so we cannot recover after the fact.
    if (!localStorage.getItem(roomKey()) && !roomCodeFromUrl() && !window.__roomAsked) {
        window.__roomAsked = true;
        askRoomCode();
    }
    // Alias: asked once per tab session, before the first connection.
    if (!sessionStorage.getItem('tienlen_name') && !window.__nameAsked) {
        window.__nameAsked = true;
        askPlayerName();
    }
    const room = localStorage.getItem(roomKey()) || '';
    const roomParam = room ? '&room=' + encodeURIComponent(room) : '';
    // Identity token (persistent rooms): stored in localStorage so it
    // survives browser restarts - the server uses it to own a seat in
    // a long-running game. Separate from the per-tab session token.
    if (!localStorage.getItem('tienlen_ident')) {
        localStorage.setItem('tienlen_ident', crypto.randomUUID());
    }
    const ident = localStorage.getItem('tienlen_ident');
    // Last bomb animation seen: the server replays bombs that happened
    // while we were away (e.g. our 2 was bombed overnight).
    const lastBomb = localStorage.getItem('tienlen_last_bomb') || '0';
    const proto = location.protocol === 'https:' ? 'wss:' : 'ws:';
    const host = location.host;   // works when served from the server itself
    // Base path: the directory this page was served from (supports
    // hosting under a subpath like /tienlen/ behind a reverse proxy).
    const base = location.pathname.replace(/\/[^/]*$/, '');
    socket = new WebSocket(proto + '//' + host + base + '/ws?token=' + token + '&ident=' + ident + '&last_bomb=' + lastBomb + roomParam);

    socket.onopen = () => {
        console.log('Connected to WebSocket server');
        setConnectionStatus(true);
        // Heartbeat: prove the tab is alive while the phone sleeps. A
        // frozen page stops sending, and the server drops silent
        // sockets after a few missed beats - so a locked phone does
        // not leave a zombie seat blocking the game. On wake the
        // browser fires visibilitychange before the interval would:
        // beat immediately so a quick standby does not disconnect us.
        startHeartbeat();
        // Send our alias (stored per tab session) so the server can
        // validate it (unique, <= 12 chars) and tell everyone.
        const name = sessionStorage.getItem('tienlen_name');
        if (name) socket.send(JSON.stringify({ type: 'set_name', name }));
    };

    socket.onmessage = (event) => {
        const data = JSON.parse(event.data);
        if (data.type === 'pong') return;   // heartbeat reply: nothing to do
        if (data.type === 'error') {
            console.warn('Server error:', data.message);
            // Name rejected (empty): re-prompt and retry. Duplicates are
            // auto-suffixed by the server, so no re-prompt for those.
            if (data.message === 'name_empty') {
                sessionStorage.removeItem('tienlen_name');
                window.__nameAsked = false;
                const name = askPlayerName();
                if (name) socket.send(JSON.stringify({ type: 'set_name', name }));
            } else {
                alert(friendlyError(data.message));
            }
            return;
        }
        if (data.type === 'waiting') {
            console.log('Waiting for players...');
            const seated = data.seated ?? 0;
            const needed = data.needed ?? 2;
            const status = needed > 0
                ? `Waiting for players... (${seated} seated, ${needed} more needed)`
                : `${seated} players seated - press "Start game" to begin`;
            document.getElementById('player-turn').textContent = status;
            // Back in the lobby: clear the game view (game-over banner,
            // table, hands) so a finished game does not linger on screen.
            receivedData = null;
            amSpectator = data.spectator === true;
            lobbySeated = seated;
            leftSeats.clear();
            document.getElementById('game-over').hidden = true;
            document.getElementById('cleared-notice').hidden = true;
            document.getElementById('cards-in-play-container').innerHTML = '';
            document.getElementById('hands-container').innerHTML = '';
            // Back in the lobby: the round log makes no sense either.
            document.getElementById('log-toggle').hidden = true;
            document.getElementById('log-panel').hidden = true;
            // No game: Place/Pass and Sort make no sense in the lobby.
            document.querySelector('.main-actions').style.display = 'none';
            document.getElementById('sort-actions').style.display = 'none';
            document.getElementById('new-game-btn').style.display = '';
            document.getElementById('new-game-group').style.display = '';
            // Start game needs enough players: disabled until then, lit
            // once the lobby is ready.
            const startBtn = document.getElementById('new-game-btn');
            startBtn.disabled = needed > 0;
            startBtn.classList.toggle('primary', needed === 0);
            // Spectators (e.g. after an admin kick) can join the lobby
            // manually: a click proves a human is present.
            renderSpectatorUI();
            return;
        }
        if (data.type === 'names') {
            // Roster of aliases: seat -> name (spectators are seat -1).
            playerNames = {};
            data.names.forEach(e => { playerNames[e.seat] = e.name; });
            // The server may have suffixed our name (duplicate): remember
            // the assigned one so a reload doesn't get a new suffix.
            // Spectators all share seat -1, so only trust a real seat.
            const mine = receivedData && receivedData.your_seat >= 0
                ? playerNames[receivedData.your_seat] : null;
            if (mine && mine !== 'null') sessionStorage.setItem('tienlen_name', mine);
            renderNameLabel();
            if (receivedData && receivedData.hands) {
                visualizeHands(receivedData.hands, receivedData.your_seat, receivedData.player_states);
                // The banner labels seats too: re-render it with the
                // freshly known aliases.
                renderGameOver(receivedData);
            }
            return;
        }
        if (data.type === 'event') {
            if (data.event === 'passed') spawnPassToast(data.seat, data.seat === receivedData?.your_seat);
            if (data.event === 'left') {
                leftSeats.add(data.seat);
                spawnLeftToast(data.seat, data.seat === receivedData?.your_seat);
            }
            if (data.event === 'joined') spawnJoinedToast(data.seat);
            if (data.event === 'promoted') spawnPromotedToast(data.seat);
            if (data.event === 'bombed') {
                spawnBombAnimation(data.seat, data.bomb_kind);
                // Remember the highest bomb id: the server replays
                // anything newer when we reconnect.
                if (typeof data.bomb_id === 'number') {
                    localStorage.setItem('tienlen_last_bomb', String(data.bomb_id));
                }
            }
            if (data.event === 'grace_started') startGraceCountdown(data.seat);
            if (data.event === 'grace_ended') stopGraceCountdown();
            if (data.event === 'you_are_seated' && receivedData) {
                receivedData.your_seat = data.seat;
                leftSeats.delete(data.seat);
                amSpectator = false;
                renderSpectatorUI();
            }
            if (data.event === 'seat_offer') offerSeat(data.seat);
            return;
        }
        receivedData = data;
        // A fresh game state rebuilds the hand DOM with new card
        // objects: a pending selection would be invisible and no
        // longer toggleable, so start every broadcast clean.
        selectedCards = [];
        amSpectator = data.your_seat === -1;
        renderSpectatorUI();
        console.log('Received:', data);
        // A game is running: make sure the controls are visible again
        // (they are hidden in the lobby). At game over the round is
        // finished - no moves are legal - so Place/Pass hide; the banner
        // shows "Play another round" / "Start over" instead. Spectators
        // and mid-game joiners (seated but dealt in at the next game)
        // make no moves either: their Place/Pass stay hidden too, and
        // sorting is pointless without a hand of your own.
        const isGameOver = data.next_move.action === 'game_over';
        const canAct = !amSpectator && Array.isArray(data.hands[data.your_seat]);
        document.querySelector('.main-actions').style.display =
            (isGameOver || !canAct) ? 'none' : '';
        document.getElementById('sort-actions').style.display = canAct ? '' : 'none';
        document.getElementById('new-game-btn').style.display = 'none';
        document.getElementById('new-game-group').style.display = 'none';
        document.getElementById('new-game-btn').classList.remove('primary');
        // A fresh deal means no departed players remain.
        if (data.player_states && data.player_states.every(s => s === 'in_play')) {
            leftSeats.clear();
        }
        // Keep the player's chosen sort order across broadcasts: the server
        // stores hands in deal order, so every state re-renders unsorted.
        if (sortDirection !== 0 && data.hands[data.your_seat]) {
            data.hands[data.your_seat] = [...data.hands[data.your_seat]]
                .sort((a, b) => sortDirection * (cardScore(a) - cardScore(b)));
        }
        visualizeHands(data.hands, data.your_seat, data.player_states);
        visualizeCardsInPlay(data.cards_in_play, data.discarded_cards, data.attacker);
        const isYourTurn = data.next_move.player === data.your_seat;
        const forcedPass = isYourTurn && data.next_move.action === 'pass';
        let turnText;
        if (isGameOver) {
            turnText = 'Round finished';
        } else if (forcedPass) {
            turnText = "You can't beat the cards — press Pass";
        } else if (isYourTurn) {
            turnText = "It's YOUR turn!";
        } else {
            turnText = `Waiting for ${seatLabel(data.next_move.player)} to move...`;
        }
        const turnEl = document.getElementById('player-turn');
        turnEl.textContent = turnText;
        turnEl.classList.toggle('your-turn', isYourTurn);
        // Browser notification when it becomes our turn (the tab may be
        // in the background; the title flash covers the minimized case).
        notifyTurn(isYourTurn, isGameOver);
        // During a forced pass, Pass is the only legal action; on an empty
        // table (you have the lead) you must place, so Pass is disabled.
        // Place needs a selection, and is impossible during a forced pass.
        const mustPlace = isYourTurn && data.next_move.action === 'make_move' && data.cards_in_play === 'none';
        const noSelection = selectedCards.length === 0;
        document.getElementById('place-btn').disabled = forcedPass || noSelection;
        document.getElementById('pass-btn').disabled = mustPlace;
        document.getElementById('pass-btn').classList.toggle('primary', forcedPass);
        renderGameOver(data);
        renderClearedNotice(data);
        renderRoundLog(data);
    };

    socket.onerror = (error) => {
        console.error('WebSocket Error:', error);
        window.__lastWsError = String(error.message || error.type || '');
    };

    socket.onclose = (event) => {
        console.log('WebSocket connection closed', event.code);
        setConnectionStatus(false);
        stopHeartbeat();
        // Auto-reconnect so a server kick lands us back in the lobby.
        // Before retrying, probe the stored room code over HTTP: a stale
        // code would otherwise fail the WebSocket handshake invisibly
        // (close 1006, no error message) and loop forever.
        setTimeout(async () => {
            const ok = await checkRoomCode();
            if (!ok) {
                // Stale code: forget it and ask again before reconnecting.
                localStorage.removeItem(roomKey());
                window.__roomAsked = false;
                askRoomCode();
            }
            connectSocket();
        }, 1000);
    };
}

// Heartbeat: a small ping every 20s proves this tab is alive. The
// server drops sockets that stay silent too long, so a phone in
// standby (frozen JS, half-open TCP) does not hold a seat hostage.
// On visibility change (phone unlocked) beat immediately: the
// interval was frozen during standby and the socket may be stale.
let heartbeatTimer = null;

function startHeartbeat() {
    stopHeartbeat();
    heartbeatTimer = setInterval(() => {
        if (socket && socket.readyState === WebSocket.OPEN) {
            socket.send(JSON.stringify({ type: 'ping' }));
        }
    }, 20000);
    // Beat as soon as the tab becomes visible again (phone unlocked,
    // tab switched back): the interval was frozen while hidden.
    document.addEventListener('visibilitychange', beatOnVisible);
}

function stopHeartbeat() {
    if (heartbeatTimer) { clearInterval(heartbeatTimer); heartbeatTimer = null; }
    document.removeEventListener('visibilitychange', beatOnVisible);
}

function beatOnVisible() {
    if (document.visibilityState === 'visible'
        && socket && socket.readyState === WebSocket.OPEN) {
        socket.send(JSON.stringify({ type: 'ping' }));
    }
}

// Browser notification when it becomes our turn: the tab is open but may
// be in the background. Permission is requested on the first user click
// (browsers require a gesture); the title also flashes as a fallback.
let lastNotifiedTurn = false;

function notifyTurn(isYourTurn, isGameOver) {
    if (isYourTurn && !isGameOver && !lastNotifiedTurn) {
        if (document.hidden && Notification.permission === 'granted') {
            new Notification('Tiến Lên', {
                body: "It's your turn!",
                tag: 'tienlen-turn'   // replaces the previous notification
            });
        }
        flashTitle("It's your turn!");
    }
    lastNotifiedTurn = isYourTurn && !isGameOver;
}

// Flash the tab title until the tab is focused: visible even when
// notifications are denied, and in browsers without them.
let titleFlashTimer = null;

function flashTitle(message) {
    stopTitleFlash();
    const original = document.title;
    let on = false;
    titleFlashTimer = setInterval(() => {
        on = !on;
        document.title = on ? '♠ ' + message : original;
    }, 1000);
    const stop = () => { stopTitleFlash(); document.title = original; };
    window.addEventListener('focus', stop, { once: true });
}

function stopTitleFlash() {
    if (titleFlashTimer) { clearInterval(titleFlashTimer); titleFlashTimer = null; }
}

// Request notification permission on the first user interaction
// (browsers require a gesture before granting it).
document.addEventListener('click', function requestNotify() {
    if ('Notification' in window && Notification.permission === 'default') {
        Notification.requestPermission();
    }
}, { once: true });

// iOS standalone with black-translucent status bar: the page extends
// under the notch. env(safe-area-inset-top) is unreliable there (often
// reports 0), so measure the truth: the visual viewport's offset from
// the layout viewport top is exactly the status bar height. Set it as
// a CSS variable the padding rules use as a floor.
function fixSafeArea() {
    const isStandalone = window.matchMedia('(display-mode: standalone)').matches
        || window.navigator.standalone === true;
    if (!isStandalone) return;
    const apply = () => {
        const inset = window.visualViewport
            ? Math.round(window.visualViewport.offsetTop)
            : 0;
        // Fallback: probe env() itself; if both lie, use the typical
        // notch height (47px on iPhone 13 Pro Max).
        let sat = inset;
        if (sat < 20) {
            const probe = document.createElement('div');
            probe.style.cssText =
                'position:fixed;top:0;height:env(safe-area-inset-top);visibility:hidden;pointer-events:none;';
            document.body.appendChild(probe);
            sat = probe.getBoundingClientRect().height;
            probe.remove();
        }
        if (sat < 20) sat = 47;   // typical notch height
        document.documentElement.style.setProperty('--sat', sat + 'px');
    };
    apply();
    // Re-measure on resize/rotation (the inset changes in landscape).
    window.addEventListener('orientationchange', () => setTimeout(apply, 300));
}
fixSafeArea();

// The display label for a seat: the player's alias if set, else
// "Player N". Spectators (seat -1) have no number.
function seatLabel(seat) {
    const name = playerNames[seat];
    if (name && name !== 'null') return name;
    return `Player ${seat + 1}`;
}

// Spectator indication: a badge in the header plus a "Join game"
// button (in the lobby it seats immediately; during a game the joiner
// is dealt in at the next game, or takes an open departed seat).
// A mid-game joiner is seated but has no hand yet: they keep a badge
// ("Waiting for next round") until the next game deals them in.
function renderSpectatorUI() {
    const badge = document.getElementById('spectator-badge');
    const inGame = receivedData && receivedData.hands;
    // Seated but not dealt into the running game (joined mid-game):
    // still watching, but the seat is secured for the next round.
    const waitingForRound = !amSpectator && inGame
        && receivedData.your_seat >= 0
        && !Array.isArray(receivedData.hands[receivedData.your_seat]);
    if (badge) {
        badge.hidden = !amSpectator && !waitingForRound;
        badge.textContent = waitingForRound
            ? 'Spectating — joining next round'
            : 'Spectating';
    }
    const joinBtn = document.getElementById('join-game-btn');
    if (!amSpectator) {
        joinBtn?.remove();
        return;
    }
    if (inGame) {
        // The server validates: an open (departed) seat is taken, a
        // full lobby is rejected with a clear error.
        if (!joinBtn) showJoinLobbyButton();
        else updateJoinButtonLabel();
    } else if (lobbySeated !== null && lobbySeated < 4) {
        if (!joinBtn) showJoinLobbyButton();
    } else {
        joinBtn?.remove();
    }
}

// Keep the join button's label in sync with the game state: created in
// the lobby ("Join as player"), relabeled once a game is running
// ("Join next game" - the joiner is dealt in at the next game).
function updateJoinButtonLabel() {
    const btn = document.getElementById('join-game-btn');
    if (!btn) return;
    const inGame = receivedData && receivedData.hands;
    btn.textContent = inGame ? 'Join next game' : 'Join as player';
    btn.title = inGame
        ? 'Take a seat; you are dealt in when the next game starts'
        : 'Take a seat and play; otherwise you watch as a spectator';
}

// Show our own alias next to the title, with a click-to-change control.
function renderNameLabel() {
    let el = document.getElementById('my-name');
    if (!el) {
        el = document.createElement('button');
        el.id = 'my-name';
        el.title = 'Change your name';
        el.onclick = () => {
            const name = askPlayerName();
            if (name) socket.send(JSON.stringify({ type: 'set_name', name }));
        };
        document.querySelector('header h1').after(el);
    }
    // Our name: from the roster when we know our seat. Spectators all
    // share roster seat -1, so fall back to the stored name for them.
    let mine = null;
    if (receivedData && receivedData.your_seat >= 0) {
        mine = playerNames[receivedData.your_seat];
    }
    if (!mine || mine === 'null') mine = sessionStorage.getItem('tienlen_name');
    el.textContent = mine || 'set name';
}

// Show/hide the connection banner. When disconnected, also disable the
// game controls so stale state can't be acted on.
function setConnectionStatus(connected) {
    const banner = document.getElementById('connection-status');
    if (banner) banner.hidden = connected;
    document.getElementById('place-btn').disabled = !connected;
    document.getElementById('pass-btn').disabled = !connected;
    if (!connected) {
        document.getElementById('player-turn').textContent = 'Connection lost — reconnecting...';
    }
}

connectSocket();

// Re-render the hands when crossing the compact breakpoint so the
// opponent representation switches between card backs and count badges.
window.matchMedia('(max-width: 640px)').addEventListener('change', (e) => {
    if (receivedData && receivedData.hands) {
        visualizeHands(receivedData.hands, receivedData.your_seat, receivedData.player_states);
    }
});

function visualizeHands(hands, yourSeat, playerStates) {
    const container = document.getElementById('hands-container');
    container.innerHTML = ''; // Clear previous content
    // On small screens, opponents render as a compact count badge
    // (more room for the current player's cards).
    const compact = window.matchMedia('(max-width: 640px)').matches;
    // Render your own hand first, then the others in seat order
    const order = [...hands.keys()].sort((a, b) =>
        (a === yourSeat ? -1 : 0) - (b === yourSeat ? -1 : 0));
    order.forEach(index => {
        const hand = hands[index];
        const isYours = index === yourSeat;
        const handDiv = document.createElement('div');
        handDiv.className = isYours ? 'hand your-hand' : 'hand opponent-hand';
        const label = isYours ? `Your hand (${seatLabel(index)})` : seatLabel(index);
        const state = playerStates && playerStates[index];
        let stateBadge = '';
        if (leftSeats.has(index)) stateBadge = ' <span class="pass-badge left-badge">left</span>';
        else if (state === 'pass') stateBadge = ' <span class="pass-badge">passed</span>';
        if (!isYours && compact) {
            // Compact opponent: name + card count, no card backs.
            handDiv.classList.add('compact');
            handDiv.innerHTML = `<h3>${label}${stateBadge} <span class="card-count">${hand.length} cards</span></h3>`;
            container.appendChild(handDiv);
            return;
        }
        handDiv.innerHTML = `<h3>${label}${stateBadge}</h3>`;
        const row = document.createElement('div');
        row.className = 'cards-row';
        hand.forEach(card => {
            if (card === null) {
                // Hidden opponent card: render a card back
                const back = document.createElement('span');
                back.className = 'card back';
                row.appendChild(back);
            } else {
                const cardEl = createCardElement(card);
                cardEl.classList.add('clickable');
                // Re-renders (rename, window resize) rebuild the DOM:
                // restore the visual state of cards still selected.
                if (selectedCards.some(c => c.rank === card.rank && c.suit === card.suit)) {
                    cardEl.classList.add('selected');
                }
                cardEl.onclick = () => selectCard(card, index, cardEl);
                row.appendChild(cardEl);
            }
        });
        handDiv.appendChild(row);
        container.appendChild(handDiv);
    });
}

// Build a styled card element from a card object {rank, suit}.
function createCardElement(card) {
    const el = document.createElement('span');
    el.className = 'card';
    const rank = document.createElement('span');
    rank.className = 'card-rank';
    rank.textContent = String(card.rank).toUpperCase();
    const suit = document.createElement('span');
    suit.className = 'card-suit';
    suit.textContent = suitSymbol(card.suit);
    el.appendChild(rank);
    el.appendChild(suit);
    if (card.suit === 'hearts' || card.suit === 'diamonds') {
        el.classList.add('red');
    }
    return el;
}

function suitSymbol(suit) {
    return { hearts: '♥', diamonds: '♦', clubs: '♣', spades: '♠' }[suit] || '';
}

function spawnPassToast(seat, isYou) {
    const toast = document.createElement('div');
    toast.className = 'pass-toast';
    toast.textContent = isYou ? 'You passed' : `${seatLabel(seat)} passed`;
    document.body.appendChild(toast);
    // remove after the animation completes
    setTimeout(() => toast.remove(), 2200);
}

function spawnLeftToast(seat, isYou) {
    const toast = document.createElement('div');
    toast.className = 'pass-toast left-toast';
    toast.textContent = isYou ? 'You left the game' : `${seatLabel(seat)} left the game`;
    document.body.appendChild(toast);
    setTimeout(() => toast.remove(), 2200);
}

function spawnJoinedToast(seat) {
    const toast = document.createElement('div');
    toast.className = 'pass-toast joined-toast';
    toast.textContent = `${seatLabel(seat)} seated — joins the next game`;
    document.body.appendChild(toast);
    setTimeout(() => toast.remove(), 2200);
}

// Grace countdown: a player disconnected mid-game; show how long until
// they are treated as gone (10s window to reconnect).
let graceTimer = null;

function startGraceCountdown(seat) {
    stopGraceCountdown();
    const banner = document.createElement('div');
    banner.id = 'grace-countdown';
    banner.className = 'grace-countdown';
    document.body.appendChild(banner);
    // Persistent room: the seat is owned, the player is just away -
    // no countdown, the game simply waits for them to return.
    if (receivedData && receivedData.persistent) {
        banner.textContent = `${seatLabel(seat)} is away — the game waits for their return`;
        return;
    }
    let remaining = 10;
    const render = () => {
        banner.textContent = `${seatLabel(seat)} disconnected — waiting to reconnect (${remaining}s)`;
    };
    render();
    graceTimer = setInterval(() => {
        remaining--;
        if (remaining <= 0) { stopGraceCountdown(); return; }
        render();
    }, 1000);
}

function stopGraceCountdown() {
    if (graceTimer) { clearInterval(graceTimer); graceTimer = null; }
    document.getElementById('grace-countdown')?.remove();
}

// A 2 was bombed: explosion animation over the table.
function spawnBombAnimation(seat, kind) {
    const table = document.getElementById('table');
    if (!table) return;
    const rect = table.getBoundingClientRect();
    const bomb = document.createElement('div');
    bomb.className = 'bomb-animation';
    bomb.textContent = '💥';
    bomb.style.left = (rect.left + rect.width / 2) + 'px';
    bomb.style.top = (rect.top + rect.height / 2) + 'px';
    document.body.appendChild(bomb);
    // the banner with the bomber's name; what was bombed depends on
    // the kind (single 2, pair of 2s, or triple 2s)
    const target = { pair: 'the PAIR of 2s', triple: 'the TRIPLE 2s' }[kind] || 'the 2';
    const banner = document.createElement('div');
    banner.className = 'bomb-banner';
    banner.textContent = `${seatLabel(seat)} BOMBED ${target}!`;
    document.body.appendChild(banner);
    setTimeout(() => bomb.remove(), 1400);
    setTimeout(() => banner.remove(), 2400);
}

function spawnPromotedToast(seat) {
    const toast = document.createElement('div');
    toast.className = 'pass-toast joined-toast';
    toast.textContent = `Spectator took seat ${seat + 1} — plays next game`;
    document.body.appendChild(toast);
    setTimeout(() => toast.remove(), 2200);
}

// A seat opened up: ask the spectator if they want it. Declining passes
// the offer to the next spectator in line.
function offerSeat(seat) {
    // Don't stack prompts if one is already open.
    if (document.getElementById('seat-offer')) return;
    const dialog = document.createElement('div');
    dialog.id = 'seat-offer';
    dialog.className = 'seat-offer';
    dialog.innerHTML = `
        <div class="seat-offer-box">
            <p>A seat opened up (${seatLabel(seat)} left). Take the seat?</p>
            <p class="seat-offer-note">You'll join the next game.</p>
            <div class="seat-offer-buttons">
                <button class="primary" id="seat-accept">Take the seat</button>
                <button id="seat-decline">Keep watching</button>
            </div>
        </div>`;
    document.body.appendChild(dialog);
    const close = () => dialog.remove();
    dialog.querySelector('#seat-accept').onclick = () => {
        socket.send(JSON.stringify({ type: 'accept_seat', seat }));
        close();
    };
    dialog.querySelector('#seat-decline').onclick = () => {
        socket.send(JSON.stringify({ type: 'decline_seat', seat }));
        close();
    };
}

// Spectator's "Join as player" button: joining is an explicit choice,
// so people who just want to watch don't occupy a seat. In the lobby it
// takes effect immediately; during a running game the joiner is dealt
// in at the next game.
function showJoinLobbyButton() {
    const btn = document.createElement('button');
    btn.id = 'join-game-btn';
    btn.className = 'primary';
    const inGame = receivedData && receivedData.hands;
    btn.textContent = inGame ? 'Join next game' : 'Join as player';
    btn.title = inGame
        ? 'Take a seat; you are dealt in when the next game starts'
        : 'Take a seat and play; otherwise you watch as a spectator';
    btn.onclick = () => {
        socket.send(JSON.stringify({ type: 'join' }));
        btn.remove();
    };
    document.getElementById('controls').prepend(btn);
}

// Game over: show final placements (1st, 2nd, ...) from player_states,
// plus the accumulated round history from the scoreboard.
function renderGameOver(data) {
    const banner = document.getElementById('game-over');
    if (data.next_move.action !== 'game_over') {
        banner.hidden = true;
        return;
    }
    banner.hidden = false;
    const placements = data.player_states
        .map((place, seat) => ({ place, seat }))
        .filter(p => p.place !== 'in_play')
        .sort((a, b) => placeOrder(a.place) - placeOrder(b.place));
    const rows = placements.map(p =>
        `<span class="placement${p.seat === data.your_seat ? ' you' : ''}">` +
        `${ordinal(placeOrder(p.place))}: ${seatLabel(p.seat)}${p.seat === data.your_seat ? ' (You)' : ''}</span>`);
    let history = '';
    if (Array.isArray(data.scoreboard) && data.scoreboard.length > 0) {
        const rounds = data.scoreboard.map((round, i) => {
            const items = round.map((place, seat) =>
                `<span class="round-item${seat === data.your_seat ? ' you' : ''}">${ordinal(placeOrder(place))}: ${seatLabel(seat)}</span>`);
            return `<div class="round-row"><span class="round-label">R${i + 1}</span>${items.join('')}</div>`;
        });
        history = `<div class="round-history">${rounds.join('')}</div>`;
    }
    banner.innerHTML = `<h2>Game over!</h2><div class="placements">${rows.join('')}</div>${history}` +
        (amSpectator ? '' :
        `<div class="game-over-actions">` +
        `<button class="primary" onclick="sendNewGame()">Play again</button>` +
        `<button onclick="sendStartOver()">Start over</button>` +
        `</div>`);
}

function placeOrder(place) {
    return { first: 1, second: 2, third: 3, fourth: 4 }[place] || 99;
}

function ordinal(n) {
    return { 1: '1st', 2: '2nd', 3: '3rd', 4: '4th' }[n] || `${n}th`;
}

// Pile cleared back to the attacker: it's your turn, the table is empty,
// and the last discarded pile was yours (you were the attacker).
function renderClearedNotice(data) {
    const el = document.getElementById('cleared-notice');
    if (!el) return;
    const clearedToYou = data.cards_in_play === 'none'
        && data.next_move.player === data.your_seat
        && data.next_move.action === 'make_move'
        && data.attacker === data.your_seat
        && data.discarded_cards.length > 0;
    el.hidden = !clearedToYou;
    if (clearedToYou) {
        el.textContent = "It's your turn again — the other players passed";
    }
}

// Round log: the history of the current round (who did what, in order).
// A button in the bottom-right corner toggles the panel; the log only
// exists while a round is running (cleared on each new deal).
function renderRoundLog(data) {
    const toggle = document.getElementById('log-toggle');
    const panel = document.getElementById('log-panel');
    if (!toggle || !panel) return;
    const entries = Array.isArray(data.move_log) ? data.move_log : [];
    // No game (lobby): hide the log entirely.
    if (!data.hands) {
        toggle.hidden = true;
        panel.hidden = true;
        return;
    }
    toggle.hidden = false;
    if (panel.hidden) return;   // only re-render when visible
    panel.innerHTML = logHtml(entries);
}

function toggleLog() {
    const panel = document.getElementById('log-panel');
    if (!panel) return;
    panel.hidden = !panel.hidden;
    if (!panel.hidden && receivedData) {
        const entries = Array.isArray(receivedData.move_log) ? receivedData.move_log : [];
        panel.innerHTML = logHtml(entries);
        panel.scrollTop = panel.scrollHeight;   // newest at the bottom
    }
}

function logHtml(entries) {
    if (!entries.length) {
        return '<h3>This round</h3><div class="log-empty">No moves yet</div>';
    }
    const rows = entries.map(e => {
        const who = seatLabel(e.seat);
        const cards = (e.cards || []).map(c =>
            `<span class="mini-card${isRed(c) ? ' red' : ''}">${rankText(c.rank)}${suitText(c.suit)}</span>`
        ).join('');
        // Placements show just the cards (playing is implicit); only
        // passes get a word. Bombs lead with the word, plain text.
        const what = e.action === 'pass'
            ? 'passed'
            : e.bomb
                ? `bombed <span class="mini-cards">${cards}</span>`
                : `<span class="mini-cards">${cards}</span>`;
        return `<div class="log-entry${e.bomb ? ' bomb' : ''}">` +
            `<span class="who">${who}</span>` +
            `<span class="what">${what}${e.bomb ? ' 💥' : ''}</span>` +
            `</div>`;
    }).join('');
    return `<h3>This round</h3>${rows}`;
}

function isRed(card) {
    return card && (card.suit === 'hearts' || card.suit === 'diamonds');
}

function rankText(rank) {
    return { 11: 'J', 12: 'Q', 13: 'K', 14: 'A', 15: '2' }[rank] || rank;
}

function suitText(suit) {
    return { hearts: '♥', diamonds: '♦', clubs: '♣', spades: '♠' }[suit] || '';
}

function drawCard(card, faded = false) {
    const container = document.getElementById('cards-in-play-container');
    const cardEl = createCardElement(card);
    if (faded) cardEl.classList.add('faded');
    container.appendChild(cardEl);
}

function clearCardsInPlay() {
    const container = document.getElementById('cards-in-play-container');
    container.innerHTML = ''; // Clear previous content
}

function visualizeCardsInPlay(cardsInPlay, discardedCards, placedBy) {
    clearCardsInPlay();
    if (cardsInPlay === 'none') {
        // Table empty: show the last cleared pile faded, so a move that
        // nobody could beat stays visible after the auto-clear.
        const last = discardedCards && discardedCards.length > 0
            ? discardedCards[discardedCards.length - 1] : null;
        if (last && last !== 'none') visualizeCardsInPlay(last, undefined, placedBy);
        document.querySelectorAll('#cards-in-play-container .card')
            .forEach(c => c.classList.add('faded'));
        return;
    }
    // Respect the chosen sort order on the table too.
    const sorted = cip => ({ ...cip, cards: [...cip.cards].sort((a, b) => sortDirection * (cardScore(a) - cardScore(b))) });
    switch (cardsInPlay.type) {
        case 'pair':
        case 'three_of_kind':
        case 'four_of_kind':
        case 'sequence':
        case 'double_sequence':
            sorted(cardsInPlay).cards.forEach(card => drawCard(card));
            break;
        case 'single':
            drawCard(cardsInPlay.card);
            break;
    }
    if (placedBy !== undefined && placedBy !== null) {
        const label = document.createElement('div');
        label.className = 'placed-by';
        label.textContent = `placed by ${seatLabel(placedBy)}`;
        document.getElementById('cards-in-play-container').appendChild(label);
    }
}

function selectCard(card, playerIndex, cardSpan) {
    const playerTurn = receivedData.next_move.player;
    const forcedPass = receivedData.next_move.action === 'pass';
    if (playerIndex === playerTurn && !forcedPass) {
        const cardIndex = selectedCards.indexOf(card);
        if (cardIndex > -1) {
            selectedCards.splice(cardIndex, 1);
            cardSpan.classList.remove('selected');
        } else {
            selectedCards.push(card);
            cardSpan.classList.add('selected');
        }
        // Place requires a selection: keep its enabled state in sync.
        document.getElementById('place-btn').disabled = selectedCards.length === 0;
        console.log('Selected Cards:', selectedCards);
    }
}

function isThreeOfKind(cards) {
    if (cards.length !== 3) return false;
    const rank = cards[0].rank;
    return cards.every(card => card.rank === rank);
}

function isFourOfKind(cards) {
    if (cards.length !== 4) return false;
    const rank = cards[0].rank;
    return cards.every(card => card.rank === rank);
}

function isSequence(cards) {
    if (cards.length < 3) return false;
    const ranks = cards.map(card => getRankScore(card.rank)).sort((a, b) => a - b);
    for (let i = 0; i < ranks.length - 1; i++) {
        if (ranks[i] + 1 !== ranks[i + 1]) {
            return false;
        }
    }
    return true;
}

function isDoubleSequence(cards) {
    if (cards.length < 6 || cards.length % 2 !== 0) return false;
    const ranks = cards.map(card => getRankScore(card.rank)).sort((a, b) => a - b);
    for (let i = 0; i < ranks.length - 2; i += 2) {
        if (ranks[i] !== ranks[i + 1] || ranks[i] + 1 !== ranks[i + 2]) {
            return false;
        }
    }
    return true;
}

function isPair(cards) {
    return cards.length === 2 && cards[0].rank === cards[1].rank;
}

function isSingle(cards) {
    return cards.length === 1;
}

// Describe the selected cards as a combination (or why they aren't one).
function describeSelection(cards) {
    if (cards.length === 0) return { error: 'No cards selected' };
    if (isSingle(cards)) return { type: 'single', name: 'a single card' };
    if (isPair(cards)) return { type: 'pair', name: 'a pair' };
    if (isThreeOfKind(cards)) return { type: 'three_of_kind', name: 'three of a kind' };
    if (isFourOfKind(cards)) return { type: 'four_of_kind', name: 'four of a kind' };
    if (isSequence(cards)) return { type: 'sequence', name: 'a sequence' };
    if (isDoubleSequence(cards)) return { type: 'double_sequence', name: 'a double sequence' };
    return { error: `${cards.length} cards don't form a valid combination` };
}

// Human-readable name of what's on the table.
function describeTable() {
    const cip = receivedData.cards_in_play;
    if (cip === 'none' || cip === undefined) return 'an empty table';
    const names = {
        single: 'a single card', pair: 'a pair', three_of_kind: 'three of a kind',
        four_of_kind: 'four of a kind', sequence: 'a sequence', double_sequence: 'a double sequence'
    };
    return names[cip.type] || cip.type;
}

function sendDataBack() {
    if (!receivedData) return;
    const selectedCardsData = [...selectedCards];
    const sel = describeSelection(selectedCardsData);
    if (sel.error) {
        alert(sel.error);
        return;
    }
    // Check the selection matches what's on the table (or is a bomb).
    const cip = receivedData.cards_in_play;
    const tableType = (cip === 'none' || cip === undefined) ? null : cip.type;
    const isBombOn2 =
        (sel.type === 'four_of_kind' && tableType === 'single' && cip.card.rank === 2) ||
        (sel.type === 'double_sequence' && tableType === 'single' && cip.card.rank === 2) ||
        (sel.type === 'double_sequence' && tableType === 'pair' && cip.cards[0].rank === 2);
    if (tableType && sel.type !== tableType && !isBombOn2) {
        alert(`You selected ${sel.name}, but the table has ${describeTable()} — you must play the same combination type.`);
        return;
    }
    let place = null;
    if (sel.type === 'single') {
        place = { card: selectedCardsData[0], type: 'single' };
    } else if (sel.type === 'pair') {
        place = { cards: selectedCardsData, type: 'pair' };
    } else if (sel.type === 'three_of_kind') {
        place = { cards: selectedCardsData, type: 'three_of_kind' };
    } else if (sel.type === 'four_of_kind') {
        place = { cards: selectedCardsData, type: 'four_of_kind' };
    } else if (sel.type === 'sequence') {
        place = { cards: sortCards(selectedCardsData), type: 'sequence' };
    } else if (sel.type === 'double_sequence') {
        place = { cards: sortCards(selectedCardsData), type: 'double_sequence' };
    }
    socket.send(JSON.stringify({ type: 'move', action: { place } }));
    selectedCards = [];
}

function sortCards(cards) {
    return [...cards].sort((a, b) => getRankScore(a.rank) - getRankScore(b.rank));
}

function getRankScore(rank) {
    if (rank === 'j') return 11;
    if (rank === 'q') return 12;
    if (rank === 'k') return 13;
    if (rank === 'a') return 14;
    return rank;
}

function sendPass() {
    if (!receivedData) return;
    socket.send(JSON.stringify({ type: 'move', action: 'pass' }));
    selectedCards = [];
}

function sendNewGame() {
    leftSeats.clear();
    socket.send(JSON.stringify({ type: 'new_game' }));
    selectedCards = [];
}

// "Start over" from the game-over screen: a fresh game with the
// scoreboard wiped (no round history carried over).
function sendStartOver() {
    leftSeats.clear();
    socket.send(JSON.stringify({ type: 'start_over' }));
    selectedCards = [];
}

// Sort your own hand by the engine's card score (rank, then suit) and
// re-render. Direction: 1 = ascending, -1 = descending. The choice is
// remembered and re-applied to every broadcast (the server stores hands
// in deal order). Also sorts the cards on the table. Only affects the
// local display.
function sortHand(direction) {
    if (!receivedData || receivedData.your_seat === undefined || receivedData.your_seat < 0) return;
    const seat = receivedData.your_seat;
    const hand = receivedData.hands[seat];
    // Nothing to sort: empty hand (game over) or hidden cards.
    if (!Array.isArray(hand) || !hand.length || hand.some(c => !c)) return;
    sortDirection = direction;
    receivedData.hands[seat] = [...hand].sort((a, b) => direction * (cardScore(a) - cardScore(b)));
    selectedCards = [];
    visualizeHands(receivedData.hands, seat, receivedData.player_states);
    visualizeCardsInPlay(receivedData.cards_in_play, receivedData.discarded_cards, receivedData.attacker);
}

function cardScore(card) {
    const rankValues = { 2: 15, j: 11, q: 12, k: 13, a: 14 };
    const suitValues = { hearts: 4, diamonds: 3, clubs: 2, spades: 1 };
    const rank = rankValues[card.rank] ?? card.rank;
    return rank * 10 + (suitValues[card.suit] ?? 0);
}

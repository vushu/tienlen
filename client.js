const socket = new WebSocket('ws://localhost:8080'); // Replace with your WebSocket server URL

const hearts = new Set(['🂲', '🂳', '🂴', '🂵', '🂶', '🂷', '🂸', '🂹', '🂺', '🂻', '🂽', '🂾', '🂱']);
const diamonds = new Set(['🃂', '🃃', '🃄', '🃅', '🃆', '🃇', '🃈', '🃉', '🃊', '🃋', '🃍', '🃎', '🃁']);
const clubs = new Set(['🃒', '🃓', '🃔', '🃕', '🃖', '🃗', '🃘', '🃙', '🃚', '🃛', '🃝', '🃞', '🃑']);
const spades = new Set(['🂢', '🂣', '🂤', '🂥', '🂦', '🂧', '🂨', '🂩', '🂪', '🂫', '🂭', '🂮', '🂡']);

let receivedData = null;
let selectedCards = [];

function card_as_unicode(card) {
    const unicodeMap = {
        '2,hearts': '🂲', '3,hearts': '🂳', '4,hearts': '🂴', '5,hearts': '🂵', '6,hearts': '🂶', '7,hearts': '🂷',
        '8,hearts': '🂸', '9,hearts': '🂹', '10,hearts': '🂺', 'j,hearts': '🂻', 'q,hearts': '🂽', 'k,hearts': '🂾', 'a,hearts': '🂱',
        '2,diamonds': '🃂', '3,diamonds': '🃃', '4,diamonds': '🃄', '5,diamonds': '🃅', '6,diamonds': '🃆', '7,diamonds': '🃇',
        '8,diamonds': '🃈', '9,diamonds': '🃉', '10,diamonds': '🃊', 'j,diamonds': '🃋', 'q,diamonds': '🃍', 'k,diamonds': '🃎', 'a,diamonds': '🃁',
        '2,clubs': '🃒', '3,clubs': '🃓', '4,clubs': '🃔', '5,clubs': '🃕', '6,clubs': '🃖', '7,clubs': '🃗',
        '8,clubs': '🃘', '9,clubs': '🃙', '10,clubs': '🃚', 'j,clubs': '🃛', 'q,clubs': '🃝', 'k,clubs': '🃞', 'a,clubs': '🃑',
        '2,spades': '🂢', '3,spades': '🂣', '4,spades': '🂤', '5,spades': '🂥', '6,spades': '🂦', '7,spades': '🂧',
        '8,spades': '🂨', '9,spades': '🂩', '10,spades': '🂪', 'j,spades': '🂫', 'q,spades': '🂭', 'k,spades': '🂮', 'a,spades': '🂡'
    };
    return unicodeMap[`${card[0]},${card[1]}`];
}

function cardToUnicode(card) {
    const rank = card.rank;
    const suit = card.suit;
    const rankMap = {
        2: '2', 3: '3', 4: '4', 5: '5', 6: '6', 7: '7', 8: '8', 9: '9', 10: '10',
        'j': 'j', 'q': 'q', 'k': 'k', 'a': 'a'
    };
    const suitMap = {
        'hearts': 'hearts', 'diamonds': 'diamonds', 'clubs': 'clubs', 'spades': 'spades'
    };
    return card_as_unicode([rankMap[rank], suitMap[suit]]);
}

function unicodeToCard(unicode) {
    const unicodeMap = {
        '🂲': { rank: 2, suit: 'hearts' }, '🂳': { rank: 3, suit: 'hearts' }, '🂴': { rank: 4, suit: 'hearts' },
        '🂵': { rank: 5, suit: 'hearts' }, '🂶': { rank: 6, suit: 'hearts' }, '🂷': { rank: 7, suit: 'hearts' },
        '🂸': { rank: 8, suit: 'hearts' }, '🂹': { rank: 9, suit: 'hearts' }, '🂺': { rank: 10, suit: 'hearts' },
        '🂻': { rank: 'j', suit: 'hearts' }, '🂽': { rank: 'q', suit: 'hearts' }, '🂾': { rank: 'k', suit: 'hearts' },
        '🂱': { rank: 'a', suit: 'hearts' }, '🃂': { rank: 2, suit: 'diamonds' }, '🃃': { rank: 3, suit: 'diamonds' },
        '🃄': { rank: 4, suit: 'diamonds' }, '🃅': { rank: 5, suit: 'diamonds' }, '🃆': { rank: 6, suit: 'diamonds' },
        '🃇': { rank: 7, suit: 'diamonds' }, '🃈': { rank: 8, suit: 'diamonds' }, '🃉': { rank: 9, suit: 'diamonds' },
        '🃊': { rank: 10, suit: 'diamonds' }, '🃋': { rank: 'j', suit: 'diamonds' }, '🃍': { rank: 'q', suit: 'diamonds' },
        '🃎': { rank: 'k', suit: 'diamonds' }, '🃁': { rank: 'a', suit: 'diamonds' }, '🃒': { rank: 2, suit: 'clubs' },
        '🃓': { rank: 3, suit: 'clubs' }, '🃔': { rank: 4, suit: 'clubs' }, '🃕': { rank: 5, suit: 'clubs' },
        '🃖': { rank: 6, suit: 'clubs' }, '🃗': { rank: 7, suit: 'clubs' }, '🃘': { rank: 8, suit: 'clubs' },
        '🃙': { rank: 9, suit: 'clubs' }, '🃚': { rank: 10, suit: 'clubs' }, '🃛': { rank: 'j', suit: 'clubs' },
        '🃝': { rank: 'q', suit: 'clubs' }, '🃞': { rank: 'k', suit: 'clubs' }, '🃑': { rank: 'a', suit: 'clubs' },
        '🂢': { rank: 2, suit: 'spades' }, '🂣': { rank: 3, suit: 'spades' }, '🂤': { rank: 4, suit: 'spades' },
        '🂥': { rank: 5, suit: 'spades' }, '🂦': { rank: 6, suit: 'spades' }, '🂧': { rank: 7, suit: 'spades' },
        '🂨': { rank: 8, suit: 'spades' }, '🂩': { rank: 9, suit: 'spades' }, '🂪': { rank: 10, suit: 'spades' },
        '🂫': { rank: 'j', suit: 'spades' }, '🂭': { rank: 'q', suit: 'spades' }, '🂮': { rank: 'k', suit: 'spades' },
        '🂡': { rank: 'a', suit: 'spades' }
    };
    return unicodeMap[unicode];
}

socket.onopen = () => {
    console.log('Connected to WebSocket server');
    // socket.send('init_game'); // Send a message when connected
    // socket.send('say_hi("Hello Server")'); // Send a message when connected
};

socket.onmessage = (event) => {
    console.log('Received:', event.data); // Log incoming messages
    receivedData = JSON.parse(event.data);
    const hands = receivedData.hands.map(hand => hand.map(card => cardToUnicode(card)));
    console.log('Parsed Hands:', hands);
    visualizeHands(hands);
    visualizeCardsInPlay(receivedData.cards_in_play);
    document.getElementById('player-turn').textContent = "Player turn: " + (receivedData.next_move.player + 1);
    document.getElementById('player-action').textContent = JSON.stringify(receivedData.next_move.action);

};

socket.onerror = (error) => {
    console.error('WebSocket Error:', error);
};

socket.onclose = () => {
    console.log('WebSocket connection closed');
};

function visualizeHands(hands) {
    const container = document.getElementById('hands-container');
    container.innerHTML = ''; // Clear previous content
    hands.forEach((hand, index) => {
        const handDiv = document.createElement('div');
        handDiv.className = 'hand';
        handDiv.innerHTML = `<h3>Player ${index + 1}</h3>`;
        hand.forEach(card => {
            const cardSpan = document.createElement('span');
            cardSpan.className = 'card';
            cardSpan.textContent = card;
            cardSpan.onclick = () => selectCard(card, index, cardSpan);
            if (hearts.has(card) || diamonds.has(card)) {
                cardSpan.style.color = 'red';
            }
            handDiv.appendChild(cardSpan);
        });
        container.appendChild(handDiv);
    });
}
function drawCard(cardsInPlay) {
    const container = document.getElementById('cards-in-play-container');
    const card = cardToUnicode(cardsInPlay);
    const cardSpan = document.createElement('span');
    cardSpan.className = 'card';
    cardSpan.textContent = card;
    if (hearts.has(card) || diamonds.has(card)) {
        cardSpan.style.color = 'red';
    }
    container.appendChild(cardSpan);
}

function clearCardsInPlay() {
    const container = document.getElementById('cards-in-play-container');
    container.innerHTML = ''; // Clear previous content
}

function visualizeCardsInPlay(cardsInPlay) {
    if (cardsInPlay === 'none') {
        const container = document.getElementById('cards-in-play-container');
        container.innerHTML = ''; // Clear previous content
        return
    }
    clearCardsInPlay();
    switch (cardsInPlay.type) {
        case 'pair':
            cardsInPlay.cards.forEach(card => drawCard(card));
            break;
        case 'three_of_kind':
            cardsInPlay.cards.forEach(card => drawCard(card));
            break;
        case 'four_of_kind':
            cardsInPlay.cards.forEach(card => drawCard(card));
            break;
        case 'sequence':
            cardsInPlay.cards.forEach(card => drawCard(card));
            break;
        case 'double_sequence':
            cardsInPlay.cards.forEach(card => drawCard(card));
            break;
        case 'single':
            drawCard(cardsInPlay.card);
            break;
        case 'none':
            break;
    }
}

function selectCard(card, playerIndex, cardSpan) {
    const playerTurn = receivedData.next_move.player;
    if (playerIndex === playerTurn) {
        const cardIndex = selectedCards.indexOf(card);
        if (cardIndex > -1) {
            selectedCards.splice(cardIndex, 1);
            cardSpan.classList.remove('selected');
        } else {
            selectedCards.push(card);
            cardSpan.classList.add('selected');
        }
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
    const ranks = cards.map(card => getRankScore(card.rank));
    // ranks.sort();
    for (let i = 0; i < ranks.length - 1; i++) {
        if (ranks[i] + 1 !== ranks[i + 1]) {
            return false;
        }
    }
    return true;
}

function isDoubleSequence(cards) {
    if (cards.length < 6) return false;
    if (cards.length % 2 !== 0) return false;
    const ranks = cards.map(card => getRankScore(card.rank));
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

function checkCardsInPlayIsSequence() {
    if (receivedData.cards_in_play === 'none') return true;
    return receivedData.cards_in_play.type === 'sequence' && receivedData.cards_in_play.cards.length === selectedCards.length;
}

function checkCardsInPlayIsDoubleSequence() {
    if (receivedData.cards_in_play === 'none') return true;
    return receivedData.cards_in_play.type === 'double_sequence' && receivedData.cards_in_play.cards.length === selectedCards.length;
}

function checkCardsInPlayIsThreeOfKind() {
    if (receivedData.cards_in_play === 'none') return true;
    if (receivedData.cards_in_play.type === 'three_of_kind') return true;
    return false;
}

function checkCardsInPlayIsFourOfKind() {
    if (receivedData.cards_in_play === 'none') return true;
    if (receivedData.cards_in_play.type === 'four_of_kind') return true;
    return false;
}

function checkCardsInPlayIsPair() {
    if (receivedData.cards_in_play === 'none') return true;
    if (receivedData.cards_in_play.type === 'pair') return true;
    return false;
}

function checkCardsInPlayIsSingle() {
    if (receivedData.cards_in_play === 'none') return true;
    if (receivedData.cards_in_play.type === 'single') return true;
    return false;
}

function checkCardsInPlayIsA2() {
    if (receivedData.cards_in_play === 'none') return true;
    if (receivedData.cards_in_play.type === 'single' && receivedData.cards_in_play.card.rank === 2) return true;
    return false;
}

function checkCardsInPlayIsAPairOf2() {
    if (receivedData.cards_in_play === 'none') return true;
    if (receivedData.cards_in_play.type === 'pair' && receivedData.cards_in_play.cards[0].rank === 2) return true;
    return false;   
}

function sendDataBack() {
    if (receivedData) {
        const selectedCardsData = selectedCards.map(card => unicodeToCard(card));
        if (isSingle(selectedCardsData) && checkCardsInPlayIsSingle()) {
            receivedData.next_move.action = { place: { card: selectedCardsData[0], type: 'single' } };
        }
        else if (isPair(selectedCardsData) && checkCardsInPlayIsPair()) {
            receivedData.next_move.action = { place: { cards: selectedCardsData, type: 'pair' } };
        }
        else if (isThreeOfKind(selectedCardsData) && checkCardsInPlayIsThreeOfKind()) {
            receivedData.next_move.action = { place: { cards: selectedCardsData, type: 'three_of_kind' } };
        }
        else if (isFourOfKind(selectedCardsData) && (checkCardsInPlayIsFourOfKind() || checkCardsInPlayIsA2())) {
            receivedData.next_move.action = { place: { cards: selectedCardsData, type: 'four_of_kind' } };
        }
        else if (isSequence(selectedCardsData) && checkCardsInPlayIsSequence()) {
            sortedCards = selectedCardsData.sort((a, b) => getRankScore(a.rank) - getRankScore(b.rank));
            receivedData.next_move.action = { place: { cards: sortedCards, type: 'sequence' } };
        }
        // We check doubleSequece can beat doubleDoubleSequece and doubleSequence can also beat a 2 and a pair of 2
        else if (isDoubleSequence(selectedCardsData) && (checkCardsInPlayIsDoubleSequence() || checkCardsInPlayIsA2() || checkCardsInPlayIsAPairOf2())) {
            sortedCards = selectedCardsData.sort((a, b) => getRankScore(a.rank) - getRankScore(b.rank));
            receivedData.next_move.action = { place: { cards: sortedCards, type: 'double_sequence' } };
        }
        socket.send(JSON.stringify(receivedData));
        selectedCards = [];
    }
}

function getRankScore(rank) {
    if (rank === 'j') return 11;
    if (rank === 'q') return 12;
    if (rank === 'k') return 13;
    if (rank === 'a') return 14;
    return rank;
}

function sendPass() {
    if (receivedData) {
        receivedData.next_move.action = 'pass';
        socket.send(JSON.stringify(receivedData));
    }
}

function sendNewGame() {
    socket.send('init_game');
    selectedCards = [];
}

function sendLastGame() {
    socket.send('last_game');
    selectedCards = [];
}

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
        '🂲': {rank: 2, suit: 'hearts'}, '🂳': {rank: 3, suit: 'hearts'}, '🂴': {rank: 4, suit: 'hearts'},
        '🂵': {rank: 5, suit: 'hearts'}, '🂶': {rank: 6, suit: 'hearts'}, '🂷': {rank: 7, suit: 'hearts'},
        '🂸': {rank: 8, suit: 'hearts'}, '🂹': {rank: 9, suit: 'hearts'}, '🂺': {rank: 10, suit: 'hearts'},
        '🂻': {rank: 'j', suit: 'hearts'}, '🂽': {rank: 'q', suit: 'hearts'}, '🂾': {rank: 'k', suit: 'hearts'},
        '🂱': {rank: 'a', suit: 'hearts'}, '🃂': {rank: 2, suit: 'diamonds'}, '🃃': {rank: 3, suit: 'diamonds'},
        '🃄': {rank: 4, suit: 'diamonds'}, '🃅': {rank: 5, suit: 'diamonds'}, '🃆': {rank: 6, suit: 'diamonds'},
        '🃇': {rank: 7, suit: 'diamonds'}, '🃈': {rank: 8, suit: 'diamonds'}, '🃉': {rank: 9, suit: 'diamonds'},
        '🃊': {rank: 10, suit: 'diamonds'}, '🃋': {rank: 'j', suit: 'diamonds'}, '🃍': {rank: 'q', suit: 'diamonds'},
        '🃎': {rank: 'k', suit: 'diamonds'}, '🃁': {rank: 'a', suit: 'diamonds'}, '🃒': {rank: 2, suit: 'clubs'},
        '🃓': {rank: 3, suit: 'clubs'}, '🃔': {rank: 4, suit: 'clubs'}, '🃕': {rank: 5, suit: 'clubs'},
        '🃖': {rank: 6, suit: 'clubs'}, '🃗': {rank: 7, suit: 'clubs'}, '🃘': {rank: 8, suit: 'clubs'},
        '🃙': {rank: 9, suit: 'clubs'}, '🃚': {rank: 10, suit: 'clubs'}, '🃛': {rank: 'j', suit: 'clubs'},
        '🃝': {rank: 'q', suit: 'clubs'}, '🃞': {rank: 'k', suit: 'clubs'}, '🃑': {rank: 'a', suit: 'clubs'},
        '🂢': {rank: 2, suit: 'spades'}, '🂣': {rank: 3, suit: 'spades'}, '🂤': {rank: 4, suit: 'spades'},
        '🂥': {rank: 5, suit: 'spades'}, '🂦': {rank: 6, suit: 'spades'}, '🂧': {rank: 7, suit: 'spades'},
        '🂨': {rank: 8, suit: 'spades'}, '🂩': {rank: 9, suit: 'spades'}, '🂪': {rank: 10, suit: 'spades'},
        '🂫': {rank: 'j', suit: 'spades'}, '🂭': {rank: 'q', suit: 'spades'}, '🂮': {rank: 'k', suit: 'spades'},
        '🂡': {rank: 'a', suit: 'spades'}
    };
    return unicodeMap[unicode];
}

socket.onopen = () => {
    console.log('Connected to WebSocket server');
    socket.send('init_game'); // Send a message when connected
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
    container.innerHTML = ''; // Clear previous content
    const card = cardToUnicode(cardsInPlay);
    const cardSpan = document.createElement('span');
    cardSpan.className = 'card';
    cardSpan.textContent = card;
    if (hearts.has(card) || diamonds.has(card)) {
        cardSpan.style.color = 'red';
    }
    container.appendChild(cardSpan);
}

function visualizeCardsInPlay(cardsInPlay) {
    if (cardsInPlay === 'none'){
        const container = document.getElementById('cards-in-play-container');
        container.innerHTML = ''; // Clear previous content
        return
    }
    switch(cardsInPlay.type) {
        case 'pair':
            // Do the drawing for the pair
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

function sendDataBack() {
    if (receivedData) {
        const selectedCardsData = selectedCards.map(card => unicodeToCard(card));
        const dataToSend = {
            // ...receivedData,
            // selectedCards: selectedCardsData
        };
        // "action": {"place": {"card": {"rank":3,"suit":"spades"},"type":"single"}}
        if (selectedCardsData.length === 1) {
            receivedData.next_move.action =  { place: { card:  selectedCardsData[0], type: 'single' }};
        }
        // if (selectedCardsData.length > 0) {
        //     receivedData.next_move.action =  { place: { single: { selectedCardsData[0]} }};
        //     // receivedData.next_move.action =  { place: { single: {selectedCardsData} }};
        // }

        socket.send(JSON.stringify(receivedData));
        selectedCards = [];
    }
}

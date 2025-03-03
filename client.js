const socket = new WebSocket('ws://localhost:8080'); // Replace with your WebSocket server URL

socket.onopen = () => {
    console.log('Connected to WebSocket server');
    socket.send('say_hi("Hello Server")'); // Send a message when connected
};

socket.onmessage = (event) => {
    console.log('Received:', event.data); // Log incoming messages
};

socket.onerror = (error) => {
    console.error('WebSocket Error:', error);
};

socket.onclose = () => {
    console.log('WebSocket connection closed');
};

// var API_URL = "http://localhost:3000";
var API_URL = "http://localhost:8080";

// Pusher.logToConsole = true;
Pusher.logToConsole = false;
// var pusher = new Pusher('dd888f5a7a55d7eb5059', {
var pusher = new Pusher('4ef0f34a4e955e13b6a1', {
     cluster: 'us2',
     forceTLS: true
});


var app = Elm.Main.init({
    node: document.querySelector("#app"),
    flags: API_URL
});


var newTurn = turnData => {
    console.log('NEW TURN EVENT FIRED');
    app.ports.newTurn.send(turnData);
};


app.ports.bindToGame.subscribe(function(gameId) {
    console.log('bound to game');
    var channel = pusher.subscribe(`game-${gameId}`);
    channel.bind('turn-complete', data => {
        alert('turn complete');
    });
    channel.bind('turn-start', newTurn);
});

const seconds = 1000;

// DEBUGGING CODE.
// simulate a game start event
// setTimeout(() => {
//     console.log('game starting. it is player 1 turn');
//     newTurn({playerNumber: 1});
// }, 20 * seconds);


// setTimeout(() => {
//     console.log('new turn. player 2 turn');
//     newTurn({playerNumber: 2});
// }, 30 * seconds);

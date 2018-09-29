// var API_URL = "http://localhost:3000";
var API_URL = "http://localhost:8080";

// Pusher.logToConsole = true;
Pusher.logToConsole = false;
// var pusher = new Pusher('dd888f5a7a55d7eb5059', {
var pusher = new Pusher('4ef0f34a4e955e13b6a1', {
     cluster: 'us2',
     forceTLS: true
});

const spritesheet = require('./assets/spritemap.svg');
const { Elm } = require('./Main.elm');

var app = Elm.Main.init({
    node: document.querySelector("#app"),
    flags: { apiUrl: API_URL, svgPath: spritesheet }
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
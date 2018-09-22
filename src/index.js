var API_URL = "http://localhost:3000";

Pusher.logToConsole = true;
var pusher = new Pusher('dd888f5a7a55d7eb5059', {
     cluster: 'us2',
     forceTLS: true
});


var app = Elm.Main.init({
    node: document.querySelector("#app"),
    flags: API_URL
});


app.ports.bindToGame.subscribe(function(gameId) {
    console.log('bound to game');
    var channel = pusher.subscribe(`game-${gameId}`);
    channel.bind('turn-complete', data => {
        alert('turn complete');
    });
    channel.bind('turn-start', turnData => {
        app.ports.newTurn.send(turnData);
    });
});

const seconds = 1000;

// DEBUGGING CODE.
// simulate a game start event
setTimeout(() => {
    console.log('game starting. starting turn 0');
    app.ports.newTurn.send(
        { playerNumber: 1 }
    );
}, 12 * seconds);

setTimeout(() => {
    console.log('game starting. starting turn 0');
    app.ports.newTurn.send(
        { playerNumber: 2 }
    );
}, 20 * seconds);
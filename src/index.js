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


var newTurn = turnData => {
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
setTimeout(() => {
    console.log('game starting. it is player 1 turn');
    newTurn({playerNumber: 1});
}, 20 * seconds);


// setTimeout(() => {
//     console.log('new turn. player 2 turn');
//     newTurn({playerNumber: 2});
// }, 30 * seconds);
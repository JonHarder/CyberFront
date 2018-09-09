
// Pusher.logToConsole = true;
var pusher = new Pusher('dd888f5a7a55d7eb5059', {
     cluster: 'us2',
     forceTLS: true
});


var app = Elm.Main.init(document.querySelector("#app"));


app.ports.bindToGame.subscribe(function(gameId) {
    console.log('bound to game');
    var channel = pusher.subscribe(`game-${gameId}`);
    channel.bind('turn-complete', data => {
        alert('turn complete');
    });
    channel.bind('game-start', data => {
         var mapId = data.mapId;
         app.ports.gameStarted.send(mapId);
    });
});


// DEBUGGING CODE.
// simulate a game start event
setTimeout(() => {
    console.log('game starting. using map 4');
    app.ports.gameStarted.send('4');
}, 3000);
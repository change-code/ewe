var socket;
var term;

function on_open() {
    term = new Terminal({cols: 80, rows: 24, screenKeys: true});

    term.open(document.body);

    term.on('data', function(data) { socket.send(data); });
    term.on('title', function(title){ document.title = title; });

    socket.onmessage =
        function (message) {
            console.log(message);
            term.write(message.data);
        };

    socket.onclose =
        function () {
            term.destroy();
        };
}

function on_load() {
    socket = new WebSocket("ws://127.0.0.1:9999/");
    socket.onopen = on_open;
}

window.addEventListener('load', on_load);
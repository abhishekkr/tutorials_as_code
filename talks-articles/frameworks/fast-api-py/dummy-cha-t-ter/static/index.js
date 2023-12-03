/*
 * A simple websocket logic that connects, sends message & process received events.
 * This doesn't even handle reconnection or any kind of failures.
 * As they are not of consideration for this example.
 */

var ws = new WebSocket(`ws://localhost:8000/chat`);

ws.onopen = function (event) {
  console.log("Connected:", event.currentTarget.url);
};

ws.onmessage = function (event) {
    var messages = document.getElementById('messages');
    var br = document.createElement('br');
    var message = document.createElement('span');
    var content = document.createTextNode(event.data);
    message.appendChild(content);
    messages.appendChild(message);
    messages.appendChild(br);
};

function submitMsg(event) {
    var input = document.getElementById("msgBox")
    ws.send(input.value)
    input.value = ''
    event.preventDefault()
    console.log("Send Mesage");
}

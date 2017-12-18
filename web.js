var WebSocketServer = require("ws").Server;
var wss = new WebSocketServer({ port: 3002});
var R = require('./server.js');
// 연결이 수립되면 클라이언트에 메시지를 전송하고 클라이언트로부터의 메시지를 수신한다
wss.on("connection", function (ws) {
    var GPS;

  
  ws.on("message", function(message) {
      console.log("Received: %s", message);
      GPS = message;
      R.Rmodule(GPS);
      var result = Math.floor(Math.random() * 4) + 1;
      ws.send(result);
  });
});


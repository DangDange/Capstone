var WebSocketServer = require("ws").Server;
var wss = new WebSocketServer({ port: 3002});
var R = require('./server.js');
// ������ �����Ǹ� Ŭ���̾�Ʈ�� �޽����� �����ϰ� Ŭ���̾�Ʈ�κ����� �޽����� �����Ѵ�
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


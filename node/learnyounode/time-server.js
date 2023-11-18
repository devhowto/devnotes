'use strict';

const net = require('node:net');

const log = console.log.bind(console);

const getParams = processArgv => processArgv.slice(2);

const [ port ] = getParams(process.argv);

const padWithZero = (n) => {
  const s = String(n);
  if (s.length === 1) return `0${s}`;
  return s;
}

const server = net.createServer(function (socket) {
  const date = new Date();
  const year = date.getFullYear();
  const month = padWithZero(date.getMonth() + 1);
  const day = padWithZero(date.getDate());
  const hour = padWithZero(date.getHours());
  const minute = padWithZero(date.getMinutes());

  socket.end(`${year}-${month}-${day} ${hour}:${minute}`);
});

server.listen(port);

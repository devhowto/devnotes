import fs from 'node:fs';

//
// $ npm run file ./node/stream1.js <<<$(curl --silent -X GET
// https://jsonplaceholder.typicode.com/todos/1)
//

var log = console.log.bind(console);

var input = '';

process.stdin.resume();
process.stdin.setEncoding('utf-8');

var ws = fs.createWriteStream('./node/output.dat');

process.stdin.on('data', function onData(data) {
  input += data;
  log('===', data);
  ws.write(data);
});

process.stdin.on('end', function onEnd() {
  log('END');
  ws.write('END');
  ws.end();
  // log(input.split('\n'));
});


// [10, 20, 30].forEach(num => {
//   ws.write(num + '\n');
// });


// function add(x, y) {
//   return x + y;
// }
//
// var res = add(1, 1);

// ws.write(res + '\n');

log('START');

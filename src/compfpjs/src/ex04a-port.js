//
// Run from compfjs directory else the json will will not be found.
//
//   $ node ./src/ex04a-port.js
//

const { readFileSync } = require('node:fs');
const { log } = require('./lib');

function getPort() {
  try {
    const str = readFileSync('./src/ex04a.cfg.json');
    const cfg = JSON.parse(str);
    return cfg.port;
  } catch (e) {
    log('getPort():\n', e);
    return 3000;
  }
}

log(getPort());
//=> 8888

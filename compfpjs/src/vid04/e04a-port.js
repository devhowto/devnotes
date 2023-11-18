const { readFileSync } = require('node:fs');
const { log } = require('../lib');

function getPort() {
  try {
    const str = readFileSync(`${__dirname}/e04a.cfg.json`);
    const cfg = JSON.parse(str);
    return cfg.port;
  } catch (e) {
    log('getPort():\n', e);
    return 3000;
  }
}

log(getPort());
//=> 8888

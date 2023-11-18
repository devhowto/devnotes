const { readFile, writeFile } = require('node:fs');
const { log } = require('../lib');


const app = function app() {
  const pathOrig = __dirname + '/config-ok.json';
  const pathNew = __dirname + '/config-new.json';
  
  return readFile(pathOrig, 'utf-8', (err, data) => {
    if (err) throw err;

    const newContents = data.replace(/8/g, '6');
    log(newContents);

    writeFile(pathNew, newContents, (err, _) => {
      if (err) throw err;

      log(`New config at ‘${pathNew}’.`);
    });
  });
};

app();
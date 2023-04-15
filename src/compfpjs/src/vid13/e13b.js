const { task } = require('folktale/concurrency/task');
const { readFile, writeFile } = require('node:fs');
const { log } = require('../lib');

/** 
 * @sig String String -> Task
 */
const read = (filename, encoding) =>
  task(({ reject, resolve }) =>
    readFile(filename, encoding, (err, data) =>
      err ? rej(err) : resolve(data)));

/** 
 * @sig String String -> Task
 */
const write = (filename, data) =>
  task(({ reject, resolve }) =>
    writeFile(filename, data, (err, ok) =>
      err ? rej(err) : resolve(ok)));

const app = function app() {
  const pathOrig = __dirname + '/config-ok.json';
  const pathNew = __dirname + '/config-new.json';
  
  return read(pathOrig, 'utf-8')
    .map(data => data.replace(/8/g, '6'))
    .chain(dataNew => write(pathNew, dataNew));
}

app()
  .run()
  .listen({
    onRejected: e => log('err', e),
    onResolved: () => log('ok'),
  });
//
// Run from compfjs directory else the json will will not be found.
//
//   $ node ./src/ex04b-port.js
//

import {
  log,
} from './lib.js';

import { readFileSync } from 'node:fs';

function Right(v) {
  return {
    map: f => Right(f(v)),
    fold: (f, g) => g(v),
    inspect: () => `Right(${v})`,
  };
}

function Left(v) {
  return {
    map: f => Left(v),
    fold: (f, g) => f(v),
    inspect: () => `Left(${v})`,
  };
}

/**
 * @sig function -> Either
 */
function tryCatch(f) {
  try {
    return Right(f());
  } catch (e) {
    return Left(e);
  }
}

function getPort() {
  return tryCatch(() => readFileSync('./src/ex04b.cfgjson'))
    .map(c => JSON.parse)
    .fold(e => 3000, c => c.port);
}

log(getPort());
//=> 8888

/*

Our tryCatch() returns either a Left or a Right. Remember that Left does
not run map at all. If readFileSync() fails to read the file, tryCatch()
returns a Left, which does not run map(), thus not trying to parse JSON
which is not there.

What happens is that then fold
*/

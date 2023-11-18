const { readFileSync } = require('node:fs');
const { log } = require('../lib');

function Right(v) {
  return {
    chain: f => f(v),
    map: f => Right(f(v)),
    fold: (f, g) => g(v),
    inspect: () => `Right(${v})`,
  };
}

function Left(v) {
  return {
    chain: f => Left(v),
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
  return tryCatch(() => readFileSync(`${__dirname}/e04c.cfg.json`))
    .chain(c => tryCatch(() => JSON.parse(c)))
    .fold(e => 3000, c => c.port);
}

log(getPort());
//=> 8888

/*

Because JSON.parse() will throw if the json content is invalid, we need
to tryCatch() it, but it would nest another Left or Right, so we would
end up with a Box inside a Box.

The solution is to create another method called chain which is like map
except it does not nest in another Box.

In Right, chain applies f without Boxing the value.

In Left, chain ignores f (does not apply it to the value) and just
returns the original, Boxed value.

When do we use chain()? When we know we already have an Either (Box,
Left or Right), we then use chain() to avoid having nested Boxes.

fold() is about the idea of removing a value from its context (taking
it out of the Box (Left or Right).

chain() expects you to run a function and return “another one” ???

fold() and chain() may have the same definition, but their conceptual
idea of when to use each is different.
*/

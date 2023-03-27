import {
  log,
  idty,
  toUpper,
} from './lib.js';


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
 * @sig String -> Either
 */
function findColor(name) {
  const color = {
    red: '#ff4444',
    blue: '#3b5998',
    yellow: '#fff68f',
  }[name];

  return color ? Right(color) : Left(null);
}

log(
  findColor('red')
  .map(s => s.slice(1))
  .fold(e => 'no color', toUpper)
);
//=> #FF4444

log(
  findColor('pink')
  .map(s => s.slice(1))
  .fold(e => 'no color', toUpper)
);
//=> no color

/*

findColor() returns either a Left or a Right, and I must map over
whatever it returns so we correctly handle both possibilities and we
don't get unexpected errors at runtime.

But findColor() has the temporary color assignment which is what we
tried to avoid since the initial examples. We want some data flow, not
step by step assignment of intermediate values 😭.

*/

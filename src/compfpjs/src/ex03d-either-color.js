const {
  isNil,
  log,
  toUpper,
} = require('./lib');


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
 * @sig a -> Either a
 */
function fromNullable(v) {
  return isNil(v) ? Left(null) : Right(v);
}

/**
 * @sig String -> Either
 */
function findColor(name) {
  return fromNullable({
    red: '#ff4444',
    blue: '#3b5998',
    yellow: '#fff68f',
  }[name]);
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

fromNullable() was introduced to improve findColor() and make it handle
a flow of data instead of intermediate steps assignments.

*/

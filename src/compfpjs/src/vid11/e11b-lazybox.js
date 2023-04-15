const {
  log,
  trim,
  toInt,
  add1,
  chr,
} = require('../lib');

const Box = x => ({
  map: f => Box(f(x)),
  fold: f => f(x),
  str: () => `Box(${x})`,
});

/** 
 * Takes a function now instead of a data value.
 */
const LazyBox = g => ({
  fold: f => f(g()),
  map: f => LazyBox(() => f(g())), 
});

const res = LazyBox(() => '   64 ')
  .map(trim)
  .map(toInt)
  .map(add1)
  .fold(chr);

log(res);
//=> 64
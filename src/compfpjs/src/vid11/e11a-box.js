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

const res = Box('   64 ')
  .map(trim)
  .map(toInt)
  .map(add1)
  .fold(chr);

log(res);
//=> 64
const { log } = require('../lib');
const { Sum } = require('../libt');

const Pair = (x, y) => ({
  x,
  y,
  concat: ({ x: x1, y: y1 }) =>
    Pair(x.concat(x1), y.concat(y1)),
  str: () => `Pair({ x: ${x.str()}, y: ${y.str()} })`,
});

log(
  Pair(Sum(1), Sum(2))
  .concat(Pair(Sum(1), Sum(2)))
  .str()
);
//=> Pair({ x: Sum(2), y: Sum(4) })

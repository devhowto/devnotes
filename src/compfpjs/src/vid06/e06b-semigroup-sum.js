const { log } = require('../lib');

/**
 * Sum Semigroup type that knows how concat (add) numbers.
 *
 * @sig Number -> Sum
 */
const Sum = x => ({
  x,
  concat: ({ x: y }) => Sum(x + y),
  str: () => `Sum(${x})`,
});

const res = Sum(1).concat(Sum(2));

log(res);
//=> { val: 3, ... }

log(res.str());
//=> Sum(3)

log(
  Sum(-1)
  .concat(Sum(-2))
  .concat(Sum(-3))
  .concat(Sum(-100))
  .str()
);
//=> Sum(-106)

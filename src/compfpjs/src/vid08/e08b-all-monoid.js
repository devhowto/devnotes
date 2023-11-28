const { log } = require('../lib');

const All = x => ({
  x,
  concat: ({ x: y }) => All(x && y),
  str: () => `All(${x})`,
});

/**
 * The neutral/identity element for all is `true`.
 *
 * By endowing `All` with `empty`, we are promoting
 * it from a Semigroup to a Monoid.
 */
All.empty = () => All(true);

log(All.empty().str());
//=> All(true)

log(
  All.empty()
  .concat(All(true))
  .concat(All(false))
  .str()
);
//=> All(false)

log(
  All.empty()
  .concat(All(true))
  .concat(All(true))
  .str()
);
//=> All(true)
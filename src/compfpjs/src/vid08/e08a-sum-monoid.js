const { log } = require('../lib');

const Sum = x => ({
  x,
  concat: ({ x: y }) => Sum(x + y),
  str: () => `Sum(${x})`,
});

/**
 * Zero is the identity property (neutral element) for addition.
 */
Sum.empty = () => Sum(0);

log(Sum.empty().str());
//=> Sum(0)

log(Sum.empty().concat(Sum(42)).str());
//=> Sum(42)


/**
 * Initial, default value is 0 as it is the neutral/identity for `+`.
 */
function sum(xs) {
  return xs.reduce((acc, x) => acc + x, 0);
}

/**
 * Initial, default value is true as it is the
 * neutral/identity for `&&`.
 */
function all(xs) {
  xs.reduce((acc, x) => acc && x, true);
}

/**
 * No initial, default value. Blows up with empty array.
 * This function is not safe.
 */
function first(xs) {
  xs.reduce((acc, _) => acc);
}

log(first([]));
//=> TypeError: Reduce of empty array with no initial value.

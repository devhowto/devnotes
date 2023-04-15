const { log } = require('../lib');

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
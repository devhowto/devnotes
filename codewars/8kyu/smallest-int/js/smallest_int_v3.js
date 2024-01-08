const log = console.log.bind(console);

import {
  head,
  isEmpty,
  tail,
} from 'ramda';

/**
 * Finds the smallest number (integer) in the array.
 *
 * Uses the recursive “go function” pattern.
 *
 * @param {Array<number>} ints
 * @returns {number}
 */
function smallestInt(ints) {
  return (function go(xs, min) {
    if (isEmpty(xs)) return min;

    return head(xs) < min
      ? go(tail(xs), head(xs))
      : go(tail(xs), min);
  })(ints, Infinity);
}

log(
  smallestInt([5, 1, 3]),
  smallestInt([-7, 1, -9]),
  smallestInt([42, 42, 42]),
);

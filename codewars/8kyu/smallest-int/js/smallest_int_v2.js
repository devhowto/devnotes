const log = console.log.bind(console);

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
    if (xs.length === 0) return min;

    return xs[0] < min
      ? go(xs.slice(1), xs[0])
      : go(xs.slice(1), min);
  })(ints, Infinity);
}

log(
  smallestInt([5, 1, 3]),
  smallestInt([-7, 1, -9]),
  smallestInt([42, 42, 42]),
);

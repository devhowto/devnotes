//
// tags: min int array
//

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
  //
  // Apply so we don't need to turn ints into each individual elem.
  // (compare to v4)
  //
  return Math.min.apply(ints)
}

log(
  smallestInt([5, 1, 3]),
  smallestInt([-7, 1, -9]),
  smallestInt([42, 42, 42]),
);

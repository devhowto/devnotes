const log = console.log.bind(console);

/**
 * Finds the smallest number (integer) in the array.
 *
 * ASSUME: The array is not empty.
 *
 * @param {Array<number>} ints
 * @returns {number}
 */
function smallestInt(ints) {
  let minSoFar = ints[0];

  for (let i = 1; i < ints.length; ++i) {
    if (ints[i] < minSoFar)
      minSoFar = ints[i];
  }

  return minSoFar;
}

log(
  smallestInt([5, 1, 3]),
  smallestInt([-7, 1, -9]),
  smallestInt([42, 42, 42]),
);

/*

Could also do minSoFar = Infinity then start i as 0.

*/

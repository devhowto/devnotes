/**
 * Finds the indices of two distinct indices in `flavors` that when
 * added together are equal to `money`.
 *
 * • T.C: O(n²).
 *
 * NOTE: The challenge requires the output indices to start at 1 😅.
 *
 * ASSUME:
 *
 * • There is always a single, correct solution.
 * • The input array is NOT sorted.
 * • The input array could contain duplicate values.
 *
 * @sig Int -> [Int] -> [Int, Int]
 * @param {number} money
 * @param {Array<number>} flavorPrices
 * @returns {[number, number]}
 */
function iceCreamParlor(money, flavorPrices) {
  var indices = [-1, -1],
      len = flavorPrices.length,
      i,
      j;

  outerLoop:
  for (i = 0; i < len; ++i) {
    for (j = i + 1; j < len; ++j) {
      if (flavorPrices[i] + flavorPrices[j] === money) {
        indices[0] = i + 1;
        indices[1] = j + 1;
        break outerLoop;
      }
    }
  }

  return indices;
}

/*
[
  [3, 2, 5, 7, 1], // flavor prices for trip 1
  [1, 3, 4, 5, 6], // flavor prices for trip 2
].forEach(prices => {
  console.log(iceCreamParlor(6, prices));
});
*/

export { iceCreamParlor };

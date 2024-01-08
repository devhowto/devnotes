//
// tags: javascript codewars 6kyu math algorithm multiple
//

const log = console.log.bind(console);

/**
 * Checks whether `y` is a multiple of `x`.
 *
 * @param {number} x
 * @param {number} y
 * @returns {boolean}
 */
function isMultOf(x, y) {
  return y % x === 0;
}

function sumMults3or5(n) {
  if (n < 0) return 0;

  let total = 0;

  for (let i = 1; i < n; ++i) {
    if (isMultOf(3, i) && isMultOf(5, i))
      total += i;
    else if (isMultOf(3, i))
      total += i;
    else if (isMultOf(5, i))
      total += i;
  }

  return total;
}

log(sumMults3or5(10));

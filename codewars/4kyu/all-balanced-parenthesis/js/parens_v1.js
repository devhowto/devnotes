const l = console.log.bind(console);

function zero(v) {
  return v === 0;
}

/**
 * Generates all possible combinations of `n` balanced parenthesis.
 *
 * @param {number} n The number of balanced parenthesis.
 * @return {String}
 */
function parens(n) {
  var res = [];

  (function f(s, l, r) {
    if (l > r) return;

    if (zero(l) && zero(r)) return res.push(s);

    if (l > 0) f(`${s}(`, l - 1, r);

    if (r > 0) f(`${s})`, l, r - 1);
  })('', n, n);

  return res;
}

l(parens(2));

export { parens };

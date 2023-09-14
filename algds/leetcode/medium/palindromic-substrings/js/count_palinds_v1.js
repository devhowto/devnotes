const log = console.log.bind(console);

//
// Generate all substrings and check which ones are palindromes.
//

/**
 * Returns `xs` without the last element.
 *
 * @param {string|unknown[]} xs
 * @returns {string|unknown[]}
 */
function init(xs) {
  return xs.slice(0, xs.length - 1);
}

/**
 * Returns `xs` without the first element.
 *
 * @param {string|unknown[]} xs
 * @returns {string|unknown[]}
 */
function tail(xs) {
  return xs.slice(1);
}

/**
 * Returns `xs` without the first and last element.
 *
 * @param {string|unknown[]} xs
 * @returns {string|unknown[]}
 */
function dropEdges(xs) {
  return xs.slice(1, xs.length - 1);
}

/**
 * Checks if first and last elements of `xs` are equal.
 *
 * @param {string|unknown[]} xs
 * @returns {boolean}
 */
function sameEdges(xs) {
  return xs[0] === xs[xs.length - 1];
}

function isPalind(s) {
  if (s.length <= 1) return true;
  return sameEdges(s) ? isPalind(dropEdges(s)) : false;
}


function countPalinds(xs) {
  let count = 0;
  const tbl = {};

  (function run(s, i) {
    log();
    log('run', { s, tbl });

    const len = s.length;

    if (len <= 1) {
      log('if len <= len then palind', s, i);
      if (tbl[i]) return;
      ++count;
      tbl[i] = 1;
      return;
    }

    if (isPalind(s)) {
      log(`“${s}” is palind`);
      ++count;
    }

    run(init(s, ++i)); // a
    run(tail(s, ++i));
  }(xs, 0));

  return count;
}

log(countPalinds("aba"));

// log(countPalinds("abba"));

// log(palind(""));
// log(palind("b"));
// log(palind("aba"));
// log(palind("abab"));
// log(palind("racecar"));

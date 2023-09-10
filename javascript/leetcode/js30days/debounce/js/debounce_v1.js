//
// tags: leetcode medium debounce javascript function
//

const log = console.log.bind(console);

/**
 * Returns a debounced version of a function.
 *
 * @param {Function} fn The function to debounce.
 * @param {number} t The number of milliseconds for the time window.
 * @returns {Function}
 */
function debounce(fn, t) {
  let timeoutId;

  return function debounced(...args) {
    if (timeoutId) clearTimeout(timeoutId);

    timeoutId = setTimeout(function debounced () {
      fn(...args);
    }, t);
  };
}

const log1 = debounce(log, 50);

setTimeout(() => log1('#1'), 50);
setTimeout(() => log1('#2'), 49);
setTimeout(() => log1('#3'), 49);

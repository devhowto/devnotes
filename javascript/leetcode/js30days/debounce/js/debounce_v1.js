//
// tags: javascript leetcode medium debounce function
//

import { performance as perf } from 'node:perf_hooks';

var log = console.log.bind(console);

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

var log1 = debounce(log, 50);

setTimeout(() => log1('#1'), 30);
setTimeout(() => log1('#2'), 44);
setTimeout(() => log1('#3'), 13);
// setTimeout(() => log1('#4'), 200);

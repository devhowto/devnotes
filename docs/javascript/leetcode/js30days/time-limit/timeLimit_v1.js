//
// tags: javascript leetcode medium promise resolve reject time limit timeout
//

const log = console.log.bind(console);
const err = console.error.bind(console);

/**
 * An async operation that returns its input after 50 milliseconds.
 *
 * @params {...unknown[]}
 */
async function request(...args) {
  return new Promise((res) => {
    setTimeout(function timedOut() {
      res(...args);
    }, 50);
  });
}

/**
 * Waits for `t` milliseconds before resolving.
 *
 * @param {number} t Number of milliseconds to wait before resolving.
 * @returns {Promise<string>}
 */
async function guard(t) {
  return new Promise((res) => {
    setTimeout(function timedOut() {
      res("Time Limit Exceeded");
    }, t);
  });
}

/**
 * Time-limits execution of `fn` by `t` milliseconds.
 *
 * If `fn` resolves within `t` milliseconds, resolves with the results
 * of `fn`; else rejects with the message “Time Limit Exceeded”.
 *
 * @param {Function} fn
 * @param {number} t
 * @returns {Promise<string | unknown>}
 */
function timeLimit(fn, t) {
  return async function timed(...args) {
    const winner = await Promise.race([fn(...args), guard(t)]);

    if (winner === "Time Limit Exceeded")
      return Promise.reject(winner);

    return Promise.resolve(winner)
  }
}

const lim = timeLimit(request, 60);

lim({ id: 1, name: 'Yoda' })
  .then(log)
  .catch(err);

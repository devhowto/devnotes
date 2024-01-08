'use strict';

const log = console.log.bind(console);

/**
 * Recursive solution using the run/go helper function approach.
 *
 * - T.C: O(n).
 * - S.C: O(n).
 *
 * @param {string}
 * @returns {number}
 */
function validParens(s) {
  return (function go(lst, l, r) {
    if (lst.length === 0) return l === r;

    if (lst[0] === '(') ++l;
    if (lst[0] === ')') ++r;

    if (r > l) return false;

    return go(lst.slice(1), l, r);
  })([...s], 0, 0);
}

log(validParens(''));               //=> true
log(validParens('('));              //=> false
log(validParens('(('));             //=> false
log(validParens(')(()))'));         //=> false
log(validParens('()'));             //=> true
log(validParens('()()'));           //=> true
log(validParens('((()))'));         //=> true
log(validParens('())()('));         //=> false
log(validParens('(())((()())())')); //=> true

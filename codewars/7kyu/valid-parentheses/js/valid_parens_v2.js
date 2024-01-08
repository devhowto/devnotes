//
// tags: recursion

'use strict';

const log = console.log.bind(console);

/**
 * @param {string}
 * @returns {number}
 */
function validParens(s) {
  return (function go(l, c) {
    if (l.length === 0) return c === 0;

    if (l[0] === '(') ++c;
    if (l[0] === ')') --c;

    if (c < 0) return false;

    return go(l.slice(1), c);
  })([...s], 0);
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

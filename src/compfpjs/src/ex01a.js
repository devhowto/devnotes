import { log } from './lib.js';

/**
 * Takes a digit-like string and returns the next char based on
 * that digit.
 *
 * For example, 65 is ‘A’ in ASCII/UTF-8. 64 + 1 is 65, which is ‘A’,
 * and 122 is ‘z’, so, 121 + 1 is 122, which is ‘a’. See `man ascii`.
 */
function nextCharFromNumStr(s) {
  const trimmed = s.trim();
  const num = Number.parseInt(trimmed, 10);
  const nextNum = num + 1;

  return String.fromCharCode(nextNum);
}

log(nextCharFromNumStr(' 	  64 '));
//=> 'A'

log(nextCharFromNumStr('	121	  '));
//=> 'z'

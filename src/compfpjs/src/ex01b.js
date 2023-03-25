import { log } from './lib.js';

/**
 * Takes a digit-like string and returns the next char based on
 * that digit.
 *
 * For example, 65 is ‘A’ in ASCII/UTF-8. 64 + 1 is 65, which is ‘A’,
 * and 122 is ‘z’, so, 121 + 1 is 122, which is ‘a’. See `man ascii`.
 */
function nextCharFromNumStr(s) {
  return String.fromCharCode(Number.parseInt(s.trim()) + 1);
}

log(nextCharFromNumStr(' 	  64 '));
//=> 'A'

log(nextCharFromNumStr('	121	  '));
//=> 'z'

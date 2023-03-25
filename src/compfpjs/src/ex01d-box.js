import {
  add1,
  chr,
  log,
  toInt,
  trim,
} from "./lib.js";

/**
 * Takes a digit-like string and returns the next char based on
 * that digit.
 *
 * For example, 65 is ‘A’ in ASCII/UTF-8. 64 + 1 is 65, which is ‘A’,
 * and 122 is ‘z’, so, 121 + 1 is 122, which is ‘a’. See `man ascii`.
 */
function nextCharFromNumStr(str) {
  return [str]
    .map(trim)
    .map(toInt)
    .map(add1)
    .map(chr);
}

log(nextCharFromNumStr(" 	  64 "));
//=> ['A']

log(nextCharFromNumStr("	121	  "));
//=> ['z']

//
// Instead of assigning each intermediate step variable, we capture each
// assignment into a very minimal context. Each of the variables s, d,
// or n are not used outside those little function scopes.
//
// MAP IS COMPOSITION!!!
//

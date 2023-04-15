const {
  add1,
  chr,
  log,
  toInt,
  trim,
} = require('../lib');

/**
 * A value wrapper that allows applying functions to a value in a
 * composable style.
 *
 * We learn map as a sort of loop or iteration, but in reality, the map
 * concept has more to do with composition within a context, and in this
 * case, Box is our context.
 *
 * We'll use these “containery” types to capture different behaviors as
 * we compose.
 *
 * Our box is officially called the Identity Functor.
 */
function Box(x) {
  return {
    // Applies a function to x and puts it back into the box.
    map: f => Box(f(x)),

    // Returns the value without the wrapping box.
    // It is just like map, except it doesn't put it back in the box.
    fold: f => f(x),

    // Stringifies the box with its containing value.
    inspect: () => `Box(${x})`,
  };
}

/**
 * Takes a digit-like string and returns the next char based on
 * that digit.
 *
 * For example, 65 is ‘A’ in ASCII/UTF-8. 64 + 1 is 65, which is ‘A’,
 * and 122 is ‘z’, so, 121 + 1 is 122, which is ‘a’. See `man ascii`.
 */
function nextCharFromNumStr(str) {
  return Box(str)
    .map(trim)
    .map(toInt)
    .map(add1)
    .fold(chr);
}

log(nextCharFromNumStr(" 	  64 "));
//=> 'A'

log(nextCharFromNumStr("	121	  "));
//=> 'z'

//
// Instead of assigning each intermediate step variable, we capture each
// assignment into a very minimal context. Each of the variables s, d,
// or n are not used outside those little function scopes.
//
// MAP IS COMPOSITION!!!
//

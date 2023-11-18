const { log } = require('../lib');

/*

true && false is false

true && true is true

The && feels like a concatenation of sorts... it combines two things
into one.

*/

/**
 * All must be true.
 *
 * A Semigroup All type that knows how to concat (AND) booleans.
 *
 * @sig Bool -> All
 */
const All = x => ({
  x,
  concat: ({ x: y }) => All(x && y),
  str: () => `All(${x})`,
});

log(All(true).concat(All(false)).str());
//=> All(false)

log(All(true).concat(All(true)).str());
//=> All(true)

//
// We could also do a Some type where at least some (one)
// must be true.
//

const log = console.log.bind(console);

/*

true && false is false

true && true is true

The && feels like a concatenation of sorts... it combines two things
into one.

*/

/**
 * All must be true.
 *
 * @sig Bool -> All
 */
function All(val) {
  return {
    val,

    concat: function concat({ val: otherVal }) {
      // Note the use && instead of +.
      return All(val && otherVal);
    },

    inspect: function inspect() {
      return `All(${val})`;
    }
  };
}

log(All(true).concat(All(false)).inspect());
//=> All(false)

log(All(true).concat(All(true)).inspect());
//=> All(true)

//
// We could also do a Some type where at least some (one)
// must be true.
//

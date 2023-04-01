const { Map } = require('immutable-ext');

const log = console.log.bind(console);

/**
 * Only keep the first one.
 *
 * @sig Unknown -> First
 */
function First(val) {
  return {
    val,

    // Ignore other first and keep our initial one.
    concat: function concat(_otherFirst) {
      return First(val);
    },

    inspect: function inspect() {
      return `First(${val})`;
    },
  }
}

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

/**
 * @sig Number -> Sum
 */
function Sum(val) {
  return {
    val,

    concat: function concat({ val: otherVal }) {
      return Sum(val + otherVal);
    },

    inspect: function inspect() {
      return `Sum(${val})`;
    }
  };
}

const acct1 = Map({
  name: First('Ahsoka'),
  isPaid: All(true),
  points: Sum(10),
  friends: ['Aayla'],
});

const acct2 = Map({
  name: First('Ahsoka'),
  isPaid: All(false),
  points: Sum(2),
  friends: ['Leia'],
});

/*
Use First for name, so it knows how to combine the name
on the two accounts.

All on the booleans, and Sum on the points.

friends is an array, so it is already concatable.

But now how do we concat the accounts, since they are
objects (not yet concatable)?

Let's use Map from immutable.js.
*/

const res = acct1.concat(acct2);
log(res.toJS());

const log = console.log.bind(console);

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

/**
 * Zero is the identity property (neutral element) for addition.
 *
 * When we add `empty()` to `Sum`
 */
Sum.empty = () => Sum(0);

log(Sum.empty().inspect());
//=> Sum(0)

log(Sum.empty().concat(Sum(42)).inspect());
//=> Sum(42)

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

    str: function inspect() {
      return `All(${val})`;
    }
  };
}

/**
 * The neutral/identity element for all is `true`.
 */
All.empty = () => All(true);

/**
 * Only keep the first one.
 *
 * Not promoted to a Monoid because we cannot find a neutral/identity
 * element for this case.
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

log(First('hello').concat(First('world')).inspect());
//=> First('hello')

/**
 * Initial, default value is 0 as it is the neutral/identity for `+`.
 */
function sum(xs) {
  return xs.reduce((acc, x) => acc + x, 0);
}

/**
 * Initial, default value is true as it is the neutral/identity for
 * `&&`.
 */
function all(xs) {
  xs.reduce((acc, x) => acc && x, true);
}

/**
 * No initial, default value. Blows up with empty array.
 */
function first(xs) {
  xs.reduce((acc, _) => acc);
}

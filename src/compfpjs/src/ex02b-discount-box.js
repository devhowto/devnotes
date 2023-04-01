const { log } = require('./lib');

/**
 * A value wrapper that allows a composable style.
 */
function Box(v) {
  return {
    // Applies a function to `v` and puts it back into the box.
    map: f => Box(f(v)),

    // Returns the value without the wrapping box.
    // It is just like map, except it doesn't put it back in the box.
    fold: f => f(v),

    // Stringifies the box with its containing value.
    str: () => `Box(${v})`,
  };
}

/**
 * Converts a dollar money string to a float.
 *
 * Compare with the previous implementation of this function. This
 * approach allows unnesting the expressions.
 *
 * @example
 * moneyToFloat('$ 1.99');
 * //=> 1.99
 */
function moneyToFloat(str) {
  return Box(str)
    .map(s => s.replace(/\$/g, ''))
    .map(r => Number.parseFloat(r));
}

/**
 * Turns a percentage into its decimal float equivalent.
 *
 * NOTE: Multiplying by 0.01 is the same as dividing by 100, similar
 * to multiplying by 0.5 is actually the same as dividing by 2.
 *
 * @example
 * percentToFloat('43%');
 * //=> 0.43
 *
 * percentToFloat('100%');
 * //=> 1
 *
 * percentToFloat('1%');
 * //=> 0.01
 */
function percentToFloat(str) {
  return Box(str)
    .map(s => s.replace(/\%/g, ''))
    .map(r => Number.parseFloat(r))
    .map(n => n * 0.01);
}

/**
 * Applies a discount on a price.
 *
 * @example
 * applyDiscount('$ 80', '20%');
 * //=> 64
 */
function applyDiscount(price, discount) {
  return moneyToFloat(price)
    .fold(cost =>
      percentToFloat(discount)
      .fold(savings => cost - cost * savings));
}
//
// As both moneyToFloat and percentToFloat return a Box, we use fold
// instead of map here so we unwrap the value.
//
// cost is the unwrapped result of moneyToFloat(price)
// savings is the unwrapped result of percentToFloat(discount)
//
// cost is available inside the last arrow function due to closure.
////

log(applyDiscount('$5.00', '20%'));
//=> 4

//
// We used `Box` to unnest some expressions and eliminate the need for
// creating intermediate variables with assignments of intermediate
// steps of computation.
//
// We can start the box already by doing some initial computation:
//
//   function f(s) {
//     Box(g(s))
//       .map(...)
//       .fold(...)
//   }
//
// That is, we don't need to start the Box with the vanilla value we
// receive on the function param. Here, we start Box with applying g
// to s, not the vanilla s.
//
// Sometimes a function will `fold` to unwrap the value and sometimes
// not.
//
//
// Box alone doesn't do much. It basically captures something in a
// context. We can keep mapping, and folding, and composing in different
// ways around it.
//
// As we'll see, there are stronger things in box. They will give us
// behaviors associated with composition and new ways to compose. This
// is good practice to work on something as simple as a structure as box
// that has no added behaviors, and we can practice composing with it.
//

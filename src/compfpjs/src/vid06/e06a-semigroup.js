/*

An introduction to concating items via the formal Semigroup interface.
Semi-groups are simply a type with a concat method that are associative.
We define three semigroup instances and see them in action.

Semigroup is a type with a concat method which is associative.

Semigroup comes from abstract algebra. Since we are encoding the idea
in programming code, we keep the original name. We understand the name,
laws and properties that comes with this mathematical structure, rather
than making something up on our own.

*/

const { log } = require('../lib');

////
// String is a semigroup because it has a concat method. If you concat
// a string with a string, you get a string, to which you can keep
// concating.
//
const str = 'a'.concat('b').concat('c');
log(str);
//=> 'abc'

////
// Here the array is a semigroup since it has a concat method and
// concating an array with an array, gives an array, which is a
// semigroup and therefore we can keep concating.
//
const nums = [1, 2].concat([3, 4]).concat([5, 6]);
log(nums);
//=> [ 1, 2, 3, 4, 5, 6 ]

////
// Because algebra associativity, we can concatenate the right side
// first, and the result is the same:
//
const xs = [1, 2].concat([3, 4].concat([5, 6]))
//                      ^---------------------^
//                                 /
//                                /
//                       concating these first!
//
log(xs);
//=> [ 1, 2, 3, 4, 5, 6 ]

//
// Same as in math addition, because of associativity, no matter how we
// group the operations, the result will always be the same. That is a
// great property!
//
// So, addition is a semigroup, except we can't call concat on a number:
//
//   1.concat(1)
//
// Wouldn't work, but well handle things like this next.
//
//

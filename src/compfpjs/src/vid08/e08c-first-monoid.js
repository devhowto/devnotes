const { log } = require('../lib');

/**
 * Only keep the first one.
 *
 * Not promoted to a Monoid because we cannot find a neutral/identity
 * element for this case (not without some tricks we don't know yet).
 *
 * @sig Unknown -> First
 */
const First = x => ({
  x,
  concat: (_) => First(x),
  str: () => `First(${x})`,
});


log(
  First('hello')
  .concat(First('world'))
  .str()
);
//=> First('hello')
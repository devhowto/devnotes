const { log } = require('../lib');

/**
 * Only keep the first one.
 *
 * A Semigroup First type that knows how to concat (keep) the first
 * value ever.
 *
 * @sig Unknown -> First
 */
const First = x => ({
  x,
  concat: (_) => First(x),
  str: () => `First(${x})`,
});

log(First('hello').concat(First('world')).str());
//=> First('hello')

const { log } = require('../lib');

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

    str: function str() {
      return `First(${val})`;
    },
  }
}

log(First('hello').concat(First('world')).str());
//=> First('hello')

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

log(First('hello').concat(First('world')).inspect());
//=> First('hello')

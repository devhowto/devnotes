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

const res = Sum(1).concat(Sum(2));

log(res);
//=> 3

log(res.inspect());
//=> Sum(3)

const { log } = require('../lib');

/**
 * @sig Number -> Sum
 */
function Sum(val) {
  return {
    val,

    concat: function concat({ val: otherVal }) {
      return Sum(val + otherVal);
    },

    str: function str() {
      return `Sum(${val})`;
    }
  };
}

const res = Sum(1).concat(Sum(2));

log(res);
//=> { val: 3, ... }

log(res.str());
//=> Sum(3)

//
// tags: function compose composition functional-programming javascript
//

const log = console.log.bind(console);

/**
 * Returns a composition of the functions.
 *
 * ASSUME: All functions are unary.
 *
 * @param {Array<Function>}
 * @returns {Function}
 */
function compose(fns) {
  return function g(v) {
    if (!fns || !fns.length) return v;

    let result = v;

    for (let i = fns.length - 1; i >= 0; --i)
      result = fns[i](result);

    return result;
  };
}

function id(x) {
  return x;
}

function add1(x) {
  return x + 1;
}

function double(x) {
  return x * 2;
}

function str(val) {
  return String(val);
}

log(compose([str, double, add1])(1));
//=> "4"

log(compose([])(1));
//=> 1

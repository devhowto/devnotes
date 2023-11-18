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
    return fns.reduceRight((v, f) => f(v), v);
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

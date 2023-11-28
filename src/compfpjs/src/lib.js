//
// NOTE:
//
// These functions are simple, barebone helpers. They follow a garbage
// in, garbage out approach. If you give them crap, they give you back
// crap. Make sure to provide them with the data types specified in
// their JSDoc. YOU HAVE BEEN WARNED.
//

/**
 * @sig a -> Void
 */
const log = console.log.bind(console);

/**
 * @sig a -> a
 */
function idty(v) {
  return v;
}

/**
 * @sig a -> Boolean
 */
function isNil(v) {
  return v === undefined || v === null;
}

function Right(v) {
  return {
    map: f => Right(f(v)),
    fold: (f, g) => g(v),
    inspect: () => `Right(${v})`,
  };
}

function Left(v) {
  return {
    map: f => Left(v),
    fold: (f, g) => f(v),
    inspect: () => `Left(${v})`,
  };
}

/**
 * @sig a -> Either a
 */
function fromNullable(v) {
  return isNil(v) ? Left(null) : Right(v);
}


/**
 * @sig String -> String
 */
function toUpper(s) {
  return s.toUpperCase();
}

/**
 * @sig String -> String
 */
function toLower(s) {
  return s.toLowerCase();
}

/**
 * Trims `s`.
 *
 * @param {string} s
 * @returns {string}
 */
function trim(s) {
  return s.trim();
}

/**
 * Adds 1 to `n`.
 *
 * @param {number} n
 * @returns {number}
 */
function add1(n) {
  return n + 1;
}

/**
 * Produces a char based on the decimal `n`.
 *
 * @param {number} n The decimal number to create the char from.
 * @returns {string} A single-char string.
 */
function chr(n) {
  return String.fromCharCode(n);
}

/**
 * Parses the digits in `str` to a decimal number.
 *
 * @param {string} str
 * @param {number} [radix = 10] Defaults to base 10.
 * @returns
 */
function toInt(str, radix = 10) {
  return Number.parseInt(str, radix);
}

module.exports = {
  add1,
  chr,
  fromNullable,
  idty,
  isNil,
  log,
  toInt,
  toLower,
  toUpper,
  trim,
};

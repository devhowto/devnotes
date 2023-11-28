const Any = x => ({
  x,
  concat: ({ x: y }) => Any(x || y),
  str: () => `Any(${x})`,
});

/** 
 * `false` is the empty/neutral/identity element for `Any`.
 */
Any.empty = () => Any(false);

module.exports = { Any };
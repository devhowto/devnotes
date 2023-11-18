/**
 * All must be truthy.
 *
 * @sig Bool -> Bool
 */
const All = x => ({
  x,
  concat: ({ x: y }) => All(x && y),
  str: () => `All(${x})`,
});

/**
 * The neutral/identity element for all is `true`.
 *
 * By endowing `All` with `empty`, we are promoting
* it from a Semigroup to a Monoid.
 */
All.empty = () => All(true);

module.exports = { All };

const Sum = x => ({
  x,
  concat: ({ x: y }) => Sum(x + y),
  str: () => `Sum(${x})`,
});

/**
 * Zero is the identity property (neutral element) for addition.
 */
Sum.empty = () => Sum(0);

module.exports = { Sum };
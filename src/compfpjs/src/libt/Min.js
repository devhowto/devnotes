const Min = x => ({
  x,
  concat: ({ x: y }) => Min(x < y ? x : y),
  str: () => `Min(${x})`,
});

Min.empty = () => Min(Infinity);

module.exports = { Min };

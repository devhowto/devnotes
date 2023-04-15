const Max = x => ({
  x,
  concat: ({ x: y }) => Max(x > y ? x : y),
  str: () => `Max(${x})`,
});

Max.empty = () => Max(-Infinity)

module.exports = { Max };

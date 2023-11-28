const { Left } = require('./Either');

const First = x => ({
  x,
  concat: (_) => First(x),
  str: () => `First(${x})`,
});

First.empty = () => First(Left());

module.exports = { First };

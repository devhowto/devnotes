const Box = x => ({
  map: f => Box(f(x)),
  chain: f => f(x),
  fold: f => f(x),
  str: () => `Box(${x})`,
});

Box.of = x => Box(x),

module.exports = { Box };
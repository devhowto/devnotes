const Box = x => ({
  /**
   * Applies a function on the value of box 2.
   *
   * b2 is the box two, or “the other box”. x is a function in this
   * case, not a nromal value.
   *
   * @param {function} b2
   */
  ap: b2 => b2.map(x),

  map: f => Box(f(x)),
  chain: f => f(x),
  fold: f => f(x),
  str: () => `Box(${x})`,
});

Box.of = x => Box(x),

module.exports = { Box };

const { fromNullable } = require('../lib');

const Right = x => ({
  x,
  chain: f => f(x),
  ap: other => other.map(x),
  traverse: (of, f) => f(x).map(Right),
  map: f => Right(f(x)),
  fold: (_, g) => g(x),
  str: () => `Right(${x})`,
});

const Left = x => ({
  x,
  chain: f => Left(x),
  ap: other => Left(x),
  traverse: (of, f) => of(Left(x)),
  map: f => Left(x),
  fold: (f, _) => f(x),
  str: () => `Left(${x})`,
});

const tryCatch = (f) => {
  try {
    return Right(f());
  } catch (e) {
    return Left(e);
  }
};

const Either = {
  of: Right,
  tryCatch,
  fromNullable,
};

module.exports = { Left, Right, Either };

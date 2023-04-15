const Left = x => ({
  isLeft: true,
  isRight: false,
  map: _ => Left(x),
  fold: (f, _) => f(x),
  str: () => `Left(${x})`,
});

const Right = x => ({
  isRight: true,
  isLeft: false,
  map: f => Right(f(x)),
  fold: (_, g) => g(v),
  str: () => `Right(${x})`,
});

module.exports = { Left, Right };

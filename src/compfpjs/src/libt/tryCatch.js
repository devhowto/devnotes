const {
  Left,
  Right,
} = require('../libt');

const tryCatch = (f) => {
  try {
    return Right(f());
  } catch (e) {
    return Left(e);
  }
};

module.exports = { tryCatch };

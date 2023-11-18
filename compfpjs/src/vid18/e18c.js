const { log } = require('../lib');
const { Box } = require('../libt');

// Curried!
const add = x => y => x + y;

/**
 * A helper to lift Applicative for two args.
 *
 * Takes a function, some functor holding x, and some
 * functor holding y.
 */
const liftA2 = (f, fx, fy) =>
  fx.map(f).ap(fy)

const res1 = Box(add).ap(Box(3)).ap(Box(4));
log(res1.str());
//=> Box(7)

const res2 = liftA2(add, Box(3), Box(4));
log(res2.str());
//=> Box(7)


/**
 * A helper to lift Applicative for three args
 *
 * Takes a function, some functor holding x, and some
 * functor holding y, and some functor holding z.
 */
const liftA3 = (f, fx, fy, fz) =>
  fx.map(f).ap(fy).ap(fz);

const b = liftA3(
  x => y => z => x + y + z,
  Box(3),
  Box(4),
  Box(100),
);

log(b.str());
//=> Box(107)

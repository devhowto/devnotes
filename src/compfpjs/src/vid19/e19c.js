const { log } = require('../lib');
const { Either } = require('../libt');

// Lift applicative for two args.
const liftA2 = (f, fx, fy) =>
  fx.map(f).ap(fy);

// Fake element with height 10.
const $ = selector =>
  Either.of({ selector, height: 10 });

const getScreenSize = screen => head => foot =>
  screen - (head.height + foot.height);

const res = liftA2(getScreenSize(800), $('header'), $('footer'));

log(res.str());
//=> Right(780)

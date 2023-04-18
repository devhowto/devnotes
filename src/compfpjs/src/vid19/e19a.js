const { log } = require('../lib');
const { Either } = require('../libt');

const dir = v => console.dir(v, { depth: null });

const liftA2 = (f, fx, fy) =>
  fx.map(f).ap(fy);

//
// Fake element with height 10.
//
const $ = selector =>
  Either.of({ selector, height: 10 });

const getScreenSize = screen => head => foot =>
  screen - (head.height + foot.height);

const res = liftA2(getScreenSize(800), $('header'), $('footer'));
// log(res.str());

log(Either.of({ height: 10 }).str());

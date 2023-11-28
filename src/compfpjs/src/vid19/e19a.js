const { log } = require('../lib');
const { Either } = require('../libt');

// Fake element with height 10.
const $ = selector =>
  Either.of({ selector, height: 10 });

const getScreenSize = (screen, head, foot) =>
  screen - (head.height + foot.height);

const res =
  $('header').chain(header =>
    $('footer').map(footer =>
      getScreenSize(800, header, footer)));

log(res.str());
//=> Right(780)

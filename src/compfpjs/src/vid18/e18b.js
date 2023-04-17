const { log } = require('../lib');
const { Box } = require('../libt');

// Curried!
const add = x => y => x + y;

const boxOf7 = Box(add).ap(Box(4)).ap(Box(3));
log(boxOf7.str());
//=> Box(7)

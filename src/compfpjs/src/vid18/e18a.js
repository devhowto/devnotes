const { log } = require('../lib');
const { Box } = require('../libt');

const res1 = Box(x => x + 1).ap(Box(2));
log(res1.str());
//=> Box(3)

const boxOf2 = Box(x => y => x + y).ap(Box(2));
//=> Box(y => 2 + y)

const boxOf5 = boxOf2.ap(Box(3));
log(boxOf5.str());
// => Box(5)

const boxOf42 =
  Box(x => y => x + y)
  .ap(Box(40))
  .ap(Box(2));

log(boxOf42.str());
//=> Box(42)

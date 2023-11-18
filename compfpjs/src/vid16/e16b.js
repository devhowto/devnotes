const { Box, Right } = require('../libt');
const { log, add1 } = require('../lib');

/** 
 * Monad -> Monad
 */
const join = m => m.chain(x => x);

const m = Box.of(1);

const r1 = join(Box.of(m));
const r2 = join(m.map(Box.of));
log(r1.str(), r2.str());
//=> Box(1) Box(1)

log(m.chain(add1));
// 2

// chain() takes it out of the box, but we can
// put the value back in the box.
const r3 = m.chain(n => Box.of(add1(n)));
log(r3.str());
// Box(2)
const { Box, Right } = require('../libt');
const { log, add1 } = require('../lib');

/** 
 * Monad -> Monad
 */
const join = m => m.chain(x => x);

const m = Box(Box(Box(3)));
log(m);

const r1 = join(m.map(join));
const r2 = join(join(m));
log(r1.str(), r2.str());
//=> Box(3) Box(3)
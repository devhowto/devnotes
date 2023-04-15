const { log } = require('../lib');
const { Box } = require('../libt');

const id = x => x;

const r1 = Box('crayons').map(id);
const r2 = id(Box('crayons'));

log(r1.str());
//=> Box(crayons)

log(r2.str());
//=> Box(crayons)

log(r1.str() === r2.str());
//=> true
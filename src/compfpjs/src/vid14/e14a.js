const { log } = require('../lib');
const { Box } = require('../libt');

const r1 = Box('squirrels')
  .map(s => s.substring(5))
  .map(s => s.toUpperCase());

const r2 = Box('squirrels')
  .map(s => s.substring(5).toUpperCase());

log(r1.str());
//=> Box(RELS)

log(r2.str());
//=> Box(RELS)

log(r2.str() === r2.str());
//=> true

/*

Box is preserving this function composition:

'squirrels'.substring(5).toUpperCase()

*/
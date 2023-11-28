const { log } = require('../lib');
const { Any } = require('../libt');

log(Any.empty().str());
//=> Any(false)

log(
  Any.empty()
  .concat(Any(false))
  .concat(Any(true))
  .concat(Any(false))
  .str()
);
//=> Any(true)
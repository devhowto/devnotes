const { log } = require('../lib');
const { Max } = require('../libt');

log(Max.empty().str());
//=> Max(-Infinity)

log(
  Max.empty()
  .concat(Max(-1e99))
  .concat(Max(0))
  .concat(Max(Infinity))
  .str()
);
//=> Max(Infinity)

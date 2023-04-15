const { log } = require('../lib');
const { Min } = require('../libt');

log(Min.empty().str());
//=> Min(Infinity)

log(
  Min.empty()
  .concat(Min(-Infinity))
  .concat(Min(1e99))
  .str()
);
//=> Min(-Infinity)

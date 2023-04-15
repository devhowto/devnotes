const { log } = require('../lib');
const { Right, Left } = require('../libt');

const r1 = Right('squirrels')
  .map(s => s.substring(5))
  .map(s => s.toUpperCase());

const r2 = Right('squirrels')
  .map(s => s.substring(5).toUpperCase());

log(r1.str());
//=> Right(RELS)

log(r2.str());
//=> Right(RELS)

log(r2.str() === r2.str());
//=> true

const r3 = Left('squirrels')
  .map(s => s.substring(5))
  .map(s => s.toUpperCase());

const r4 = Left('squirrels')
  .map(s => s.substring(5).toUpperCase());

log(r3.str());
//=> Left(squirrels)

log(r4.str());
//=> Left(squirrels)

log(r3.str() === r4.str());
//=> true

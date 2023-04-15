const { log } = require('../lib');
const { List } = require('immutable-ext');

const id = x => x;

const r1 = List.of('crayons').map(id);
const r2 = id(List.of('crayons'));

log(r1.toJS());
//=> Box(crayons)

log(r2.toJS());
//=> Box(crayons)

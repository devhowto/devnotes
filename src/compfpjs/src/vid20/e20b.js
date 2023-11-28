const { log } = require('../lib');
const { List } = require('immutable-ext');

const res = List.of(x => x + 10).ap(List([1, 2, 3]));
log(res.toJS());
//=> [ 1, 2, 3 ]

const { log } = require('../lib');
const { List } = require('immutable-ext');

const res =
  List.of(color => size => item => `${color} ${size} ${item}`)
    .ap(List(['pink']))
    .ap(List(['small', 'medium', 'large']))
    .ap(List(['blouse', 'sweater']));

log(res.toJS());
//=> [
//=>   'pink small blouse',
//=>   'pink small sweater',
//=>   'pink medium blouse',
//=>   'pink medium sweater',
//=>   'pink large blouse',
//=>   'pink large sweater'
//=> ]

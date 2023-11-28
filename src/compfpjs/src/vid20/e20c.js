const { log } = require('../lib');
const { List } = require('immutable-ext');

const res =
  List.of(item => size => `${item}-${size}`)
    .ap(List(['blouse', 'sweater']))
    .ap(List(['small', 'medium', 'large']));

log(res.toJS());
//=> [
//=>   'blouse-small',
//=>   'blouse-medium',
//=>   'blouse-large',
//=>   'sweater-small',
//=>   'sweater-medium',
//=>   'sweater-large'
//=> ]

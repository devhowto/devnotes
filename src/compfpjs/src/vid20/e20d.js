const { log } = require('../lib');
const { List } = require('immutable-ext');

const res =
  List.of(item => size => color => `${item}-${size}-${color}`)
    .ap(List(['blouse', 'sweater']))
    .ap(List(['small', 'medium', 'large']))
    .ap(List(['pink']));

log(res.toJS());
//=> [
//=>   'blouse-small-pink',
//=>   'blouse-medium-pink',
//=>   'blouse-large-pink',
//=>   'sweater-small-pink',
//=>   'sweater-medium-pink',
//=>   'sweater-large-pink'
//=> ]

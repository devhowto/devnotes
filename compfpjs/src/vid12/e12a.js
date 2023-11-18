const { of, rejected } = require('folktale/concurrency/task');
const { log, add1 } = require('../lib');

of(1)
  .run()
  .listen({
    onRejected: e => log('err', e),
    onResolved: v => log('ok', v),
  });
//=> ok 1

rejected(-1)
  .map(add1)
  .listen({
    onRejected: e => log('err', e),
    onResolved: v => log('ok', v),
  });
//=> err -1
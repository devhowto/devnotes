const { of, rejected } = require('folktale/concurrency/task');
const { log, add1 } = require('../lib');

of(1)
  .map(add1) // runs
  .chain(x => of(x + 100))
  .run()
  .listen({
    onRejected: e => log('err', e),
    onResolved: v => log('ok', v),
  });
//=> ok 102

rejected(-1)
  .map(add1) // ignored
  .chain(x => of(x + 2)) // ignored
  .run()
  .listen({
    onRejected: e => log('err', e),
    onResolved: v => log('ok', v),
  });
//=> err -1

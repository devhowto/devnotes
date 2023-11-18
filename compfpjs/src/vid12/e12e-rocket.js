const { of, rejected, task } = require('folktale/concurrency/task');
const { log } = require('../lib');

const launchRocket = () => {
  return task(res => {
    log('Launching Rocket 🚀');
    res.resolve('Rocket');
  });
};

//
// ctrl is our application task. It won't run
// until we want it to with run().
//
const ctrl = launchRocket();

ctrl
  .map(s => s + '!!!')
  .run()
  .listen({
    onRejected: e => log('err', e),
    onResolved: v => log('ok', v),
  });


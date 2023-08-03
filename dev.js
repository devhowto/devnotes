export {};

const log = console.log.bind(console);

function wait(ms = 1000) {
  // setTimeout(function timer() {
  //   return Promise.resolve({ done: true });
  // });

  return new Promise((res, _rej) => {
    return setTimeout(() => {
      res({ done: true });
    }, ms);
  });
};

const sleep = (delay = 0) =>
  new Promise((resolve) => setTimeout(resolve, delay));

async function doIt() {
  await wait(3000);
  log('Wow!!');
}

await doIt();

const { log } = require('../lib');

const xs = [1, 2, 3];
const ys = [10, 20, 30];
const zs = [100, 200, 300];

for (const x of xs) {
  for (const y of ys) {
    for (const z of zs) {
      log({ x, y, z });
    }
  }
}

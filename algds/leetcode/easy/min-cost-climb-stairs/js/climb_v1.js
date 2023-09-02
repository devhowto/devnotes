const log = console.log.bind(console);
const min = Math.min.bind(Math);

function climb(costs, store = {}) {
  const len = costs.length;

  store[0] = costs[0];
  store[1] = costs[1];

  for (let i = 2; i < len; i++)
    store[i] = costs[i] + min(store[i - 1], store[i - 2]);

  return min(store[len - 1], store[len - 2]);
};

log(climb([3, 7, 2, 4, 9, 1]));




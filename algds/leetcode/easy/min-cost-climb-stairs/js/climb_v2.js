const log = console.log.bind(console);
const min = Math.min.bind(Math);

function climb(costs) {
  const len = costs.length;

  let prev1 = costs[0];
  let prev2 = costs[1];
  let curr;

  for (let i = 2; i < len; i++) {
    curr = costs[i] + min(prev1, prev2);
    prev1 = prev2;
    prev2 = curr;
  }

  return min(prev1, prev2);
};

log(climb([3, 7, 2, 4, 9, 1]));
log(climb([3, 7, 9, 11, 13, 14]));

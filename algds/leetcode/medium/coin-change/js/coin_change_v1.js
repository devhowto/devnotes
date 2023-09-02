var coinChange = function (coins, amount) {
  if (amount < 1) return 0;

  let tempMinCoin = Number.MAX_SAFE_INTEGER;
  const minCoinDP = Array(amount + 1).fill(Number.MAX_SAFE_INTEGER);
  minCoinDP[0] = 0;

  for (let i = 1; i < minCoinDP.length; i++) {
    for (let j = 0; j < coins.length; j++) {
      if (coins[j] > i) continue;

      if (coins[j] === i) {
        tempMinCoin = 1;
        break;
      }

      let remainingAmount = i - coins[j];
      tempMinCoin = Math.min(1 + minCoinDP[remainingAmount], tempMinCoin);
    }

    minCoinDP[i] = tempMinCoin;
    tempMinCoin = Number.MAX_SAFE_INTEGER;
  }

  if (minCoinDP[amount] === Number.MAX_SAFE_INTEGER) return -1;

  return minCoinDP[amount];
};

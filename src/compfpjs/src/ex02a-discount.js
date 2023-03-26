import {
  log,
} from "./lib.js";

/**
 * Converts a dollar money string to a float.
 *
 * @example
 * moneyToFloat('$ 1.99');
 * //=> 1.99
 */
function moneyToFloat(str) {
  return Number.parseFloat(str.replace(/\$/g, ''));
}

/**
 * Turns a percentage into its decimal float equivalent.
 *
 * NOTE: Multiplying by 0.01 is the same as dividing by 100, similar
 * to multiplying by 0.5 is actually the same as dividing by 2.
 *
 * @param {string} str
 * @returns {number}
 *
 * @example
 * percentToFloat('43%');
 * //=> 0.43
 *
 * percentToFloat('100%');
 * //=> 1
 *
 * percentToFloat('1%');
 * //=> 0.01
 */
function percentToFloat(str) {
  const replaced = str.replace(/\%/g, '');
  const num = Number.parseFloat(replaced);
  return num * 0.01;
}

/**
 * Applies a discount on a price.
 *
 * @param {string} price A price like '$ 4.99'.
 * @param {string} discount A percent discount like '20%'.
 * @returns {number} The total after the discount has been applied
 *   to the price.
 *
 * @example
 * applyDiscount('$ 80', '20%');
 * //=> 64
 */
function applyDiscount(price, discount) {
  const cost = moneyToFloat(price);
  const savings = percentToFloat(discount);

  return cost - cost * savings;
}

// log(['$1', '$12.01', '$73.49'].map(moneyToFloat));
//=> [1, 12.01, 73.49]

// log(['100%', '73%', '1%'].map(percentToFloat));
//=> [1, 0.73, 1]

log(percentToFloat('77.3%'));

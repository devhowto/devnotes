//
// tags: random uniform distribution probability javascript math
//

//
// Generates a chart to show that p5 generates uniform distribution
// of random numbers.
//

const LEN = 18;

const randomCounts = Array(LEN).fill(0);

function setup() {
  createCanvas(740, 440);
}

function draw() {
  const randLen = randomCounts.length;

  background(240);

  //
  // Each time a random number is generated, its count is
  // incremented. That count is then used later to draw
  // the bar's height.
  //
  let idx = floor(random(randLen));
  ++randomCounts[idx];

  stroke('red');
  fill('orangered');

  //
  // If we have LEN bars, we need to make sure we can fit
  // them all inside our canvas width. This formula gives
  // us the width of the bar so LEN bars of with barWidth
  // fits width.
  //
  let barWidth = width / randLen;

  for (let n = 0; n < randLen; ++n)
    rect(
      n * barWidth,
      height - randomCounts[n],
      barWidth - 1,
      randomCounts[n],
    );
}

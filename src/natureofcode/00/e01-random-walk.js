//
// tags: random walk javascript math
//


/*
 * Simulates a random walk in four directions.
 */

let walker;

class Walker {
  constructor() {
    this.x = width / 2;
    this.y = height / 2;
  }

  show() {
    stroke(5);
    point(this.x, this.y);
  }

  step() {
    // 0, 1, 2 or 3
    const choice = floor(random(4));

    if (choice === 0) ++this.x;
    else if (choice === 1) --this.x;
    else if (choice === 2) ++this.y;
    else --this.y;
  }
}

function setup() {
  createCanvas(400, 400);
  background(240);

  walker = new Walker();
}

function draw() {
  walker.step();
  walker.show();
}


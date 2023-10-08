//
// tags: random walk javascript math
//

//
// This version causes the random walk to have a greater tendency to
// move down and right.
//

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
    //
    // By making the proportion of positive numbers a bit larger than
    // the negative ones, the random walk has a tendency to move more
    // to the right and to the bottom than to the left and to the top.
    //
    // The random(-1, 2) would create a greater tendency, as there
    // would be a 1/2 ratio of negatives to positives. Something like
    // random(-3, 4) is a ratio of 3/4 of negatives to positives.
    //
    this.x += random(-3, 4);
    this.y += random(-3, 4);
  }
}

function setup() {
  createCanvas(400, 400);
  // background(240);

  walker = new Walker();
}

function draw() {
  walker.step();
  walker.show();
}


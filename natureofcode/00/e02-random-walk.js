//
// tags: random walk javascript math
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
    // Much simpler and produces 8 directions, instead of four
    // like the previous version.
    //
    this.x += random(-1, 1);
    this.y += random(-1, 1);
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


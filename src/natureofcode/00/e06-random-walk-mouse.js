//
// tags: exercise random walk javascript math
//

//
// Exercise 3.
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
    // A random number between 0 (inclusive) and 1 (exclusive).
    //
    const rand = random(1);

    console.log('==== mouse', { mouseX, mouseY });


    switch (true) {
      case (rand < 0.4):
        this.x++;
        break;
      case (rand < 0.6):
        this.x--;
        break;
      case (rand < 0.8):
        this.y++;
        break;
      default:
        this.y--;
        break;
    }
  }
}

function setup() {
  createCanvas(400, 400);
  frameRate(10);
  // background(240);

  walker = new Walker();
}

function draw() {
  walker.step();
  walker.show();
}


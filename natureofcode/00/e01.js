const log = console.log.bind(console);
const info = console.info.bind(console);

let walker;

class Walker {
  constructor() {
    log({ width, height });
    this.x = width / 2;
    this.y = height / 2;
    log(this.x, this.y);
  }

  show() {
    stroke(5);
    point(this.x, this.y);
  }

  step() {
    // 0, 1, 2 or 3
    const choice = floor(random(4));
    log(choice);

    //
    // TODO: Why doesn't this.x += 1 or this.x -= 1 work?
    //
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


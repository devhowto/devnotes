import { readFileSync } from 'node:fs';

const log = console.log.bind(console);


// raw data representation

// <Buffer 31 20 32 20 33 0a>
const line = readFileSync('./node/data1.txt', {
  flag: 'r',
  encoding: 'utf-8'
});

const nums = line.split(' ').map(e => Number(e));


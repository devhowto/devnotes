//
// https://github.com/ramda/ramda/issues/3438
//

import {
  pipe,
  pick,
  andThen,
} from "ramda";

var log = console.log.bind(console);

var jediDB = [
  {
    id: 1,
    name: "Yoda",
    skill: "The Force",
    level: 100,
  },
  {
    id: 2,
    name: "Ahsoka Tano",
    skill: "Lightsaber",
    level: 97,
  },
  {
    id: 3,
    name: "Aayla Secura",
    skill: "Mind trick",
    level: 99,
  },
];

/**
 * Fetches a jedi by the ID.
 *
 * NOTE: Pretend this is performing an HTTP request to
 * a JSON API endpoint.
 */
function fetchJedi(id, timeMs = 256) {
  return new Promise((resolve) => {
    setTimeout(() => {
      var jedi = jediDB.find(jedi => jedi.id === id);
      resolve(jedi);
    }, timeMs);
  });
}

var fetchJediById = pipe(
  fetchJedi,
  andThen(pick(['name', 'skill'])),
);

// fetchJediById(3).then(log);

pipe(
  fetchJediById,
  andThen(log),
)(1);
//=> { name: 'Yoda', skill: 'The Force' }

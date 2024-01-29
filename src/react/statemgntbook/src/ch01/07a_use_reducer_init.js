import { useState, useReducer } from 'react';

const log = console.log.bind(console);

function reducer(state, action) {
  log({ state, action });
  switch(action.type) {
    case 'INCREMENT': return state + 1;
    default: return state;
  }
};

function Counter({ initialCount = 0 }) {
  const [count, dispatch] = useReducer(reducer, initialCount);

  return (
    <section>
      <pre>{count}</pre>
      <button onClick={() => dispatch({ type: 'INCREMENT' })}>+1</button>
    </section>
  );
}

export { Counter };

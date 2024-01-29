// import { Counter } from './ch01/01a_use_count.js';
// import { Counter } from './ch01/01b_use_count.js';
// import { Counter } from './ch01/01c_use_count.js';
// import { Counter } from './ch01/01d_use_count.js';
// import { Parent } from './ch01/02a_global_state';
// import { Parent } from './ch01/02b_global_state';
// import { Counter } from './ch01/03a_use_state';
// import { Counter } from './ch01/03b_use_state';
// import { Counter } from './ch01/03c_use_state';
// import { Counter } from './ch01/03e_use_state';
// import { Counter } from './ch01/03f_use_state';
// import { Counter } from './ch01/03g_use_state';
// import { Component } from './ch01/04a_use_reducer';
// import { Component } from './ch01/04b_use_reducer';
// import { Component } from './ch01/04c_use_reducer';
// import { Component } from './ch01/04d_use_reducer';
// import { Counter } from './ch01/05a_use_state_with_use_reducer';
// import { Counter } from './ch01/05b_use_state_with_use_reducer';
// import { Counter } from './ch01/06a_use_reducer_with_use_state';
import {
  CounterWithUseState,
  CounterWithUseReducer,
} from './ch01/06b_use_reducer_with_use_state';

import './App.css';

function App() {
  return (
    <main>
      <CounterWithUseState />
      <CounterWithUseReducer />
    </main>
  )
}

export default App;

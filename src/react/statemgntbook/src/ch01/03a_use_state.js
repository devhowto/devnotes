import { useState } from "react";

function Counter() {
  const [count, setCount] = useState(0);

  return (
    <section>
      <pre>{count}</pre>
      <button onClick={() => setCount(1)}>
        Set count to 1
      </button>
    </section>
  );
}

export { Counter };

//
// What happens if the user clicks again? It will invoke setCount()
// again, but as the value is the same, it will BAIL OUT.
//
// BAILOUT is a technical term in react and it basically means
// “avoid rerenders”.
//

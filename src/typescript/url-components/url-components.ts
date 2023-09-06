//
// Run with:
//
//   $ ts-node -T --esm ./url-components
//

const log: Console["log"] = console.log.bind(console);

function str(val: unknown): string {
  return String(val);
}

type Nullable<T> = T | null | undefined;

type URLComponents = Partial<{
  scheme: "https" | "http";
  host: string;
  port: number | string;
  path: string;
  params: Record<string, Nullable<string | number | boolean>>;
  fragment: string;
}>;

type Product = {
  id: string;
  url: URLComponents;
  imageUrl: URLComponents;
};

/**
 * Transforms an object into query string URL params.
 */
function toQS(
  params: Record<string, Nullable<string | number | boolean>>
): string {
  return Object.keys(params) .reduce(
    (acc: Array<string>, key: string): Array<string> => {
      return params[key] != null
        ? [...acc, `${key}=${params[key]}`]
        : [...acc, key];
    }, ["?"]).join("&");
};

function makeUrl(components: URLComponents): string[] {
  const keys: Array<keyof URLComponents> = [
    "scheme",
    "host",
    "port",
    "path",
    "params",
    "fragment",
  ];

  return keys.reduce(
    (
      acc: Array<string>,
      key: typeof keys[number]
    ): Array<string> => {
      const comp = components[key];

      if (comp === undefined) return acc;

      if (key === "scheme") return [...acc, `${://"];

      if (key === "params")
        return [...acc, toQS(components.params || {}) ];

      return [...acc, str(components[key])];

  }, []);
}

const url1: URLComponents = {
  scheme: "https",
  host: "myproj.local",
  path: "/blog",
  // params: {
  //   id: 1,
  //   category: "blog",
  // },
};

log(makeUrl(url1));

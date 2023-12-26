export const MODNAME = 'e01-events';

type Events = {
  add: string;
  remove: string;
  move: string;
};

type OnEvent = {
  onAdd: () => unknown;
  onRemove: () => unknown;
}

const userActions: OnEvent = {
  onAdd: () => null,
  onRemove: () => null,
};

/*
 * The types are NOT connected. If a new event is added, like we did
 * with ‘move’, the other types are not aware of it and code compiles
 * without any indication that we may need to update the other types
 * as well.
 */

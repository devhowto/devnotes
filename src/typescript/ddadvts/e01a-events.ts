export const MODNAME = 'e01a-events';

type Events = {
  add: string;
  remove: string;
  ////
  // Note this `move', which was added by someone at some time after the
  // initial implementation. But the type and object below do not
  // complain that `move` is missing.
  //
  move: string;
};

type OnEvent = {
  onAdd: () => unknown;
  onRemove: () => unknown;
  //
  // Not complaining that `move` is not here.
  //
}

const userActions: OnEvent = {
  onAdd: () => null,
  onRemove: () => null,
  //
  // Not complaining that `move` is not here.
  //
};

/*
 * The types are NOT connected. If a new event is added, like we did
 * with ‘move’, the other types are not aware of it and code compiles
 * without any indication that we may need to update the other types
 * as well.
 */

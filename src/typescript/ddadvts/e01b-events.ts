export const MODNAME = 'e01b-events';

type Events = {
  add: string;
  remove: string;
  move: string;
};

type EventKeys = keyof Events;

type OnEvent = {
  // Mapped types!
  [Key in EventKeys]: () => unknown;
}

/**
 * Note we don't have the “on” with the uppercase action any longer.
 *
 * But at least, if we add another event name or action, we'll
 * immediately get compiler messages informing us of the mismatches.
 */
const userActions: OnEvent = {
  add: () => null,
  remove: () => null,
  move: () => null,
};

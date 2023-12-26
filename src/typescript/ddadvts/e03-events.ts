export const MODNAME = 'e03-events';

type Events = {
  add: string;
  remove: string;
  move: string;
};

type EventKeys = keyof Events;

type OnEvent = {
  [Key in EventKeys as `on${Capitalize<Key>}`]: () => unknown;
}

const userActions: OnEvent = {
  onAdd: () => null,
  onRemove: () => null,
  onMove: () => null,
};

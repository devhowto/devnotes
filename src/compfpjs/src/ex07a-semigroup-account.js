
const acct1 = {
  name: 'Ahsoka',
  isPaid: true,
  points: 10,
  friends: ['Aayla'],
};

const acct2 = {
  name: 'Ahsoka',
  isPaid: false,
  points: 2,
  friends: ['Leia'],
};

/*

Someone accidentally created two accounts.
They want to combine them.
When thinking of combining things, think Semigroups,
because that is a way of combining/concatting things
together.

If a data structure is entirely made of Semigroups, it
will be a Semigroup itself. Concatting two strings still
gives back a string. Concatting two arrays still gives
back an array.

Semigroups are the combining/concatting idea. Concatting/combining Semigroups gives you back a Semigroup.

If I can concat pieces of my data structure, my data
structure is therefore concatable.

*/

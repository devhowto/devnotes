import { parens } from './parens_v1.js';

describe('parens(n)', () => {
  describe('when the input is 0', () => {
    it('should return an array with 1 empty string', () => {
      expect(parens(0)).toEqual([''])
    });
  });

  describe('when the input is 1', () => {
    it('should return 1 set of balanced parenthesis', () => {
      expect(parens(1)).toEqual(['()'])
    });
  });

  describe('when the input is 2', () => {
    it('should return 2 set of balanced parenthesis', () => {
      expect(parens(2)).toEqual(['(())', '()()'])
    });
  });

  describe('when the input is 3', () => {
    it('should return the five possible ', () => {
      expect(
        parens(3)
      ).toEqual(['((()))', '(()())', '(())()', '()(())', '()()()'])
    });
  });
});

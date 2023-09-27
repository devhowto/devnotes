//
// tags: codewars haskell algorithm string
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <criterion/criterion.h>

void sdup(const char* src, char* dst) {
  while ((*dst++ = *src++))
    ;
}

void strrev(char* src, char* dst) {
  short i = 0;
  short j = strlen(src);

  *(dst + j--) = '\0';

  while ((j >= 0)) {
    *(dst + j--) = *(src + i++);
  }
}

void spin_words(const char* s, char* r) {
  char* cpy = malloc(strlen(s) + 1);
  sdup(s, cpy);
  short cnt = 0;
  char* tok = strtok(cpy, " ");

  while (tok != NULL) {
    char* rev = malloc(strlen(tok) + 1);

    if (cnt++ != 0)
      strcat(r, " ");

    if (strlen(tok) >= 5) {
      strrev(tok, rev);
      strcat(r, rev);
    }
    else {
      strcat(r, tok);
    }

    tok = strtok(NULL, " ");

    free(rev);
  }

  free(cpy);
}

void tester(const char *sentence, char *result);

Test(Example_Tests, should_pass_all_the_tests_provided) {
  tester("Welcome",              "emocleW"             );
  tester("spam",                 "spam"                );
  tester("This is a test",       "This is a test"      );
  tester("This is another test", "This is rehtona test");
}

void tester(const char *sentence, char *expected) {
  char submitted[30 * 14 + 1];

  spin_words(sentence, (char *)&submitted);

  if (strcmp(submitted, expected)) {
    cr_assert_fail(
      "Sentence:  \"%s\"\n \nSubmitted: \"%s\"\n \nExpected:  \"%s\"\n \n",
      sentence,             submitted,            expected
    );
  }

  cr_assert(1);
}

// int main(void) {
//   char s[] = "The source is wicked with these ones.";
//   char w[200];
//   spin_words(s, w);
//
//   printf("%s\n", w);
//
//   return 0;
// }

//
// $ make ./out/spin_words_v1 && ./out/spin_words_v1
// gcc -std=c99 -Wall -pedantic -g -lc spin_words_v1.c -o out/spin_words_v1
//
// Welcome
// emocleW
//
// Use the force Luke
// Use the ecrof Luke
//
// red hat hell
// red hat hell
//
// allocate enough memory
// etacolla hguone yromem
//

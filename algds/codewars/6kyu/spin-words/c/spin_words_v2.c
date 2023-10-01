//
// tags: codewars C algorithm string
//

#include <stdio.h>
#include <criterion/criterion.h>

#define REVERSE_THRESHOLD_LENGHT 5

void tester(const char *sentence, char *result);

void spin_words(const char *str, char *res) {
  int count = 0, i, j;

  for (i = 0; str[i] != '\0'; i++) {
    if (str[i + 1] == ' ' || str[i + 1] == '\0') {
      count++;

      if (count >= REVERSE_THRESHOLD_LENGHT)
        for (j = 0; j < count; ++j)
          res[i + 1 - count + j] = str[i - j];
      else
        for (j = 0; j < count; ++j)
          res[i - j] = str[i - j];

      count = -1;
      res[i + 1] = str[i + 1];
    }
    else count++;
  }
}

// int main(void) {
//   char s[] = "the force is strong";
//   char r[100] = { '\0' };
//   spin(s, r);
//   printf("%s\n", r);
//
//   return 0;
// }

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

/*
void spin_words(const char *sentence, char *result) {
  int count = 0, i, j;

  for (i = 0; sentence[i] != '\0'; i++) {
    if (sentence[i + 1] == ' ' || sentence[i + 1] == '\0') {
      count++;

      if (count >= 5) {
        for (j = 0; j < count; j++)
          result[i + 1 - count + j] = sentence[i - j];
      }
      else {
        for (j = 0; j < count; j++)
          result[i - j]=sentence[i - j];
      }

      count = -1;
      result[i + 1] = sentence[i + 1];
    }

    else count++;
  }
}
*/

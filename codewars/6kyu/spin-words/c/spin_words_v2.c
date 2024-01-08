//
// tags: codewars C algorithm string
//

#include <stdio.h>
#include <criterion/criterion.h>

#define REVERSE_THRESHOLD_LENGHT 5

void spin_words(const char *str, char *res) {
  int cnt = 0, i, j;

  for (i = 0; str[i] != '\0'; i++) {
    cnt++;

    if (str[i + 1] != ' ' && str[i + 1] != '\0')
      continue;

    if (cnt >= REVERSE_THRESHOLD_LENGHT)
      for (j = 0; j < cnt; ++j)
        res[i + 1 - cnt + j] = str[i - j];
    else
      for (j = 0; j < cnt; ++j)
        res[i - j] = str[i - j];

    cnt = -1;
    res[i + 1] = str[i + 1];
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

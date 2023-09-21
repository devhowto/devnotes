//
// tags: codewars haskell algorithm string
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void strdup(const char* src, char* dst) {
  while ((*dst++ = *src++) != '\0')
    ;

  *dst = '\0';
}

void str_rev(char* src, char* dst, short len) {
  short i = 0;
  short j = len - 1;

  *(dst + (len - 1)) = '\0';

  while (i < len)
    *(dst + j--) = *(src + i++);
}

void spin_words(const char* s, char* r) {
  // Copy so we own cpy mem.
  char* cpy = 0;
  char* tok = 0;
  cpy = malloc(sizeof(s));
  short cnt = 0;

  strdup(s, cpy);

  tok = strtok(cpy, " ");

  while (tok != NULL) {
    short l = strlen(tok);

    if (l >= 5) {
      char* rev = malloc(l);

      str_rev(tok, rev, l);

      if (cnt++ != 0)
        strcat(r, " ");

      strcat(r, rev);
    }
    else {
      if (cnt++ != 0)
        strcat(r, " ");

      strcat(r, tok);
    }

    tok = strtok(NULL, " ");
  }
}

int main(void) {
  char* s0 = "Welcome";
  char* w0 = malloc(strlen(s0));
  char* s1 = "Use the force Luke";
  char* w1 = malloc(strlen(s1));
  char* s2 = "red hat hell";
  char* w2 = malloc(strlen(s2));
  char* s3 = "allocate enough memory";
  char* w3 = malloc(strlen(s3));

  spin_words(s0, w0);
  spin_words(s1, w1);
  spin_words(s2, w2);
  spin_words(s3, w3);

  printf("\n");
  printf("%s\n%s\n\n", s0, w0);
  printf("%s\n%s\n\n", s1, w1);
  printf("%s\n%s\n\n", s2, w2);
  printf("%s\n%s\n\n", s3, w3);

  return 0;
}

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

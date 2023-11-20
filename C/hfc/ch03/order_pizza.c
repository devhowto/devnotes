#include <stdio.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
  char *delivery = "";
  int thick = 0;
  int count = 0;
  char ch;

  while ((ch = getopt(argc, argv, "d:t")) != EOF)
    switch (ch) {
    case 'd':
      delivery = optarg;
      break;
    case 't':
      thick = 1;
      break;
    default:
      fprintf(stderr, "Unknown option: “%s”\n", optarg);
      return 1;
    }

  argc -= optind;
  argv += optind;

  if (thick)
    fprintf(stdout, "Thick crust!\n");

  if (delivery[0])
    fprintf(stderr, "To be delivered “%s”.\n", delivery);

  fprintf(stdout, "Ingredients:\n");

  for (count = 0; count < argc; ++count)
    fprintf(stdout, "%s\n", argv[count]);

  return 0;
}

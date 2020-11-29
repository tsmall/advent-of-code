#include <stdio.h>
#include <stdlib.h>

int
main (int argc, char **argv)
{
  {
    FILE *char_file = fopen("chars.txt", "r");
    if (char_file == NULL)
      {
        fprintf(stderr, "ERROR: Unable to open file\n");
        exit(1);
      }
    
    char c;
    while ((c = fgetc(char_file)) != EOF)
      {
        if (c != '\n')
          printf("CHAR: %c\n", c);
      }

    fclose(char_file);
  }

  {
    FILE *line_file = fopen("lines.txt", "r");
    if (line_file == NULL)
      {
        fprintf(stderr, "ERROR: Unable to open file\n");
        exit(1);
      }
    
    int line_length = 100;
    char *line = malloc(sizeof(char) * line_length);
    int chars_read;
    while ((chars_read = getline(&line, &line_length, line_file)) > 0)
      {
        // Drop the newline character.
        line[chars_read - 1] = '\0';
        printf("LINE: %s\n", line);
      }

    free(line);
    fclose(line_file);
  }

  {
    FILE *numbers_file = fopen("numbers.txt", "r");
    if (numbers_file == NULL)
      {
        fprintf(stderr, "ERROR: Unable to open file\n");
        exit(1);
      }
    
    int number;
    while (fscanf(numbers_file, "%d", &number) != EOF)
      {
        printf("NUMBER: %d\n", number);
      }
    
    fclose(numbers_file);
  }
}

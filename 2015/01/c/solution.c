#include <stdio.h>

int main() {
    int floor = 0;
    int basementidx = 0;

    char c;
    int i = 1;
    while ((c = getchar()) != EOF) {
        switch (c) {
        case '(':
            floor++;
            break;
        case ')':
            floor--;
            if (floor < 0 && basementidx == 0) {
                basementidx = i;
            }
            break;
        }
        i++;
    }

    printf("--- Part One ---\n");
    printf("%d\n", floor);
    printf("--- Part Two ---\n");
    printf("%d\n", basementidx);

    return 0;
}

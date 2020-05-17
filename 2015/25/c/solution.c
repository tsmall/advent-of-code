#include <stdbool.h>
#include <stdio.h>

typedef struct point {
    int x;
    int y;
} point;

static inline bool equal(const point p1, const point p2) {
    return (p1.x == p2.x) && (p1.y == p2.y);
}

int main() {
    point p = { .x = 1, .y = 1 };
    const point end = { .x = 3075, .y = 2981 };

    long int code = 20151125;
    const int mult = 252533;
    const int div  = 33554393;

    int diagonal, count;
    for (diagonal = 2;; diagonal++) {
        p.x = 1;
        p.y = diagonal;
        for (count = 1; count <= diagonal; count++) {
            code = (code * mult) % div;
            if (equal(p, end)) goto found;

            p.x += 1;
            p.y -= 1;
        }
    }

 found:
    printf("--- Part 1 ---\n");
    printf("%ld\n", code);
}

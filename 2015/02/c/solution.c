#include <stdio.h>

struct Dimension {
    int l;
    int w;
    int h;
};
typedef struct Dimension Dimension;

int min(int length, int nums[]) {
    int min = nums[0];
    for (int i = 1; i < length; i++) {
        if (nums[i] < min) min = nums[i];
    }
    return min;
}

int wrapping_paper_needed(Dimension d) {
    int area_lw = d.l * d.w;
    int area_wh = d.w * d.h;
    int area_hl = d.h * d.l;
    int extra   = min(3, (int[]) {area_lw, area_wh, area_hl});

    return (2 * area_lw) + (2 * area_wh) + (2 * area_hl) + extra;
}

int ribbon_needed(Dimension d) {
    int perim_lw = (2 * d.l) + (2 * d.w);
    int perim_wh = (2 * d.w) + (2 * d.h);
    int perim_hl = (2 * d.h) + (2 * d.l);
    int bow      = d.l * d.w * d.h;

    return min(3, (int[]) {perim_lw, perim_wh, perim_hl}) + bow;
}

int main() {
    int total_wrapping_paper = 0;
    int total_ribbon         = 0;

    Dimension d;
    while (fscanf(stdin, "%dx%dx%d", &d.l, &d.w, &d.h) == 3) {
        total_wrapping_paper += wrapping_paper_needed(d);
        total_ribbon         += ribbon_needed(d);
    }

    printf("--- Part One ---\n");
    printf("%d\n", total_wrapping_paper);
    printf("--- Part Two ---\n");
    printf("%d\n", total_ribbon);
}

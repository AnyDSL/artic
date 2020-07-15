#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

void write(const uint8_t* line, size_t size) {
    fwrite(line, size, 1, stdout);
}

void print(const uint8_t* s) {
    printf("%s", s);
}

void println(const uint8_t* s) {
    printf("%s\n", s);
}

void print_i32(int32_t i) {
    printf("%"PRIi32"\n", i);
}

void print_u8(uint8_t c) {
    printf("%c\n", c);
}

void print_f64(double d) {
    printf("%.9f\n", d);
}

void print_piece_mask(const uint64_t* a) {
    for (int i = 0; i < 12; ++i)
        printf("%"PRIu64" ", a[i]);
    printf("\n");
}

void print_piece_def(const uint8_t* a) {
    for (int i = 0; i < 40; ++i)
        printf("%d ", a[i]);
    printf("\n");
}

void print_meteor_scnt(int32_t cnt) {
    printf("%"PRIi32" solutions found\n\n", cnt);
}

void print_meteor_lines(const uint8_t* a) {
    printf("%c %c %c %c %c \n %c %c %c %c %c \n",
        a[0] + '0', a[1] + '0', a[2] + '0', a[3] + '0', a[4] + '0',
        a[5] + '0', a[6] + '0', a[7] + '0', a[8] + '0', a[9] + '0');
}

double* alloc_img(int32_t w, int32_t h) {
    return calloc(sizeof(double), w * h * 3);
}

static inline uint8_t convert_col(double col) {
    col *= 255.0;
    if (col < 0) col = 0;
    if (col > 255) col = 255;
    return col;
}

void save_img(int32_t w, int32_t h, const double* img) {
    printf("P6\n");
    printf("%d %d\n", w, h);
    printf("255\n");
    uint8_t* out_row = malloc(sizeof(uint8_t) * 3 * w);
    for (int32_t y = 0; y < h; y++) {
        const double* in_row = &img[y * (w * 3)];
        for (int32_t x = 0; x < w; x++) {
            out_row[x * 3 + 0] = convert_col(in_row[x * 3 + 0]);
            out_row[x * 3 + 1] = convert_col(in_row[x * 3 + 1]);
            out_row[x * 3 + 2] = convert_col(in_row[x * 3 + 2]);
        }
        fwrite(out_row, w * 3, sizeof(uint8_t), stdout);
    }
    free(out_row);
}

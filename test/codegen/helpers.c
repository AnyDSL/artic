#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>

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

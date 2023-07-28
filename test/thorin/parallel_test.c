#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

void parallel_test(int, int*, int*, int*);

int main(int argc, char** argv) {
    int n = 4096 * 1024; // 4M elements
    int* a = malloc(sizeof(int) * n);
    int* b = malloc(sizeof(int) * n);
    int* c = malloc(sizeof(int) * n);
    for (int i = 0; i < n; i++) {
        a[i] = i % 100;
        b[i] = 100 - i % 100;
    }
    parallel_test(n, a, b, c);
    for (int i = 0; i < n; i++) {
        assert(c[i] == 100);
    }
    printf("It werks.\n");
}
#include <stdio.h>
#include <stdlib.h>

double** make_matrix (int m, int n) {
    double** a;
    int i;

    a = calloc(m, sizeof(double*));
    for (i = 0; i < m; i += 1) {
        a[i] = calloc(n, sizeof(double));
    }
    return a;
}

int print_matrix(double** a, int m, int n, double* array_c, double* array_b) {
    printf("max z = %10.1lfx0 %+10.1lfx1\n", array_b[0], array_b[1]);
    for (int c = 0; c < m; c++) {
        printf("%+10.1lfx0%+10.1lfx1 \u2264 %10.1lf", a[c][0], a[c][1], array_c[c]);
        printf("\n");
    }
    return 0;
}

int main(void) {
    int m;
    int n;
    double* c;
    double* b;
    double** matrix;

    c = calloc(n, sizeof(double));
    b = calloc(n, sizeof(double));

    scanf("%d %d", &m, &n);
    scanf("%lf %lf", &c[0], &c[1]);
    matrix = make_matrix(m, n);

    for (int c = 0; c < m; c++) {
        for (int r = 0; r < n; r++) {
            scanf("%lf", &matrix[c][r]);
        }
    }
    scanf("%lf %lf", &b[0], &b[1]);
    print_matrix(matrix, m, n, b, c);
    for (int i = 0; i < n; i++){
        free(matrix[i]);
    }
    free(b);
    free(c);
    free(matrix);
    return 0;
}

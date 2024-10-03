#include <stdio.h>
#include <stdlib.h>
#include <math.h>

const double EPS = 10e-9;

void bp(void){  }

double** make_matrix (int m, int n) 
{
    double** a;
    int i;

    a = calloc(m, sizeof(double*));
    for (i = 0; i < m; i += 1) {
        a[i] = calloc(n+1, sizeof(double));
    }
    return a;
}

void* xmalloc(size_t size)
{
   void* ptr = malloc(size);

   if (ptr == NULL) {
      fprintf(stderr, "out of memory\n");
      exit(1);
   }

   return ptr;
}

typedef struct simplex_t 
{ 
    int m; /* Constraints. */ 
    int n; /* Decision variables. */
    int *var;/* 0..n  1 are nonbasic. */ 
    double **a; /* A. */ 
    double *b; /* b. */ 
    double *x; /* x. */ 
    double *c; /* c. */ 
    double y; /* y. */ 
} simplex_t;

int initial (simplex_t* s, int m, int n, double** a, double* b, double* c, double* x, int y, int* var);


void pivot (simplex_t* s, int row, int col)
{
    double** a = s->a;    
    double* b = s->b;     
    double* c = s->c;    
    int m = s->m;       
    int n = s->n;      
    int i,j,t; 
    
    // Swap the variable indices between the entering and leaving variables
    t = s->var[col];             // Save the variable that is entering the basis
    s->var[col] = s->var[n+row]; // Update the variable at the entering column with the one leaving
    s->var[n+row] = t;           // Set the leaving variable index
    
    // Update the value of the objective function based on the pivot operation
    s->y = s->y + c[col] * b[row] / a[row][col]; 

    // Update the coefficients of the objective function (row of the simplex tableau)
    for (i = 0; i < n; i = i + 1) 
        if (i != col) 
            c[i] = c[i] - c[col] * a[row][i] / a[row][col];
    
    // Update the coefficient of the entering variable in the objective function to its new value
    c[col] = - c[col] / a[row][col]; 
    
    // Update the right-hand side vector (constants) for all rows except the pivot row
    for (i = 0; i < m; i = i + 1) 
        if (i != row) 
            b[i] = b[i] - a[i][col] * b[row] / a[row][col];

    // Update the constraint matrix for all rows except the pivot row and all columns except the pivot column
    for (i = 0; i < m; i = i + 1) 
        if (i != row) 
            for (j = 0; j < n; j = j + 1) 
                if (j != col) 
                    // Update each non-pivot element based on the pivot element
                    a[i][j] = a[i][j] - a[i][col] * a[row][j] / a[row][col]; 

    // Update the pivot column for all rows except the pivot row
    for (i = 0; i < m; i = i + 1) 
        if (i != row) 
            a[i][col] = -a[i][col] / a[row][col]; 

    // Update the pivot row for all columns except the pivot column
    for (i = 0; i < n; i = i + 1) 
        if (i != col)  
            a[row][i] = a[row][i] / a[row][col]; 

    // Update the right-hand side of the pivot row
    b[row] = b[row] / a[row][col];

    // Set the pivot element to its final value (1 after normalization)
    a[row][col] = 1 / a[row][col];
}

int select_nonbasic (simplex_t* s) 
{
    int i; 
    for (i = 0; i < s->n; i = i + 1) {
        if (s->c[i] > EPS) { 
            return i;
        }
    }
    return -1;
}


void prepare(simplex_t* s, int k)
{
    int m = s->m;  
    int n = s->n; 
    int i;

    // Shift the variable indices to make room for the new artificial variable
    // Move all variables one position up in the var array
    for(i = m + n; i > n; i = i - 1) 
        s->var[i] = s->var[i-1];  // Shift the variables down by one position

    // Set the variable at index 'n' to the new artificial variable (m + n)
    s->var[n] = m + n;  // Add the new variable in the variable list

    // Increase the number of variables by one
    n = n + 1;  
    printf("n = %d\n", n);  // Print the new number of variables for debugging

    // Modify the constraint matrix (tableau) to add the artificial variable
    // Set the coefficients of the new variable (artificial variable) to -1 in each constraint row
    for(i = 0; i < m; i = i + 1) 
        s->a[i][n-1] = -1;  // The last column (new variable's column) gets a value of -1 for each row

    // Allocate memory for the solution vector and the cost vector
    // The solution vector 'x' is extended to include the new artificial variable
    s->x = calloc(m + n, sizeof(double));  // Allocate memory for solution vector (all zeroed initially)
    
    // The cost vector 'c' is also extended to include the new variable
    s->c = calloc(n, sizeof(double));  // Allocate memory for cost vector
    
    // Set the cost of the artificial variable to -1 in the objective function
    s->c[n-1] = -1;  // Set the cost of the new variable (last column) to -1 in the objective function

    // Update the number of variables in the simplex structure to include the new variable
    s->n = n;

    // Perform a pivot operation to bring the k-th constraint into the basis using the new artificial variable
    pivot(s, k, n-1);  // Pivot on row 'k' and the newly added variable (last column)
}

int init (simplex_t *s, int m, int n, double** a, double* b, double* c, double* x, double y, int* var)
{
    int i,k;
    s->m = m;
    s->n = n;
    s->a = a;
    s->b = b;
    s->c = c;
    s->x = x;
    s->y = y;
    s->var = var;

    if (s->var == NULL) {
        s->var = calloc(m + n + 1, sizeof(int)); 
        for (i = 0; i < m+n; i = i + 1) 
            s->var[i] = i; 
    }
    
    for (k = 0, i = 1; i < m; i = i + 1) 
        if ((s -> b[i]) < (s -> b[k])) 
            k = i; 
    return k;
}


double xsimplex (int m, int n, double** a, double* b, double* c, double* x, double y, int* var, int h)
{
    struct simplex_t s;  
    int i, row, col;    

    if (!initial(&s, m, n, a, b, c, x, y, var)) {  
        free(s.var);       
        s.var = NULL;     
        printf("WARNING: No solution\n");  
        return NAN;      
    }

    // Main Simplex loop: Perform pivoting as long as there's a non-basic variable that can improve the objective
    while ((col = select_nonbasic(&s)) >= 0) {  // Find the non-basic variable (column) to enter the basis
        row = -1; 

        // Select the pivot row using the minimum ratio test (Bland's rule)
        for (i = 0; i < m; i = i + 1) {  
            if ((a[i][col] > EPS) &&     // Check if the coefficient in the selected column is positive
                ((row < 0) || (b[i] / a[i][col] < b[row] / a[row][col]))) {
                    row = i;  // Choose the row with the smallest ratio of b[i] / a[i][col]
            }
        }

        if (row < 0) {
            free(s.var); 
            s.var = NULL; 
            printf("WARNING: Unbounded\n"); 
            return INFINITY;  
        }

        // Perform the pivot operation on the selected row and column
        pivot(&s, row, col);  // Pivot on the chosen row and column to update the tableau
    }

    // If h == 0 (standard simplex algorithm phase, without artificial variables)
    if (h == 0) {
        // Set all variables that are non-basic (not in the basis) to zero
        for (i = 0; i < n; i = i + 1) {
            if (s.var[i] < n) {  // If the variable index is part of the original variables
                x[s.var[i]] = 0;  // Set the corresponding value in 'x' to zero
            }
        }

        // Set values of basic variables (in the basis) according to the solution vector 'b'
        for (i = 0; i < m; i = i + 1) {
            if (s.var[n + i] < n) {  // If the variable index is part of the original variables
                x[s.var[n + i]] = s.b[i];  // Set the value from the solution vector 'b'
            }
        }

        free(s.var);
        s.var = NULL;
    }
    // If h != 0 (this could be used in cases like Phase 1 with artificial variables)
    else {
        // Set all original variables' values to zero
        for (i = 0; i < n; i = i + 1) {
            x[i] = 0;  // Initialize all solution values to zero
        }

        // Set the values of the basic variables (including possible artificial variables)
        for (i = n; i < n + m; i = i + 1) {
            x[i] = s.b[i - n];  // Assign the solution values from 'b'
        }
    }

    // Return the optimal value of the objective function 'y' from the simplex structure
    return s.y;
}

int initial (simplex_t* s, int m, int n, double** a, double* b, double* c, double* x, int y, int* var)
{
    int i,j,k; 
    double w;
    int* t;
    k = init(s, m, n, a, b, c, x, y, var); 
    
    if (b[k] >= 0) {
        return 1; // feasible
    }
    prepare(s, k);
    n = s->n;
    s->y = xsimplex(m, n, s->a, s->b, s->c, s->x, 0, s->var, 1);
    for (i = 0; i < m + n; i = i + 1){
        if(s->var[i] == m+n-1){
            if (abs(s->x[i])>EPS) {
                free(s->x);
                free(s->c);
                s->x = NULL;
                s->c = NULL;
                return 0;
            } else {
                break;
            }
        }
    }
    
    if(i >= n){
        for (j = k = 0; k < n; k = k + 1)
            if( abs(s->a[i-n][k]) > abs(s->a[i-n][j]) )
                j=k;
        pivot(s, i-n, j);
        i=j;
    }
    if(i<n-1){
        printf("i = %d\n", i);
        k = s->var[i];
        s->var[i] = s->var[n-1];
        s->var[n-1] = k;
        
        for (k=0; k<m; k=k+1){
            w = s->a[k][n-1];
            s->a[k][n-1] = s->a[k][i];
            s->a[k][i] = w;
        }
    } else {
        //x_n+m is nonbasic and last, forget it
    }
    s->c = NULL;
    s->c = c;
    s->y = y;
    for (k=n-1; k<n+m-1; k=k+1)
        s->var[k] = s->var[k+1]; n = s->n = s->n - 1;
    t = calloc(n, sizeof(double));
    
    for(k=0; k<n; k=k+1){
        for(j=0; j<n; j=j+1){
            if (k == s->var[j]){
                t[j] = t[j] + s->c[k];
                goto next_k;
            }
        }
        
        for(j=0; j<m; j=j+1)
            if(s->var[n+j] == k)
                break;
        s->y = s->y + s->c[k] * s->b[j];
        for(i=0; i<n; i=i+1)
            t[i] = t[i] - s->c[k] * s->a[j][i];
    next_k:;
    }

    for(i=0; i<n;i=i+1)
        s->c[i] = t[i];
    free(t);
    free(s->x);
    t = NULL;
    s->x = NULL;
    return 1;
} 

double simplex(int m, int n, double** a, double* b, double* c, double* x, double y) 
{
    return xsimplex(m,n,a,b,c,x,y,NULL,0);
}

int print_matrix(double** a, int m, int n, double* array_c, double* array_b) 
{
    printf("max z = %10.1lfx0 %+10.1lfx1\n", array_b[0], array_b[1]);
    for (int c = 0; c < m; c++) {
        printf("%+10.1lfx0%+10.1lfx1 \u2264 %10.1lf", a[c][0], a[c][1], array_c[c]);
        printf("\n");
    }
    return 0;
}

int main(void) 
{
    int m;
    int n;
    double* c;
    double* b;
    double** a;
    double* x;
    double y;

    scanf("%d %d", &m, &n);

    c = calloc(n, sizeof(double));
    b = calloc(m, sizeof(double));

    scanf("%lf %lf", &c[0], &c[1]);
    a = make_matrix(m, n);

    for (int c = 0; c < m; c++) {
        for (int r = 0; r < n; r++) {
            scanf("%lf", &a[c][r]);
        }
    }
    
    scanf("%lf %lf %lf", &b[0], &b[1], &b[2]);
    print_matrix(a, m, n, b, c);
    x = calloc(n+m+1, sizeof(double));
    for (int i=0; i<n; i++)
            x[i] = 0;
    y = 0; 
    printf("Solution: %lf\n",simplex(m, n, a, b, c, x, y));
    
    for (int i = 0; i < m; i++) {
        free(a[i]);
        a[i] = NULL;
    }

    free(b);
    free(c);
    free(a);
    
    return 0;
}


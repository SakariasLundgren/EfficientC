#include <stdio.h>
#include <stdlib.h>
#include <math.h>

const double EPS = 1e-6;

void bp(void){  }

typedef struct list_t list_t;
typedef struct link_t link_t;
typedef struct node_t node_t;
typedef struct simplex_t simplex_t;

int initial(simplex_t* s, int m, int n, double** a, double* b, double* c, double* x, int y, int* var);
double simplex(int m, int n, double** a, double* b, double* c, double* x, double y);

double** make_matrix (int m, int n) 
{
    double** a;
    int i;

    a = calloc(m, sizeof(double*));
    for (i = 0; i < m; i += 1) {
        a[i] = calloc(n, sizeof(double));
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

struct node_t
{
    int m;
    int n;
    int k;
    int h;
    double xh;
    double ak;
    double bk;
    double *min;
    double *max;
    double **a;
    double *b;
    double *x;
    double *c;
    double z;
};

struct simplex_t 
{ 
    int m; /* Constraints. */ 
    int n; /* Decision variables. */
    int *var;/* 0..n  1 are nonbasic. */ 
    double **a; /* A. */ 
    double *b; /* b. */ 
    double *x; /* x. */ 
    double *c; /* c. */ 
    double y; /* y. */ 
};

struct list_t 
{
    link_t* first;
    link_t* last;
};

struct link_t
{
    link_t* next;
    void* data;
};

list_t* new_list(void)
{
    list_t* head;

    head = xmalloc(sizeof(list_t));
    head->first = NULL;
    head->last = NULL;
    return head;
}

static link_t* new_link(void* data)
{
    link_t* link;
    
    link = xmalloc(sizeof(link_t));
    link->next = NULL;
    link->data = data;
    return link;
}

void insert_first(list_t* head, void* data)
{
    link_t* link;
    link = new_link(data);
    if(head->last == NULL){
        head->last = link;
    } else {
        link->next = head->first;
    }
    head->first = link;
}

void insert_last(list_t* head, void* data)
{
    link_t* link;
    link = new_link(data);
    if(head->first == NULL){
        head->first = link;
    } else {
        head->last->next = link;
    }        
    head->last = link;
}

void remove_link_at_index(list_t* head, int index)
{
    if(index < 0) return;

    struct link_t* q = head->first;
    struct link_t* q_prev = NULL;
    int count = 0;

    while(count != index){
        q_prev = q;
        q = q->next;
        count++;
    }

    if(index == 0){ // First element in the list
        head->first = q->next;
    } else if(q->next == NULL) { // Last element in the list
        head->last = q_prev;
        q_prev->next = NULL;
    } else { // Element in the middle of the list
        q_prev->next = q->next;
    }

    free(q);
}

int list_length(list_t* head) {
    int size = 0;
    link_t* q = head->first;

    while (q != NULL) {
        size++;
        q = q->next;
    }

    return size;
}

void* get_data_at_index(list_t* head, int index) {
    int count = 0;
    link_t* q = head->first;

    while (q != NULL) {
        if (count == index) {
            return q->data;
        }
        count++;
        q = q->next;
    }

    return NULL;
}

void free_list(list_t* head) {
    link_t* p;
    link_t* q;

    p = head->first;
    if (head != NULL) {
        free(head);
        head = NULL;
    }

    while(p != NULL) {
        q = p->next;
        if (p != NULL) {
            free(p);
            p = NULL;
        }
        p = q;
    }
}

void free_node(struct node_t *p)
{
    int i;

    if(p == NULL) return;
    
	if (p->b!=NULL){
		free(p->b);
        p->b = NULL;
    }
	if (p->c!=NULL){
		free(p->c);
        p->c = NULL;
  }

	if (p->x!=NULL){
		free(p->x);
        p->x = NULL;
    }
    
    if(p->min != NULL){
        free(p->min);
        p->min = NULL;
    }

    if(p->max != NULL){
        free(p->max);
        p->max = NULL;
    }

    if(p->a != NULL){
        for (i = 0; i < p->m+1; i++) {
            free(p->a[i]);
            p->a[i] = NULL;
        }
    }
    free(p->a);
    p->a = NULL;
    free(p);
}

struct node_t *initial_node(int m, int n, double** a, double* b, double* c)
{
    int i,j;
    struct node_t *p = calloc(1, sizeof(struct node_t));

    p->a = make_matrix(m+1, n+1);
    p->b = calloc(m+1, sizeof(double));
    p->c = calloc(n+1, sizeof(double));
    p->x = calloc(m+n+1, sizeof(double));
    p->min = calloc(n, sizeof(double));
    p->max = calloc(n, sizeof(double));
    p->m = m;
    p->n = n;
    
    // Copy a, b and c parameters to p
    for(i = 0; i < n; i++){
        for(j = 0; j < m; j ++)
            p->a[j][i] = a[j][i]; 

        p->b[i] = b[i];
    }
    for(i = 0; i < n; i++)
        p->c[i] = c[i];

    for(i = 0; i < n; i++) {
        p->min[i] = -INFINITY;
        p->max[i] =  INFINITY;
    }
    return p;
}

struct node_t* extend(struct node_t* p, int m, int n, double **a, double *b, double *c, int k, double ak, double bk)
{
    struct node_t *q = calloc(1, sizeof(struct node_t));
    int i,j;

    q->k = k; 
    q->ak = ak;
    q->bk = bk;

    if(ak > 0 && p->max[k] < INFINITY){
        q->m = p->m;
    } else if (ak < 0 && p->min[k] > 0) {
        q->m = p->m;    
    } else {
        q->m = p->m + 1;
    }
    
    q->n = p->n;
    q->h = -1;

    q->a = make_matrix(q->m + 1, q->n + 1);  // note normally q.m > m
    q->b = calloc(q->m + 1, sizeof(double));
    q->c = calloc(q->n + 1, sizeof(double));
    q->x = calloc(q->n + 1, sizeof(double));
    q->min = calloc(n, sizeof(double));
    q->max = calloc(n, sizeof(double)); 

    for (i = 0; i < m; i++){
        for(j = 0; j < n; j ++){  
            q->a[i][j] = a[i][j];
        }
        q->b[i] = b[i];
    }

    for(i = 0; i < n; i ++){
        q->max[i] = p->max[i];
        q->min[i] = p->min[i];
        q->c[i] = c[i];
    }
    // Copy parameter c to q->c
    if(ak > 0){
        if(q->max[k] == INFINITY || bk < q->max[k]){
            q->max[k] = bk;
        }
    } else if (q->min[k] == -INFINITY || -bk > q->min[k]) {
        q->min[k] = -bk;
    }
     
    for(i = m, j = 0; j < n; j++) {
        if(q->min[j] > -INFINITY) {
            q->a[i][j] = -1;
            q->b[i] = -q->min[j];
            i++;
        }

        if(q->max[j] < INFINITY){
            q->a[i][j] = 1;
            q->b[i] = q->max[j];
            i++;
        }
    }
    return q;
}

int is_integer(double* xp)
{
    // xp is a pointer to a double
    double x = *xp;
    double r = lround(x);  // ISO C lround
    if (fabs(r - x) < EPS){ 
        *xp = r;
        return 1;
    } else {
        return 0;
    }
}

int integer(struct node_t* p)
{
    int i;
    for (i = 0; i < p->n; i++)
        if(!is_integer(&(p->x[i])))
            return 0;
    return 1;
}

void bound(struct node_t* p, struct list_t* h, double* zp, double* x)
{
    int i;
    // zp is a pointer to max z found so far
    if (p->z > *zp){
        *zp = p->z;

        // Copy each element of p->x to z, save the best x
        for(i = 0; i < p->n ;i++){
            x[i] = p->x[i];
        }
        
        // Remove and delete all nodes q in h with q->z < p->z
        int length_h = list_length(h);
        for(i = 0; i < length_h; i++){
            struct node_t* node_check = get_data_at_index(h, i);
            if(node_check->z < p->z){
                remove_link_at_index(h, i);
                free_node(node_check);
                i--;
                length_h--;
            } 
        }
    }    
}

int branch(struct node_t* q, double z)
{
    double min, max;
    int h, i;

    if(q->z < z) 
        return 0;

    for(h = 0; h < q->n; h++){
        if(!is_integer(&q->x[h])){ 
            if(q->min[h] == -INFINITY){ 
                min = 0;
            } else {
                min = q->min[h];
            }
            max = q->max[h];
            if(floor(q->x[h]) < min || ceil(q->x[h]) > max){ 
                continue;
            }
            q->h = h;
            q->xh = q->x[h];
            
            // Delete each of a,b,c,x of q, or recycle in other way? (what)
            if(q->a != NULL){
                for(i = 0; i < q->m+1; i++){
                    free(q->a[i]);
                    q->a[i] = NULL;
                }
                free(q->a);
                q->a = NULL;

            }
            free(q->b);
            free(q->c);
            free(q->x);
            
            q->b = NULL;
            q->c = NULL;
            q->x = NULL;

            return 1;
        }
    }
    return 0;
}

void succ(struct node_t* p, struct list_t* h, int m, int n, double** a, double* b, double* c, int k, double ak, double bk, double* zp, double* x)
{
    struct node_t* q = extend(p, m, n, a, b, c, k, ak, bk);
    if(q == NULL){
        return;
    } 
    q->z = simplex(q->m, q->n, q->a, q->b, q->c, q->x, 0);
    if(isfinite(q->z)){ 
        if(integer(q)){
            bound(q, h, zp, x);
        } else if(branch(q, *zp)){
            // add q to h
            insert_first(h, q);
            return;
        }
    }
    free_node(q);
    q = NULL;
}

double intopt(int m, int n, double** a, double* b, double* c, double* x)
{
    int i, length_h;
    struct node_t* p = initial_node(m, n, a, b, c);
    struct list_t* h = new_list();
    insert_first(h, p); 
    double z = -INFINITY;  // best integer solution found so far 
    p->z = simplex(p->m, p->n, p->a, p->b, p->c, p->x, 0);
    if(integer(p) || !isfinite(p->z)){ 
        z = p->z;
        if(integer(p)){
            // Copy p->x to x
            for(i = 0; i < n; i++)
                x[i] = p->x[i]; 
        }
        free_list(h);
        free_node(p);
        p = NULL;
        h = NULL;
        return z;
    }

    branch(p, z);
    while(length_h = list_length(h) != 0){
        // take p from h; TODO
        p = get_data_at_index(h, length_h - 1);
        remove_link_at_index(h, length_h - 1);
        succ(p, h, m, n, a, b, c, p->h, 1, floor(p->xh), &z, x);
        succ(p, h, m, n, a, b, c, p->h, -1, -ceil(p->xh), &z, x);
        free_node(p);
    }
    free_list(h);
    h = NULL;
    if(z == -INFINITY){ 
        return NAN;  // not-a-number
    } else {
        return z;
    }
}


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
    for (i = 0; i < n; i++) 
        if (i != col) 
            c[i] = c[i] - c[col] * a[row][i] / a[row][col];
    
    // Update the coefficient of the entering variable in the objective function to its new value
    c[col] = - c[col] / a[row][col]; 
    
    // Update the right-hand side vector (constants) for all rows except the pivot row
    for (i = 0; i < m; i++) 
        if (i != row) 
            b[i] = b[i] - a[i][col] * b[row] / a[row][col];

    // Update the constraint matrix for all rows except the pivot row and all columns except the pivot column
    for (i = 0; i < m; i++){
        if (i != row) {
            for (j = 0; j < n; j++){
                if (j != col){ 
                    // Update each non-pivot element based on the pivot element
                    a[i][j] = a[i][j] - a[i][col] * a[row][j] / a[row][col]; 
                }
            }
        }
    }

    // Update the pivot column for all rows except the pivot row
    for (i = 0; i < m; i++) 
        if (i != row) 
            a[i][col] = -a[i][col] / a[row][col]; 

    // Update the pivot row for all columns except the pivot column
    for (i = 0; i < n; i++) 
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
    for (i = 0; i < s->n; i++) {
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
    for(i = m + n; i > n; i--){ 
        s->var[i] = s->var[i-1];  // Shift the variables down by one position
    }
    // Set the variable at index 'n' to the new artificial variable (m + n)
    s->var[n] = m + n;  // Add the new variable in the variable list

    // Increase the number of variables by one
    n = n + 1;  

    // Modify the constraint matrix (tableau) to add the artificial variable
    // Set the coefficients of the new variable (artificial variable) to -1 in each constraint row
    for(i = 0; i < m; i++){ 
        s->a[i][n-1] = -1;  // The last column (new variable's column) gets a value of -1 for each row
    }

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
        for (i = 0; i < m+n; i++){ 
            s->var[i] = i; 
        }
    }
    
    for (k = 0, i = 1; i < m; i++) 
        if (s->b[i] < s->b[k]) 
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
        return NAN;      
    }

    // Main Simplex loop: Perform pivoting as long as there's a non-basic variable that can improve the objective
    while ((col = select_nonbasic(&s)) >= 0) {  // Find the non-basic variable (column) to enter the basis
        row = -1; 

        // Select the pivot row using the minimum ratio test (Bland's rule)
        for (i = 0; i < m; i++) {  
            if ((a[i][col] > EPS) && ((row < 0) || (b[i] / a[i][col]) < (b[row] / a[row][col]))){
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
        for (i = 0; i < n; i++) {
            if (s.var[i] < n) {  // If the variable index is part of the original variables
                x[s.var[i]] = 0;  // Set the corresponding value in 'x' to zero
            }
        }

        // Set values of basic variables (in the basis) according to the solution vector 'b'
        for (i = 0; i < m; i++) {
            if (s.var[n + i] < n) {  // If the variable index is part of the original variables
                x[s.var[n + i]] = s.b[i];  // Set the value from the solution vector 'b'
            }
        }

        free(s.var);
        s.var = NULL;
    } else {
        // Set all original variables' values to zero
        for (i = 0; i < n; i++) {
            x[i] = 0;  // Initialize all solution values to zero
        }

        // Set the values of the basic variables (including possible artificial variables)
        for (i = n; i < n + m; i++) {
            x[i] = s.b[i - n];  // Assign the solution values from 'b'
        }
    }

    // Return the optimal value of the objective function 'y' from the simplex structure
    return s.y;
}

int initial(struct simplex_t* s, int m, int n, double** a, double* b, double* c, double* x, int y, int* var)
{
    int i,j,k; 
    double w;
    double* t;
    k = init(s, m, n, a, b, c, x, y, var); 
    
    if (b[k] >= 0) {
        return 1; // feasible
    }

    prepare(s, k);

    n = s->n;
    s->y = xsimplex(m, n, s->a, s->b, s->c, s->x, 0, s->var, 1);
    
    for (i = 0; i < m+n; i++){
        if(s->var[i] == m+n-1){
            if (fabs(s->x[i])>EPS) {
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
        for (j = k = 0; k < n; k++)
            if( fabs(s->a[i-n][k]) > fabs(s->a[i-n][j]) )
                j=k;
        pivot(s, i-n, j);
        i=j;
    }

    if(i < n-1){
        k = s->var[i];
        s->var[i] = s->var[n-1];
        s->var[n-1] = k;
        
        for (k = 0; k < m; k++){
            w = s->a[k][n-1];
            s->a[k][n-1] = s->a[k][i];
            s->a[k][i] = w;
        }
    } else {
        //x_n+m is nonbasic and last, forget it
    }
    free(s->c);
    s->c = c;
    s->y = y;
    for (k = n-1; k < n+m-1; k++)
        s->var[k] = s->var[k+1]; 

    n = s->n = s->n - 1;
    t = calloc(n, sizeof(double));
    
    for(k = 0; k < n; k++){
        for(j = 0; j < n; j++){
            if (k == s->var[j]){
                t[j] = t[j] + s->c[k];
                goto next_k;
            }
        }
        
        for(j = 0; j < m; j++)
            if(s->var[n+j] == k)
                break;

        s->y = s->y + s->c[k] * s->b[j];
        for(i = 0; i < n; i++)
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
    int m = 0;
    int n = 0;
    double* c;
    double* b;
    double** a;
    double* x;
    double y = 0;

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
    //print_matrix(a, m, n, b, c);
    x = calloc(n+m+1, sizeof(double));
    for (int i=0; i<n; i++)
            x[i] = 0;
    y = 0; 
    printf("Solution: %lf\n",intopt(m, n, a, b, c, x));
    
    for (int i = 0; i < m; i++) {
        free(a[i]);
        a[i] = NULL;
    }

    free(b);
    free(c);
    free(a);
    free(x);

    a = NULL;
    b = NULL;
    c = NULL;
    x = NULL;
    
    return 0;
}


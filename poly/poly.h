#ifndef poly_h
#define poly_h

typedef struct poly_t poly_t;

poly_t*		new_poly_from_string(const char* str_polynom);
void		free_poly(poly_t* p);

poly_t*		mul(poly_t* a, poly_t* b);

void		print_poly(poly_t* p);

#endif

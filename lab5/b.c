#include <stdio.h>
#include <math.h>

#define N	100

double	a[N];
double	s;

int main()
{
	int	i;
	
	s = 0;

	for (i = 0; i < N; i += 1) 
		s += a[i];
}

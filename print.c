#define PRINT		1

static void print(simplex_t* s, int line)
{
#if PRINT
	int	i;
	int	j;
	int	width = 3;

	for (i = 0; i < 80; i += 1)
		putchar('-');
	putchar('\n');
	printf("line %d\n\n", line);
	
	printf("maximize:   ");
	for (i = 0; i < s->n; i += 1)
		printf("%1.*lf x_%d + ", width, s->c[i], s->var[i]);
	printf("%1.*lf\n", width, s->y);
	for (i = 0; i < s->m; i += 1) {
		if (i == 0)
			printf("\nsubject to: ");
		else
			printf("            ");
		printf(" x_%d = %1.*lf -(", s->var[i+s->n], width, s->b[i]);
		for (j = 0; j < s->n; j += 1)
			printf(" %1.*lf x_%d %c", width, s->a[i][j], s->var[j], j < s->n-1?'+' : ')');
		printf("\n");
	}
	printf("\n");
	
#endif
}

// print(&s, __LINE__);

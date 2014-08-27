
v[20];
n 10;
q n;

main() {
	extrn v, q, n;

	v[0] = 12;
	*q = 13;
	printf("%d*n", v[0]);
	printf("%d*n", n);
	printf("%d*n", *q);
	printf("%d*n", v);
	v = 17;
	printf("%d*n", v);
	
	v = &n; *v = 47;
	printf("%d*n", n);
}


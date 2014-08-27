
bfun1() {
	if (0) ;
	while (1) {
		return (6);
	}
}

bfun2(a, b, c)
	a ? b : c;

bfun3(a, b) {
	return ((a + b) * (b - a));
}

bfun4() 2 + 3 * 4 + 5;

bfun5(k) {
	auto j;
	j = 0;
	while (k =- 1) {
		j =+ 2;
	}
	return (j);
}

bfun6(p) { *p = 12; p[1] = 17; }

bfun7(p) *&*++p;

bfun8(p) {
	p++[0]++;
}

bfun9(p, a) {
	p[0][a][2] = 6;
}

bfun10(f, g) {
	f();
	return (g(1, 2, 3));
}

bfun11() {
	printf("hello from b*n");
	printf("goodbye from b*n");
}

bfun12() {
	extrn bfun11;
	auto f;
	f = &bfun11;
	f();
}

bfun13(a, b) {
	return ((((a + b) * (a + b)) + ((a + b) * (a + b))) * (a * (a + b * a)));
}

bfun14(a, b) {
	a + (a + (a + (a + (a + (a + (a + (a + b)))))));
}



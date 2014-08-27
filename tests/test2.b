
eql(a, b) a == b;
neq(a, b) a != b;
_lt(a, b) a < b;
leq(a, b) a <= b;
_gt(a, b) a > b;
geq(a, b) a >= b;

test(a, b, c, d, e, f) {
	printf("%d, %d, %d*n", eql(a, b), eql(c, d), eql(e, f));
	printf("%d, %d, %d*n", neq(a, b), neq(c, d), neq(e, f));
	printf("%d, %d, %d*n", _lt(a, b), _lt(c, d), _lt(e, f));
	printf("%d, %d, %d*n", leq(a, b), leq(c, d), leq(e, f));
	printf("%d, %d, %d*n", _gt(a, b), _gt(c, d), _gt(e, f));
	printf("%d, %d, %d*n", geq(a, b), geq(c, d), geq(e, f));
}

main() {
	test(5, 10, 10, 5, 6, 6);
}


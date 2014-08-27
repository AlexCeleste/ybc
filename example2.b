/* The following program will calculate the constant e-2 to about
   4000 decimal digits, and print it 50 characters to the line in
   groups of 5 characters.  The method is simple output conversion
   of the expansion
     1/2! + 1/3! + ... = .111....
   where the bases of the digits are 2, 3, 4, . . . */

main() {
	extrn putchar, n, v;
	auto i, c, col, a;

	i = col = 0;
	while(i<n)
		v[i++] = 1;
	while(col<2*n) {
		a = n+1 ;
		c = i = 0;
		while (i<n) {
			c =+ v[i] *10;
			v[i++]  = c%a;
			prob:
			c =/ a--;
			endprob:
		}

		putchar(c+'0');
		if(!(++col%5))
			putchar(col%50?' ': '*n');
	}
	printf("%d, %d*n", v[5], i);
	putchar('*n');
}

v[2000];
n 2000;


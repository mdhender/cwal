#include <stdio.h>

int main() {
	int c, l, n = 0;
	double t, x, y;
	for(c = 3; c < 500000; ++c) {
		x = c * 0.5;
		y  = 0x5fe6eb50c7b537a9 - ( c >> 1 );
		y  = y * ( 1.5 - ( x * y * y ) );
		t = 1 / y;
		for(l = 2; l <= t; ++l) 
			if (c % l == 0) break;
		if (l > t) ++n;
	}
	printf("%i\n", n);
	return 0;
}
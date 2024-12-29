/*
EXAMPLE: trig.c

approximation of trig functions using the mclaurin series expansion
*/

float PI = 3.14159;

int factorial(int n) {
  if (n <= 1) {
    return 1;
  }
  return n * factorial(n-1);
}

float sin(float x) {
  if (x < 0.0) {
    return -sin(-x);
  } else if (PI < x && x < 2.0 * PI) {
    return sin(PI-x);
  } else if (x > 2.0 * PI) {
    int deltaPI = (int) (x / PI);
    return sin(x - (PI * (float) deltaPI));
  }

  return x
    - (pow(x, 3.0)/ (float) factorial(3))
    + (pow(x, 5.0)/ (float) factorial(5))
    - (pow(x, 7.0)/ (float) factorial(7));
}

float cos(float x) {
  return sin(PI/2.0 - x);
}

float tan(float x) {
  return sin(x) / cos(x);
}


int main() {
  float x;
  printf("x: ");
  scanf("%lf", &x); // we use %lf because cluck uses doubles underneath floats

  printf("sin(x): %lf\n", sin(x));
  printf("cos(x): %lf\n", cos(x));
  printf("tan(x): %lf\n", tan(x));

  return 0;
}

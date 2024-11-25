/*
EXAMPLE: exp.c

two different implementations of exponentiation
one much more efficient than the other!
*/

bool isEven(int n) {
  return n / 2 * 2 == n;
}

// naive implementation of squaring
int exp_naive(int n, int pow) {
  int prod = 1;
  do {
    prod = prod * n;
    pow = pow - 1;
  } while (pow > 0);
  return prod;
}

// exponentiation by squaring
int exp_by_square(int n, int pow) {
  if (pow == 1) {
    return n;
  }
  int res = exp_by_square(n, pow / 2);
  res = res * res;
  if (!isEven(pow)) {
    res = res * n;
  }

  return res;
}

int main() {
  printint(exp_naive(7, 9));
  printint(exp_by_square(7, 9));
  return 0;
}

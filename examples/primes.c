/*
EXAMPLE: primes.c

an EXTREMELY inefficient prime number generator which
generates the first 10,000 primes
*/

bool isPrime(int n) {
  int i;
  for (i = 2; i < n; i++) {
    if (n / i * i == n) {
      return false;
    } 
  }
  return true;
}

int main() {
  int i;
  int n = 0;

  for (i = 3; n < 10000; i++) {
    if (isPrime(i)) {
      printint(i);
      n++;
    }
  }
  return 0;
}

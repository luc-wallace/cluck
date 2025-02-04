/*
EXAMPLE: primes.c

implementation of the famous sieve of eratosthenes algorithm
using my C dialect
*/

int main() {
  int N;
  printf("Enter N: ");
  scanf("%d", &N);

  if (N < 2) {
    printf("N must be greater than 1\n");
    return 1;
  }

  // allocate aray of N boolean values, all set to false
  bool *A = (bool *)malloc(N * sizeof(bool));

  int i;
  int max = (int)sqrt((float)N);

  for (i = 2; i <= max; i++) {
    if (!A[i]) {
      int j = i * i;
      while (j <= N) {
        A[j] = true; // A[j] is not prime
        j = j + i;
      }
    }
  }

  // find all values in the array still set to false
  for (i = 2; i < N; i++) {
    if (!A[i]) {
      printf("%d ", i);
    }
  }
  printf("\n");

  free(A);

  return 0;
}

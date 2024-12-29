/*
EXAMPLE: merge.c

implementation of merge sort, array is arbitrary length
*/

void merge(int *arr, int left, int mid, int right) {
  int n1 = mid - left + 1;
  int n2 = right - mid;

  // allocate memory for temp arrays
  int *leftArray = (int *)malloc(n1 * sizeof(int));
  int *rightArray = (int *)malloc(n2 * sizeof(int));

  // copy data to temp arrays
  int i;
  for (i = 0; i < n1; i++)
    leftArray[i] = arr[left + i];
  int j;
  for (j = 0; j < n2; j++)
    rightArray[j] = arr[mid + 1 + j];

  // merge temp arrays back into arr
  i = 0;
  j = 0;
  int k = left;
  while (i < n1 && j < n2) {
    if (leftArray[i] <= rightArray[j]) {
      arr[k] = leftArray[i];
      i++;
    }
    else {
      arr[k] = rightArray[j];
      j++;
    }
    k++;
  }

  // copy remaining elements of leftArray, if any
  while (i < n1) {
    arr[k] = leftArray[i];
    i++;
    k++;
  }

  // copy remaining elements of rightArray, if any
  while (j < n2) {
    arr[k] = rightArray[j];
    j++;
    k++;
  }

  free(leftArray);
  free(rightArray);
}

void mergeSort(int *arr, int left, int right) {
  if (left < right) {
    int mid = left + (right - left) / 2;

    // sort first and second halves
    mergeSort(arr, left, mid);
    mergeSort(arr, mid + 1, right);

    // merge the sorted halves
    merge(arr, left, mid, right);
  }
}

int main() {
  int n;
  printf("Enter the number of elements: ");
  scanf("%d", &n);

  int *arr = (int *)malloc(n * sizeof(int));

  printf("Enter the elements: \n");
  int i;
  for (i = 0; i < n; i++) {
    scanf("%d", &arr[i]);
  }

  mergeSort(arr, 0, n - 1);

  printf("Sorted array: \n");
  for (i = 0; i < n; i++) {
    printf("%d ", arr[i]);
  }
  printf("\n");

  free(arr);

  return 0;
}
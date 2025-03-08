/*
EXAMPLE: turing.c

a basic implementation of a turing machine
0 represents a blank, 1 represents a 0 and 2 represents a 1
*/

// output tape state to console
void outputTape(char tape[], int len, int head) {
  int i;

  // print tape
  for (i = 1; i < len; i++) {
    printf("%c ", tape[i]);
  }
  printf("\n");

  // print head
  for (i = 1; i < len; i++) {
    if (i == head) printf("^ "); else printf("  ");
  }
  printf("\n");
}

// turing machine that adds one to a binary number
void runTuringMachine(char tape[]) {
  int len = 0;
  int i;
  // get length of tape - in reality this would be infinite
  for (i = 1; tape[i] != ' '; i++) {
    len++;
  }

  printf("START\n");

  int state = 0;
  int head = len - 1;
  while (head >= 0 && head <= len + 1) {
    outputTape(tape, len, head);
    switch (state) {
      case 0:
        // if cell is a 0, replace it with a 1 and halt
        if (tape[head] == '0') {
          tape[head] = '1';
          state = 1;
        // if cell is a 1, replace it with a 0 and move left
        } else if (tape[head] == '1') {
          tape[head] = '0';
          state = 0;
        }
      case 1: break;
    }
    head--;
  }


  printf("HALT\n");
}

int main() {
  char one[] = {' ', '1', '0', '1', '1', '0', '0', '0', ' '}; // 1 0 1 1 0 0 1
  char two[] = {' ', '0', '1', '1', ' '}; // 1 1 1
  char three[] = {' ', '1', '1', '1', ' '}; // 0 0 0 - overflow

  runTuringMachine(one);
  printf("\n");
  runTuringMachine(two);
  printf("\n");
  runTuringMachine(three);

  return 0;
}

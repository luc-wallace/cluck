/*
EXAMPLE: turing.c

a basic implementation of a turing machine that
adds one to a binary number
*/

// size of tape
int TAPE_SIZE = 8;

// output tape state to console
void outputTape(char tape[], int head) {
  int i;

  // print tape
  for (i = 0; i < TAPE_SIZE; i++) {
    printf("%c ", tape[i]);
  }
  printf("\n");

  // print head
  for (i = 0; i < TAPE_SIZE; i++) {
    if (i == head) printf("^ "); else printf("  ");
  }
  printf("\n");
}

// turing machine that adds one to a binary number
void runTuringMachine(char tape[]) {
  int len = 0;
  int i;

  printf("START\n");

  int state = 0;
  int head = 0;
  while (head >= 0 && head < TAPE_SIZE) {
    outputTape(tape, head);
    switch (state) {
      case 0:
        if (tape[head] == '_') {
          state = 1;
          head--;
        } else {
          state = 0;
          head++;
        }
      case 1:
        // if cell is a 0, replace it with a 1 and halt
        if (tape[head] == '0') {
          tape[head] = '1';
          state = 2;
        // if cell is a 1, replace it with a 0 and move left
        } else if (tape[head] == '1') {
          tape[head] = '0';
          state = 1;
          head--;
        }
      case 2: break;        
    }
  }

  printf("HALT\n");
}

int main() {
  char* tape = (char*) malloc(sizeof(char) * TAPE_SIZE);

  printf("Enter binary number: ");
  scanf("%s", tape);

  int i;
  for (i = 0; i < TAPE_SIZE; i++) { // fill out tape with blanks
    if (tape[i] == '0' || tape[i] == '1') continue;
    tape[i] = '_';
  }

  runTuringMachine(tape);

  return 0;
}

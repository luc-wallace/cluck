/*
EXAMPLE: brainfuck.c

a basic implementation of the brainfuck
programming language which is turing complete
*/

int main(int argc, char* argv[]) {
  if (argc <= 1) {
    printf("No program entered");
  }

  char* program = argv[1];

  // allocate 30000 bytes of memory, all with zeroes
  char memory[30000];
  int i;
  for (i = 0; i < 30000; i++) {
    memory[i] = (char) 0;
  }

  int head = 0;

  // stack to handle loops
  int stack[15000];
  int stack_p = -1;

  int instr_p = 0;
  while (program[instr_p] != '\0') {
    switch (program[instr_p]) {
      case '>':
        head++;
      case '<':
        head--;
      case '+':
        memory[head]++;
      case '-':
        memory[head]--;
      case '[':
        stack_p++;
        stack[stack_p] = instr_p + 1;
      case ']':
        if (memory[head] == (char) 0) {
          stack_p--;
        } else {
          instr_p = stack[stack_p] - 1;
        }
      case ',':
        scanf(" %c", memory + head);
      case '.':
        printf("%c", memory[head]);
    }
    instr_p++;
  }

  return 0;
}

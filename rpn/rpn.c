#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#define N		(10)

// ASCII convertions
#define SPACE 32
#define MUL 42
#define PLUS 43
#define MIN 45
#define DIV 47
#define NEWLINE 10

void bp(void){}

int main(void)
{ 
    // Initialize variables needed
    int stack[N];
    int length_stack = 0; // Tracks stack length
    int input_nbr = 0;
    int line = 1;
    int error_flag = 0;
    int i;

    for(i = 0; i < N; i++){
        stack[i] = 0;
    }

    while((input_nbr = getchar()) != EOF){
        if (line == 14) break;
        if(error_flag){
            if(input_nbr == NEWLINE){
                goto newline;
            }
            continue;
        } else if(input_nbr < 58 && input_nbr > 47){
            input_nbr = input_nbr - 48; //ASCII 48 = 0
            if (length_stack >= 10) {
                printf("line %d: error at %d\n", line, input_nbr);
                error_flag = 1;
                continue;
            }
            stack[length_stack] = input_nbr;
            
            while((input_nbr = getchar()) != EOF) {
                if(input_nbr > 47 && input_nbr < 58) {
                    stack[length_stack] = 10 * stack[length_stack] + input_nbr - 48;
                } else {
                    length_stack++;

                    if(input_nbr == PLUS || input_nbr == MIN || input_nbr == MUL || input_nbr == DIV){
                        goto arith;
                    } else if(input_nbr == NEWLINE) {
                        goto newline;
                    } else if (input_nbr == SPACE) {
                        goto space;
                    } else {
                        goto illegal_input;
                    }
                }
            }
            length_stack++;
            
        } else if(input_nbr == PLUS || input_nbr == MIN || input_nbr == MUL || input_nbr == DIV) {
            arith:;
            if(length_stack < 2){
                if (input_nbr == 43){
                    printf("line %d: error at +\n", line);
                } else {
                    printf("line %d: error at %d\n", line, input_nbr);
                }
                error_flag = 1;
                continue;
            }

            switch (input_nbr) {
                case PLUS:
                    stack[length_stack - 2] = stack[length_stack - 2] + stack[length_stack - 1];
                    stack[length_stack - 1] = 0;
                    break;
                case MIN:
                    stack[length_stack - 2] = stack[length_stack - 2] - stack[length_stack - 1];
                    stack[length_stack - 1] = 0;
                    break;
                case MUL:
                    stack[length_stack - 2] = stack[length_stack - 1] * stack[length_stack - 2];
                    stack[length_stack] = 0;
                    break;
                case DIV:
                    if(stack[length_stack - 1] == 0){
                        printf("line %d: error at /\n", line);
                        error_flag = 1;
                        continue;
                    } else {
                        stack[length_stack - 2] = stack[length_stack - 2] / stack[length_stack - 1];
                        stack[length_stack] = 0;
                    }
                    break;
                default: break;
            }
            length_stack--; 
            
        } else if(input_nbr == SPACE) {
            space:;

        } else if(input_nbr == NEWLINE){
            newline:;
            if(length_stack == 1 && error_flag != 1){
                printf("line %d: %d\n", line, stack[0]);
            } else if(error_flag == 0 && length_stack != 1){
                printf("line %d: error at \\n\n", line);
                error_flag = 0;
            }
            line++;
            input_nbr = 0;

            for(i = 0; i < length_stack; i++) {
                stack[i] = 0;
            };
            length_stack = 0; // Tracks stack length
            error_flag = 0;
        } else {
            illegal_input:;
            // Illegal input. TODO ERROR OUTPUT
            if(input_nbr == 33) {
                printf("line %d: error at !\n", line);
            } else {
                printf("line %d: error at %d\n", line, input_nbr);
            }

            for(i = 0; i < N; i++) {
                stack[i] = 0;
            };
            line++;
            length_stack = 0;
            input_nbr = 0;
            error_flag = 0;
        }
    }

	return 0;
}

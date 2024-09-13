#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define NREG	(32)		/* Number of registers in the processor. */
#define NWORD	(1024)		/* Number of words in the memory. */

/* RT is the destination register,
 * RA is the first source register,
 * RB is the second source register,
 * CONST is the constant
 *
 * Note that an instruction either has RB or CONST, not both.
 *
 * For example, add 2,3,4 means R2 = R3 + R4, 
 * so RT = R2, RA = R3, and RB = R4.
 *
 */
#define ADD	(0)		/* RT = RA + RB			*/
#define ADDI	(1)		/* RT = RA + CONST		*/
#define SUB	(2)		/* RT = RA - RB			*/
#define SUBI	(3)		/* RT = RA - CONST		*/
#define SGE	(4)		/* RT = RA >= RB		*/
#define SGT	(5)		/* RT = RA > RB			*/
#define SEQ	(6)		/* RT = RA == RB		*/
#define BT      (7)		/* if RB then PC = CONST	*/
#define BF      (8)		/* if !RB then PC = CONST	*/
#define BA      (9)		/* PC = CONST			*/
#define ST      (10)		/* MEMORY[ RA + CONST ] = RT	*/
#define LD      (11)  		/* RT = MEMORY [ RA + CONST ]	*/
#define CALL	(12)		/* R31 = PC+1; PC = CONST	*/
#define JMP	(13)		/* PC = RA			*/
#define MUL	(14)		/* RT + RA * RB			*/
#define SEQI	(15)		/* RT = RA == CONST		*/
#define HALT    (16)		/* STOPS THE MACHINE. 		*/

/* You need a C99 compiler for this. */

char*	mnemonics[] = { 
	[ADD] = "add",
	[ADDI] = "addi",
	[SUB] = "sub",
	[SUBI] = "subi",
	[SGE] = "sge",
	[SGT] = "sgt",
	[SEQ] = "seq",
	[SEQI] = "seqi",
	[BT] = "bt",
	[BF] = "bf",
	[BA] = "ba",
	[ST] = "st",
	[LD] = "ld",
	[CALL] = "call",
	[JMP] = "jmp",
	[MUL] = "mul",
	[HALT] = "halt",
};

typedef struct {
	unsigned	pc;		/* Program counter. */
	unsigned 	reg[NREG];	/* Registers. */
} cpu_t;

unsigned make_instr(unsigned opcode, unsigned dest, unsigned s1, unsigned s2)
{
	return (opcode << 26) | (dest << 21) | (s1 << 16) | (s2 & 0xffff);
}

unsigned extract_opcode(unsigned instr)
{
	return instr >> 26;
}

unsigned extract_dest(unsigned instr)
{
	return (instr >> 21) & 0x1f;
}

unsigned extract_source1(unsigned instr)
{
	return (instr >> 16) & 0x1f;
}

signed extract_constant(unsigned instr)
{
	return (short)(instr & 0xffff);
}

void error(char* fmt, ...)
{
	va_list		ap;
	char		buf[BUFSIZ];

	va_start(ap, fmt);
	vsprintf(buf, fmt, ap);
	fprintf(stderr, "error: %s\n", buf);
	exit(1);
}

void read_program(char* file, unsigned memory[], int* ninstr)
{
	FILE*		in;
	int		opcode;
	int		a, b, c;
	int		i;
	char		buf[BUFSIZ];
	char		text[BUFSIZ];
	int		n;
	int		line;

	/* Find out the number of mnemonics. */
	n = sizeof mnemonics / sizeof mnemonics[0];

	in = fopen(file, "r");

	if (in == NULL)
		error("cannot open file");

	line = 0;

	while (fgets(buf, sizeof buf, in) != NULL) {
		if (buf[0] == ';')
			continue;

		if (sscanf(buf, "%s %d,%d,%d", text, &a, &b, &c) != 4)
			error("syntax error near: \"%s\"", buf);

		opcode = -1;

		for (i = 0; i < n; ++i) {
			if (strcmp(text, mnemonics[i]) == 0) {
				opcode = i;
				break;
			}
		}

		if (opcode < 0)
			error("syntax error near: \"%s\"", text);

		memory[line] = make_instr(opcode, a, b, c);

		line += 1;
	} 

	*ninstr = line;
}

int main(int argc, char** argv)
{
	char*		file;
	unsigned	memory[NWORD];
	cpu_t		cpu;
	int		i;
	int		j;
	int		ninstr;
	unsigned	instr;
	unsigned	opcode;
	unsigned	source_reg1;
	int		constant;
	unsigned	dest_reg;
	int		source1;
	int		source2;
	int		dest;
	unsigned	data;
	bool		proceed;
	bool		increment_pc;
	bool		writeback;

	if (argc > 1)
		file = argv[1];	
	else
		file = "a.s";

	read_program(file, memory, &ninstr);

	/* First instruction to execute is at address 0. */
	cpu.pc = 0;

	proceed = true;

	while (proceed) {

		/* Fetch next instruction to execute. */
		instr = memory[cpu.pc];

		/* Decode the instruction. */
		opcode = extract_opcode(instr);
		source_reg1 = extract_source1(instr);
		constant = extract_constant(instr);
		dest_reg = extract_dest(instr);

		/* Fetch operands. */
		source1 = cpu.reg[source_reg1];
		source2 = cpu.reg[constant & (NREG-1)];

		increment_pc = true;
		writeback = true;

		printf("pc = %3d: ", cpu.pc);

		switch (opcode) {
		case ADD:

			puts("ADD");
			dest = source1 + source2;
			break;
			
		case ADDI:
			puts("ADDI");
			dest = source1 + constant;
			break;
			
		case SUB:
			puts("SUB");
			dest = source1 - source2;
			break;
			
		case SUBI:
			puts("SUBI");
			dest = source1 - constant;
			break;
			
		case MUL:
			puts("MUL");
			dest = source1 * source2;
			break;
			
		case SGE:
			puts("SGE");
			dest = source1 >= source2;
			break;
			
		case SGT:
			puts("SGT");
			dest = source1 > source2;
			break;
			
		case SEQ:
			puts("SEQ");
			dest = source1 == source2;
			break;
			
		case SEQI:
			puts("SEQI");
			dest = source1 == constant;
			break;
			
		case BT:
			puts("BT");
			writeback = false;
			if (source1 != 0) {
				cpu.pc = constant;
				increment_pc = false;
			}
			break;
				
		case BF:
			puts("BF");
			writeback = false;
			if (source1 == 0) {
				cpu.pc = constant;
				increment_pc = false;
			}
			break;
				
		case BA:
			puts("BA");
			writeback = false;
			increment_pc = false;
			cpu.pc = constant;
			break;

		case LD:
			puts("LD");
			data = memory[source1 + constant];
			dest = data;
			break;

		case ST:
			puts("ST");
			data = cpu.reg[dest_reg];
			memory[source1 + constant] = data;
			writeback = false;
			break;

		case CALL:
			puts("CALL");
			increment_pc = false;
			dest = cpu.pc + 1;
			dest_reg = 31;
			cpu.pc = constant;
			break;

		case JMP:
			puts("JMP");
			increment_pc = false;
			writeback = false;
			cpu.pc = source1;
			break;

		case HALT:
			puts("HALT");
			increment_pc = false;
			writeback = false;
			proceed = false;
			break;
		
		default:
			error("illegal instruction at pc = %d: opcode = %d\n", 
				cpu.pc, opcode);
		}
			
		if (writeback)
			cpu.reg[dest_reg] = dest;

		if (increment_pc)
			cpu.pc += 1;

#if 0
		// print out contents of entire memory.

		for (i = 0; i < ninstr; ++i)
			printf("%d: %x\n", i, memory[i]);

		for (i = 1023; i >= cpu.reg[1]; --i)
			printf("m[%4d] = %d\n", i, memory[i]);
#endif

		i = 0;
		while (i < NREG) {
			for (j = 0; j < 4; ++j, ++i) {
				if (j > 0)
					printf("| ");
				printf("R%02d = %-12d", i, cpu.reg[i]);
			}
			printf("\n"); 
		}
	}
	return 0;
}

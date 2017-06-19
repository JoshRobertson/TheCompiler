/* Filename: scanner.c
* Compiler: MS Visual Studio 2013
* Author: Joshua Robertson
* Course: CST8152 - Compilers, Lab Section 011
* Assignment: 2
* Date: October 27, 2015
* Professor: Sv. Ranev
* Purpose: Functions implementing a Lexical Analyzer (Scanner)
*    as required for CST8152, Assignment #2
* Version: 1.15.02
* Date: 29 September 2015
* Provided by: Svillen Ranev
* Function List: scanner_init(),
*				 mlwpar_next_token(),
*				 get_next_state(),
*				 char_class(),
*				 aa_func02(),
*				 aa_func03(),
*				 aa_func05(),
*				 aa_func08(),
*				 aa_func10(),
*				 aa_func12(),
*				 atool(),
*				 iskeyword()
*/

#define _CRT_SECURE_NO_WARNINGS
#pragma warning( push)
#pragma warning( disable : 4001) /*disable single line comment errors from standard files*/
#include <ctype.h>   /* conversion functions */
#include <string.h>  /* string functions */
#include <float.h>   /* floating-point types constants */
#include <errno.h>	 /* for error checking using strtol*/
/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */
#pragma warning( pop)

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"
#include "stable.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/*Columns*/
#define COL0 0
#define COL1 1
#define COL2 2
#define COL3 3
#define COL4 4
#define COL5 5
#define COL6 6
#define PLAT_MAX 32767

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */
extern STD sym_table;
/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */
static long atool(char * lexeme); /* converts octal string to decimal value */

/*Purpose: Initialize the scanner
Author: Joshua Robertson
History: 1.0 October 27, 2015
Called functions: b_isempty(), b_setmark(), b_retract_to_mark(), b_reset()
Parameters: Buffer* sc_buf
Return value: int 0 for exit success
Algorithm: ensure buffer isn't empty, reset all buffer flags and offsets, start line count
*/
int scanner_init(Buffer * sc_buf) {
	if (b_isempty(sc_buf)) return EXIT_FAILURE;	/*1*/
	/* in case the buffer has been read previously  */
	b_setmark(sc_buf, 0);
	b_retract_to_mark(sc_buf);
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
	/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*Purpose: Read in stream from buffer one symbol at a time, and match lexeme with a pattern.
Returns token anytime token pattern is recognized.
Ignores white space or comments beginning with prefix !<
Author: Joshua Robertson
History: 1.0 October 27, 2015
Called functions: memset(), strncpy()
Parameters: Buffer* sc_buf
Return value: Token for current lexeme or case being scanned
Algorithm: read in input buffer in endless loop, parse input for pattern matching case driven tokens. if found, return proper token.
If not, enter the state machine to determine a lexeme. Pass lexeme to appropriate accepting funtion to return proper token. if invalid input,
return error token.
*/
Token mlwpar_next_token(Buffer * sc_buf)
{
	Token t;			/* token to return after recognition */
	unsigned char c;	/* input symbol */
	int state = 0;		/* initial state of the FSM */
	short lexstart;		/* start offset of a lexeme in the input buffer */
	short lexend;		/* end offset of a lexeme in the input buffer */
	int accept = NOAS;	/* type of state - initially not accepting */


	/*DECLARE YOUR VARIABLES HERE IF NEEDED*/
	int i = 0;	/* generic counter variable*/
	lexstart = 0;
	/*set end of lexeme to getc position in the buffer*/
	lexend = b_getc_offset(sc_buf); 

	while (1){ /* endless loop broken by token returns it will generate a warning */

		/* get the next symbol from the input buffer*/
		c = b_getc(sc_buf);
		/*set lexstart to ending point of previous lex*/
		lexstart = lexend; 
		/*get newly incremented getc offset after each getc call*/
		lexend = b_getc_offset(sc_buf); 

		/*set mark to start of lexeme, set string table mark to addc position, and check for run time errors*/
		if ((int)b_setmark(sc_buf, lexstart) == R_FAIL_1 || (int)b_setmark(str_LTBL, b_size(str_LTBL)) == R_FAIL_1)
		{
			t.code = ERR_T;
			strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
			scerrnum = 2;
			return t;
		}
		
		switch (c)
		{
			/* '\0' or only one of the folowing constants: 255, 0xFF , EOF*/
		case '\0': case UCHAR_MAX: case EOF:
			t.code = SEOF_T;
			return t;

			/* white space*//* tab *//* form feed *//* vertical tab */
		case ' ': case '\t': case '\f': case '\v': continue;

			/* line feed and CRLF */
		case '\n': line++; continue;

			/* left parenthesis */
		case '{': t.code = LBR_T; return t;

			/* right parenthesis */
		case '}': t.code = RBR_T; return t;

			/* left bracket */
		case '(': t.code = LPR_T; return t;

			/* right bracket */
		case ')': t.code = RPR_T; return t;

			/* comma */
		case ',': t.code = COM_T; return t;

			/* end of statement */
		case ';': t.code = EOS_T; return t;

			/* concatentation*/
		case '#': t.code = SCC_OP_T; return t;

			/* addition */
		case '+': t.code = ART_OP_T; t.attribute.arr_op = PLUS; return t;

			/* subtraction */
		case '-': t.code = ART_OP_T; t.attribute.arr_op = MINUS; return t;

			/* multiplication */
		case '*': t.code = ART_OP_T; t.attribute.arr_op = MULT; return t;

			/* division */
		case '/': t.code = ART_OP_T; t.attribute.arr_op = DIV; return t;

			/* greater then */
		case '>': t.code = REL_OP_T; t.attribute.rel_op = GT; return t;

			/* less than*/
		case '<':
			t.code = REL_OP_T;
			
			/* not equals operator*/
			if (b_getc(sc_buf) == '>')	
			{
				t.attribute.rel_op = NE;
			}

			/*just LT operator*/
			else						
			{
				b_retract(sc_buf); /*return buffer to original position*/
				t.attribute.rel_op = LT;
			}

			return t;

			/*assignment or comparison*/
		case '=':
			/*comparision*/
			if (b_getc(sc_buf) == '=') 
			{
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
			}

			/*assignment*/
			else
			{
				b_retract(sc_buf); /*return buffer to original position*/
				t.code = ASS_OP_T;
			}
			return t;

			/*logical .AND. or .OR.*/
		case '.':
			if (b_getc(sc_buf) == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.')
			{
				/* logical and */
				t.code = LOG_OP_T;
				t.attribute.log_op = AND;
				return t;
			}
			else
			{
				b_retract(sc_buf);
				if (b_getc(sc_buf) == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.')
				{
					/* logical or */
					t.code = LOG_OP_T;
					t.attribute.log_op = OR;
					return t;
				}
				else
				{
					/* illegal symbol, retract back to beginning of lexeme  */
					b_retract_to_mark(sc_buf);
					t.code = ERR_T;
					t.attribute.err_lex[0] = b_getc(sc_buf);
					t.attribute.err_lex[1] = '\0';
					return t;
				}
			}

			/*!<comment*/
		case '!' :
			/*valid comment*/
			if (b_getc(sc_buf) == '<') 
			{
				/*pull out all chars from comment marker to end of line or carriage return*/
				while (c != '\r' && c != '\n' && c != '\0' && c != UCHAR_MAX) 
					c = b_getc(sc_buf);
				b_retract(sc_buf);
				continue;
			}

			/*otherwise illegal comment. generate error token and ignore rest of line*/
			else
			{
				b_retract(sc_buf);
				t.code = ERR_T;
				/*store ! with first incorrect char in token attribute*/
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = b_getc(sc_buf); 
				t.attribute.err_lex[2] = '\0';
				
				/*pull out all chars from comment marker to end of line or carriage return*/
				while (c != '\r' && c != '\n' && c != '\0' && c != UCHAR_MAX)
					c = b_getc(sc_buf);

				/*retract once when end of line is hit*/
				if (c == '\r' || c == '\n')
					b_retract(sc_buf);

				return t;
			}

			/* string */
		case '"':
			c = b_getc(sc_buf);
			
			/*loop until end of string is found*/
			while (c != '"' && c != '\0' && c != UCHAR_MAX) 
			{
				if (c == '\n')
					line++; /*count line numbers*/
				c = b_getc(sc_buf);
			}
			
			/*EOF reached, but string not terminated so its illegal*/
			if (c == '\0' || c == EOF || c == UCHAR_MAX)
			{
				t.code = ERR_T;
				lexend = b_getc_offset(sc_buf);
				/*if unterminated string, retract to start of string*/
				b_retract_to_mark(sc_buf); 

				for (i = (int)b_mark(sc_buf); i < lexend; i++)
				{					
					/*store c in err_lex. Stop if hits end of string before ERR_LEN chars*/
					if ((t.attribute.err_lex[i - (int)b_mark(sc_buf)] = b_getc(sc_buf)) == '\0') 
						break;
					
					/*if illegal string is longer than 20 char, truncate and append ...*/
					if ((b_getc_offset(sc_buf) - b_mark(sc_buf)) >= ERR_LEN) 
					{
						t.attribute.err_lex[ERR_LEN - 3] = '.';
						t.attribute.err_lex[ERR_LEN - 2] = '.';
						t.attribute.err_lex[ERR_LEN - 1] = '.';
						t.attribute.err_lex[ERR_LEN] = '\0';
						break;
					}
				}

				/*set mark to end of lexeme, and check for null runtime error*/
				if((int)b_setmark(sc_buf, lexend) == R_FAIL_1)
				{
					t.code = ERR_T;
					strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
					scerrnum = 4;
					return t;
				}

				/*retract getc_offset forward to end of lexeme*/
				b_retract_to_mark(sc_buf);
				return t;
			}

			/*else valid string*/
			t.code = STR_T;

			/*mark end of string*/
			lexend = b_getc_offset(sc_buf);
			/*go back to the begining to copy into string table*/
			b_retract_to_mark(sc_buf);

			b_getc(sc_buf); /*throw away opening "*/
			for (i = (int)b_mark(sc_buf)+1; i < lexend-1; i++)
			{
				/*add c to string table, check for null runtime error*/
				if(b_addc(str_LTBL, b_getc(sc_buf)) == NULL)
				{
					t.code = ERR_T;
					strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
					scerrnum = 6;
					return t;
				}
			}
			b_getc(sc_buf); /*throw away closing "*/
			
			/*attribute is starting position of lexeme in string table*/
			t.attribute.str_offset = b_mark(str_LTBL); 
			
			/*append final \0 for string in literal table and check for null runtime error*/
			if (b_addc(str_LTBL, '\0') == NULL) 
			{
				t.code = ERR_T;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				scerrnum = 8;
				return t;
			}
			return t;

			/*match using transition table*/
		default:
			/* If (c is a digit OR c is a letter)*/
			if ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
			{				
				
				state = get_next_state(state, c, &accept);

				/*while not accepting state, get next state and get char*/
				while (accept == NOAS)
				{
					c = b_getc(sc_buf);
					state = get_next_state(state, c, &accept);
				}
				
				/* retract if state is ASWR*/
				if (accept == ASWR)
				{
					b_retract(sc_buf); 
				}

				/*mark start of lexeme*/
				lexstart = b_mark(sc_buf);
				/*mark end of lexeme*/
				lexend = b_getc_offset(sc_buf);
				/*create temp buffer for size of lexeme*/
				if ((lex_buf = b_create((lexend - lexstart), INC_RNG_MIN, 'a')) == NULL){
					t.code = ERR_T;
					strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
					scerrnum = 10;
					return t;
				};
				/*retract getc_offset to start of lexeme*/
				b_retract_to_mark(sc_buf);

				while (lexstart < lexend)
				{
					/*add c from input buffer into temp buffer, and check for null runtime error*/
					if (b_addc(lex_buf, b_getc(sc_buf)) == NULL)
					{
						t.code = ERR_T;
						strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
						scerrnum = 12;
						return t;
					}
					lexstart++;
				}

				/*append \0 into temp buf to ensure proper C string creation, check for null runtime error*/
				if (b_addc(lex_buf, '\0') == NULL)
				{
					t.code = ERR_T;
					strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
					scerrnum = 14;
					return t;
				}

				/*call acception function for the proper state. pass in the lex_buf array from the first char*/
				t = aa_table[state](b_setmark(lex_buf, BEGINNING));
				b_destroy(lex_buf);
				return t;
			}

			/*error if input is non letter/digit*/
			else
			{
				t.code = ERR_T;
				t.attribute.err_lex[0] = c;
				t.attribute.err_lex[1] = '\0';
				return t;
			}
		}
	}/*end while(1)*/
}

/*Purpose: gets the next state for the scanner to transition to
Author: Joshua Robertson
History: 1.0 October 27, 2015
Called functions: memset(), strncpy()
Parameters: int state as current state, char c as current symbol, and int *accept as pointer to index of next state
Return value: int next as index of next state from as_table
Provided completed by: Sv. Ranev
Algorithm: take in current symbol c and current state, get the class for c, and search st_table for next state.
Assert next state isn't Illegal State.
*/
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	assert(next != IS);
#ifdef DEBUG
	if (next == IS){
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/*Purpose:
Author: Joshua Robertson
History: 1.0 October 27, 2015
Called functions: none
Parameters: char c
Return value: int val as column corresponding to c's class in transition table
Algorithm: check which column c's value corresponds with, and return the index of that column.
*/
int char_class(char c)
{
	int val;

	if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
		val = COL0;
	else if (c == '0')
		val = COL1;
	else if (c >= '1' && c <= '7')
		val = COL2;
	else if (c == '8' || c == '9')
		val = COL3;
	else if (c == '.')
		val = COL4;
	else if (c == '%')
		val = COL5;
	else
		val = COL6;

	return val;
}

/*HERE YOU WRITE THE DEFINITIONS FOR YOUR ACCEPTING FUNCTIONS.
************************************************************

/*Purpose: Accepting function for the arithmetic variable identifier (VID -AVID/KW)
Author: Joshua Robertson
History: 1.0 October 27, 2015
Called functions: memset(), strncpy(), iskeyword()
Parameters: char lexeme[]
Return value: SVID Token or Error Token
Algorithm: check if lexeme is keyword. If yes, set token as KW, assign kw index and return token.
Else, set token as AVID, copy lexeme (up to 8 characters) into vid_lex, return token.
*/
Token aa_func02(char lexeme[])
{
	Token t;
	/*check if lexeme is a keyword*/
	t.attribute.kwt_idx = iskeyword(lexeme);

	if (t.attribute.kwt_idx > -1)
	{
		t.code = KW_T;
		return t;
	}

	t.code = AVID_T;

	if (strlen(lexeme) > VID_LEN)
	{
		lexeme[VID_LEN] = '\0';
	}

	/* a type parameter F or I depending on the default type of the arithmetic variable identifier*/
	switch (lexeme[0]){
		/*As per Platypus specs, AVIDS starting with i, o, d, or w are integers*/
	case 'i': case 'o': case 'd': case 'w':
		/*install AVID in symbol table and store position offset in token attribute*/
		if ((t.attribute.vid_offset = st_install(sym_table, lexeme, 'I', line)) == -1)
		{
			printf("\nError: The Symbol Table is full - install failed.\n\n");
			st_store(sym_table);
			exit(1);
		}
		break;
	default:
		/*everything else is a float by default*/
		if ((t.attribute.vid_offset = st_install(sym_table, lexeme, 'F', line)) == -1)
		{
			printf("\nError: The Symbol Table is full - install failed.\n\n");
			st_store(sym_table);
			exit(1);
		}
	}

	return t;
}

/*Purpose: Accepting function for the string variable identifier (VID -SVID)
Author: Joshua Robertson
History: 1.0 October 27, 2015
Called functions: memset(), strncpy()
Parameters: char lexeme[]
Return value: SVID Token or Error Token
Algorithm: set token code, copy lexeme into vid_lex, append %, return token
*/
Token aa_func03(char lexeme[])
{
	Token t;
	t.code = SVID_T;

	if (strlen(lexeme) > VID_LEN)
	{
		/*if (lexeme[strlen(lexeme) - 1] == '%')*/
		lexeme[VID_LEN - 1] = '%';
		lexeme[VID_LEN] = '\0';
	}

	/*install SVID in symbol table and store position offset in token attribute*/
	if ((t.attribute.vid_offset = st_install(sym_table, lexeme, 'S', line)) == -1)
	{
		printf("\nError: The Symbol Table is full - install failed.\n\n");
		st_store(sym_table);
		exit(1);
	}

	return t;
}

/*Purpose: Accepting function for the integer literal(IL) - decimal constant (DIL) and zero (0)
Author: Joshua Robertson
History: 1.0 October 27, 2015
Called functions: memset(), strncpy(), strtol()
Parameters: char lexeme[]
Return value: IL Token or Error Token
Algorithm: convert lexeme to long and validate. If fail, return error token, else cast lex to int and return FPL token
*/
Token aa_func05(char lexeme[])
{
	Token t;
	/*convert lexeme to int*/
	long ilex = strtol(lexeme, NULL, 10);

	/*check errno for valid conversion success, and validate target range limits */
	if (errno == ERANGE || ilex > PLAT_MAX) 
	{
		t.code = ERR_T;
		/*fill attribute with \0s and copy lexeme in appropriate attribute member*/
		memset(t.attribute.err_lex, 0, ERR_LEN + 1);
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN);
	}
	else
	{
		t.code = INL_T;
		/*cast to short once range is validated*/
		t.attribute.int_value = (int)ilex;
	}

	return t;
}

/*Purpose: Accepting function for the floating-point literal
Author: Joshua Robertson
History: 1.0 October 27, 2015
Called functions: memset(), strncpy(), strtod()
Parameters: char lexeme[]
Return value: FPL Token or Error Token
Algorithm: convert lexeme to double and validate. If fail, return error token, else cast lex to float and return FPL token
*/
Token aa_func08(char lexeme[])
{
	Token t;
	/*convert lexeme to double*/
	double dlex = strtod(lexeme, NULL);

	/*check errno for valid conversion success, and validate target range limits*/
	if (errno == ERANGE || dlex > FLT_MAX || (dlex != 0.0 && dlex < FLT_MIN))
	{
		t.code = ERR_T;
		/*fill attribute with \0s and copy lexeme in appropriate attribute member*/
		memset(t.attribute.err_lex, 0, ERR_LEN + 1);
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN);
	}
	else
	{
		t.code = FPL_T;
		/*cast to float once range is validated*/
		t.attribute.flt_value = (float)dlex;
	}

	return t;
}

/*Purpose: Accepting function for the integer literal (IL) - octal constant (OIL)
Author: Joshua Robertson
History: 1.0 October 27, 2015
Called functions: atool(), memset(), strncpy()
Parameters: char lexeme[]
Return value: IL token or Error Token
Algorithm: convert lexeme to long, and validate. If fail, return error token, else cast lex to int and return IL token
*/
Token aa_func10(char lexeme[])
{
	Token t;
	/*convert lexeme to long*/
	long llex = atool(lexeme);

	/*check errno for valid conversion success, and validate target range limits*/
	if (errno == ERANGE || llex > PLAT_MAX)
	{
		t.code = ERR_T;
		/*fill attribute with \0s and copy lexeme in appropriate attribute member*/
		memset(t.attribute.err_lex, 0, ERR_LEN + 1);
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN);
	}
	else
	{
		t.code = INL_T;
		/*cast to int once range is validated*/
		t.attribute.int_value = (int)llex;
	}

	return t;
}

/*Purpose: Accepting function for the error token
Author: Joshua Robertson
History: 1.0 October 27, 2015
Called functions: memset(), strncpy()
Parameters: char lexeme[]
Return value: Error Token
Algorithm: create error token, copy lexeme into err_lex attribute and return the token
*/
Token aa_func12(char lexeme[])
{
	Token t;
	t.code = ERR_T;

	/*fill attribute with \0s and copy lexeme in appropriate attribute member*/
	memset(t.attribute.err_lex, 0, ERR_LEN + 1);
	strncpy(t.attribute.err_lex, lexeme, ERR_LEN);

	return t;
}

/*Purpose: convert a string to a long
Author: Joshua Robertson
History: 1.0 October 27, 2015
Called functions: strtol()
Parameters: char * lexeme
Return value: long value of converted string, or 0L if conversion fails
Algorithm: convert lexeme to long data type and return new long value. If conversion
cannot happen, return 0L. If conversion is out of range, snap to limits
*/
long atool(char * lexeme)
{
	/*convert lexeme to base 8*/
	return strtol(lexeme, NULL, 8);
}

/*Purpose: Check if a lexeme is a reserved keyword
Author: Joshua Robertson
History: 1.0 October 27, 2015
Called functions: strcmp()
Parameters: char * kw_lexeme
Return value: int index in keyword array if lexeme is keyword, or -1 if not
Algorithm: compare lexeme to each index in kw_table array. if match is found, return the index
*/
int iskeyword(char * kw_lexeme)
{
	int i;

	for (i = 0; i < KWT_SIZE; i++){
		if (strcmp(kw_table[i], kw_lexeme) == 0) /*check if parameter matches keyword table*/
			return i; /*if yes, return kwt column number*/
	}

	return -1; /*else not a keyword*/
}


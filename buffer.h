/**********************************************
File Name: buffer.h
Compiler: MS Visual Studio 2013
Author: Joshua Robertson 040775156
Course: CST8152 - Compilers, Lab Section: 011
Assignment: #1
Date: September 29th, 2015
Professor: Sv. Ranev
Purpose: Preprocessor directives, type declarations and prototypes necessary for buffer implementation 
		as required for CST8152, Assignment #1, Fall 2015.
Function List:
b_create();
b_addc();
b_reset();
b_destroy();
b_isfull();
b_size();
b_capacity();
b_setmark();
b_mark();
b_mode();
b_inc_factor();
b_load();
b_isempty();
b_eob();
b_getc();
b_print();
b_pack();
b_rflag();
b_retract();
b_retract_to_mark();
b_getc_offset();
**********************************************/
#ifndef BUFFER_H_
#define BUFFER_H_

/* standard header files */
#pragma warning( push)
#pragma warning( disable : 4001) /*disable single line comment errors from standard files*/
#include <stdlib.h>
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */
#pragma warning( pop)

/* constant definitions */
/* You may add your own constant definitions here */
#define R_FAIL_1 -1				/* fail return value */
#define R_FAIL_2 -2				/* fail return value */
#define LOAD_FAIL -2			/* load fail error */
#define SET_R_FLAG 1			/* realloc flag set value */
#define INC_FACTOR_ERR 256		/* increment factor error*/
#define NO_INC 0				/* no increment*/
#define INC_RNG_MIN 1			/* minimum increment*/
#define INC_RNG_MAX_A UCHAR_MAX	/* maximum increment (mode A)*/
#define INC_RNG_MAX_M 100		/* maximum increment (mode M)*/
#define MODE_F 0				/* Fixed-sze mode*/
#define MODE_A 1				/* Additive self-incrementing mode*/
#define MODE_M -1				/* Multiplicative self-incrementing mode*/
#define R_FLAG_NO 0				/* Array not realloc'ed*/
#define R_FLAG_YES 1			/* Array realloc'ed*/
#define NEW_SIZE 0				/* Initializer*/
#define FULL 1					/* Buffer is full*/
#define NOT_FULL 0				/* Buffer is not full*/
#define END 1					/* End of File reached*/
#define NOT_END 0				/* End of file not reached*/
#define BEGINNING 0				/* Initial position*/

/* user data type declarations */
typedef struct BufferDescriptor {
	char *cb_head;   /* pointer to the beginning of character array (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short mark_offset; /* the offset (in chars) to the mark location */
	char  inc_factor; /* character array increment factor */
	char  r_flag;     /* character array reallocation flag */
	char  mode;       /* operational mode indicator*/
	int   eob;       /* end-of-buffer reached flag */
} Buffer, *pBuffer;
/*typedef Buffer *pBuffer;*/

/* function declarations */
Buffer *b_create(short init_capacity, char inc_factor, char o_mode);

pBuffer b_addc(pBuffer const pBD, char symbol);

int b_reset(Buffer *const pBD);

void b_destroy(Buffer *const pBD);

int b_isfull(Buffer *const pBD);

short b_size(Buffer *const pBD);

short b_capacity(Buffer *const pBD);

char* b_setmark(Buffer *const pBD, short mark);

short b_mark(Buffer *const pBD);

int b_mode(Buffer *const pBD);

size_t b_inc_factor(Buffer *const pBD);

int b_load(FILE *const fi, Buffer *const pBD);

int b_isempty(Buffer *const pBD);

int b_eob(Buffer *const pBD);

char b_getc(Buffer *const pBD);

int b_print(Buffer *const pBD);

Buffer *b_pack(Buffer *const pBD);

char b_rflag(Buffer *const pBD);

short b_retract(Buffer *const pBD);

short b_retract_to_mark(Buffer *const pBD);

short b_getc_offset(Buffer *const pBD);

#endif

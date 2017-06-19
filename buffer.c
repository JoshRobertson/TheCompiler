/**********************************************
File Name: buffer.c
Compiler: MS Visual Studio 2013
Author: Joshua Robertson 040775156
Course: CST8152 - Compilers, Lab Section: 011
Assignment: #1
Date: September 29th, 2015
Professor: Sv. Ranev
Purpose: A simple buffer application that reads in a source file into a buffer,
		and outputs the buffer contents to a file. Buffer can be run in fixed, additive,
		or multiplicative mode to determine size and behaviour.
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

#include "buffer.h"
/*
Purpose: Creates a new buffer structure
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: calloc(), free(), malloc()
Parameters: short init_capacity (range 0 - SHRT_MAX)
			char inc_factor (range 0 - 100 in multiplicative mode, 0 - 255 in additive mode )
			char o_mode ('a', 'f', 'm' only)
Return value: Buffer* (or NULL on error)
Algorithm: check for valid parameters, return null if fail.
			calloc space for one Buffer struct, return null if fail.
			malloc space for char array buffer, free pointer and return null if fail.
			based on o_mode selection, set mode and inc_factor, free buffer and struct and return null if fail.
			Set capactity in buffer, and zero remaining fields.		
*/
Buffer* b_create(short init_capacity, char inc_factor, char o_mode)
{
	Buffer* pBD = NULL; /* the buffer, initialized to NULL to start*/
	
	/*check for valid parameter*/
	if (init_capacity < 0 || (inc_factor < 0 && o_mode == MODE_M))
		return NULL;

	/*calloc space for buffer*/
	pBD = calloc(1, sizeof(Buffer));
	if (!pBD)
		return NULL;

	/* allocate initial capacity of the buffer*/
	pBD->cb_head = (char*)malloc(init_capacity * sizeof(char));
	if (!pBD->cb_head)
	{
		free(pBD);
		return NULL;
	}

	/* Fixed-size mode or increment is 0. No increment implies fixed size */
	if (o_mode == 'f' || (unsigned char)inc_factor == NO_INC)
	{
		if (init_capacity == 0) /*in fixed mode, break if capacity is 0*/
			return NULL;
		pBD->mode = MODE_F;
		pBD->inc_factor = NO_INC;
	}

	/* Additive mode uses range of valid char values (1-255)*/
	else if (o_mode == 'a' && (unsigned char)inc_factor >= INC_RNG_MIN && (unsigned char)inc_factor <= INC_RNG_MAX_A)
	{
		pBD->mode = MODE_A;
		pBD->inc_factor = inc_factor;
	}
	
	/* Multiplicative mode uses range from 1-100 */
	else if (o_mode == 'm' && (unsigned char)inc_factor >= INC_RNG_MIN && (unsigned char)inc_factor <= INC_RNG_MAX_M)
	{
		pBD->mode = MODE_M;
		pBD->inc_factor = inc_factor;
	}
	
	/* else is catch-all error case*/
	else
	{
		free(pBD->cb_head);
		free(pBD);
		return NULL;
	}

	/* set capacity*/
	pBD->capacity = init_capacity;

	return pBD;
}

/*
Purpose: Add a character to the buffer, expand it if needed
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: realloc()
Parameters: pBuffer pBD
			char symbol
Return value: pBuffer (or NULL on error)
Algorithm: check that pBD exists, return null on error.
			check if there is room to add char to buffer; if yes, add char and increment offset marker.
			if not, check mode to see if/how buffer can be expanded. Return NULL if can't expand.
			calculate new size based on mode selected, and ranges enforced.
			realloc with new size, check if still no room to add char.
			update buffer and add char if possible.
*/
pBuffer b_addc(pBuffer const pBD, char symbol)
{
	/* local variables */
	short new_size = NEW_SIZE;	/* new_size is used to calculate the new size of the buffer if required */
	char *new_buffer = NULL;	/* a pointer to the re-allocated buffer */

	if (!pBD)
		return NULL;

	/* reset reallocation flag */
	pBD->r_flag = R_FLAG_NO;

	/* check if there is room to add a character */
	if ((short)pBD->addc_offset < pBD->capacity)	
		pBD->cb_head[pBD->addc_offset++] = symbol;/*add new char at offset position, increment offset after*/

	else /* No room left in array, need to reallocate*/
	{
		
		if (pBD->mode == MODE_F)
			return NULL;	/* fixed buffer; don't reallocate */
		else if (pBD->mode == MODE_A)																								
		{
			/* additive mode; increment capacity by inc_factor */
			new_size = pBD->capacity + (short)((unsigned char)pBD->inc_factor);

			/* if new size is negative return NULL (error), or if MAX return NULL to stop reading*/
			if (new_size < 0 || new_size == SHRT_MAX)																			
				return NULL;
		}
		else if (pBD->mode == MODE_M)
		{
			if (pBD->capacity == SHRT_MAX) /*if capacity is at MAX, cannot increment any more*/
				return NULL;

			/* multiplicative mode; increment by percentage of remaining available capacity */
			new_size = pBD->capacity + (short)((SHRT_MAX - pBD->capacity) * ((unsigned char)pBD->inc_factor / 100.0));

			/* if new size is negative and the old size wasn't SHRT_MAX then the new_size is SHRT_MAX */
			/* or if new size and capacity are equal, set capacity to SHRT_MAX*/
			if ((new_size < 0 && (pBD->capacity < SHRT_MAX)) || pBD->capacity == new_size)
				new_size = SHRT_MAX;
			else if (new_size < 0) /* otherwise error */
				return NULL;
		}

		/* reallocate the buffer with the new size*/
		new_buffer = realloc(pBD->cb_head, new_size);
		if (!new_buffer)
			return NULL;

		/* if there is still no space to add a char, return NULL */
		if (new_size <= pBD->addc_offset)
			return NULL;

		/* update buffer and add char */
		if (new_buffer != pBD->cb_head){
			pBD->cb_head = new_buffer;
			pBD->r_flag = SET_R_FLAG; /*indicate reallocation succeeded*/
		}
		pBD->capacity = new_size;
		pBD->cb_head[pBD->addc_offset++] = symbol; 
	}

	return pBD;
}

/*
Purpose: reset the buffer and move the add-char location back to the beginning to overwrite any existing buffer contents
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: None
Parameters: Buffer* pBD
Return value: int, 0 on success or -1 on runtime error
Algorithm: reset everything to 0
*/
int b_reset(Buffer *const pBD)
{
	if (pBD)
	{
		/* reset everything */
		pBD->addc_offset = 0;
		pBD->mark_offset = 0;
		pBD->r_flag = 0;
		pBD->eob = 0;
		pBD->getc_offset = 0;
		return 0;
	}
	return R_FAIL_1;
}

/*
Purpose: Frees the memory occupied by the character buffer and the Buffer struct.
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: free()
Parameters: Buffer* pBD
Return value: void
Algorithm: if pointers to struct and character buffer exist, free them.
*/
void b_destroy(Buffer *const pBD)
{
	if (pBD)
	{
		if (pBD->cb_head)	/* verify that cb_head is valid */
			free(pBD->cb_head); /* free the buffer */
		free(pBD); /* free the structure */
	}
}

/*
Purpose: Check to see if the buffer is full
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: None
Parameters: Buffer* pBD
Return value: int, 1 if buffer is full, 0 if not full, -1 if error
Algorithm: if buffer exists and offset equals capacity, return full.
*/
int b_isfull(Buffer *const pBD)
{
	if (pBD)
		/* return 1 if buffer is full */
		if (pBD->addc_offset == pBD->capacity) 
			return FULL;
		else return NOT_FULL;
	return R_FAIL_1;
}

/*
Purpose: get the size of the buffer
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: None
Parameters: Buffer* pBD
Return value: short addc_offset, or -1 if error
Algorithm: N/A
*/
short b_size(Buffer *const pBD)
{
	if (pBD)
		return pBD->addc_offset;
	return R_FAIL_1;
}

/*
Purpose: get the capacity of the buffer
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: None
Parameters: Buffer* pBD
Return value: short capacity, or -1 if error
Algorithm: N/A
*/
short b_capacity(Buffer *const pBD)
{
	if (pBD)
		return pBD->capacity;
	return R_FAIL_1;
}

/*Purpose: Set the position of the mark within the buffer
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: None
Parameters: Buffer* pBD
			short mark, range between 0 and addc_offset
Return value: char* pointer to char at mark position, or NULL if error
Algorithm: if mark is more than 0 and less than addc_offset, set mark_offset to mark
			return char at position of mark_offset.
*/
char* b_setmark(Buffer *const pBD, short mark)
{
	/*if mark is between 0 and add-char location, set mark to mark offset*/
	if (pBD && mark <= pBD->addc_offset && mark >= 0)
	{
		pBD->mark_offset = mark;
		return (pBD->cb_head + pBD->mark_offset); /*return pointer to char at mark*/
	}
	return NULL;
}

/*Purpose: get the current position of the mark
History: 1.0 September 29th, 2015
Called functions: None
Parameters: Buffer* pBD
Return value: short mark_offset, or -1 if error
Algorithm: N/A
*/
short b_mark(Buffer *const pBD)
{
	if (pBD) 
		return pBD->mark_offset;
	return R_FAIL_1;
}

/*Purpose: get the expansion mode of the buffer
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: None
Parameters: Buffer* pBD
Return value: int mode (casted), or -2 if error
Algorithm: cast mode to int and return
*/
int b_mode(Buffer *const pBD)
{
	if (pBD)
		return (int)pBD->mode;
	return R_FAIL_2;
}

/*
Purpose: returns the non-negative value of inc_factor to the calling function.
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: None
Parameters: Buffer* pBD
Return value: size_t inc_factor, or 256 if error
Algorithm: cast inc_factor to unsigned char then to size_t and return
*/
size_t b_inc_factor(Buffer *const pBD)
{
	if (pBD)
		return (size_t)(unsigned char)pBD->inc_factor;
	return INC_FACTOR_ERR;
}

/*
Purpose: load a buffer from a file pointer
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: feof(), fgetc(), b_addc()
Parameters: FILE* fi, Buffer* pBD
Return value: int count, or -1 for runtime error, or -2 for adding fail
Algorithm: read characters until eof is hit. if eof isn't hit, add char to buffer.
			Increment counter of read chars. return count.
*/
int b_load(FILE *const fi, Buffer *const pBD)
{
	char c;					/* the character just read */
	int count = NEW_SIZE;	/* the number of characters read in total */

	/* check buffer and file aren't null */
	if (!pBD || !fi)
		return R_FAIL_1;

	/* while not at the end of the file, read characters */
	while (!feof(fi))
	{
		c = (char)fgetc(fi);

		/* add the character to the file if not at EOF */
		if (!feof(fi))
		{
			count += 1; /* count the character and add; return -2 if adding fails */
			if (b_addc(pBD, c) == NULL)
				return LOAD_FAIL;
		}
	}
	return count;
}

/*
Purpose: check if the buffer is empty
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: None
Parameters: Buffer* pBD
Return value: int, 1 if buffer is full, 0 if not full, -1 if error.
Algorithm: if addc_offset is 0, it means the buffer is full.
*/
int b_isempty(Buffer *const pBD)
{
	if (!pBD)
		return R_FAIL_1;
	if (pBD->addc_offset == 0) /*if offset is 0, buffer is full*/
		return FULL;
	else
		return NOT_FULL;
}

/*
Purpose: Return EOB to calling function
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: None
Parameters: Buffer* pBD
Return value: int eob, or -1 on error
Algorithm: N/A
*/
int b_eob(Buffer *const pBD)
{
	if (pBD)
		return pBD->eob;
	return R_FAIL_1;
}


/*
Purpose: Returns char at offset position
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: None
Parameters: Buffer* pBD
Return value: char at getc_offset, or -2 if runtime error, or -1 if at EOB
Algorithm: if getc_offset equals addc_offset, set eob to 1. If not, set eob to 0 and increment getc_offset.
			Return char at getc_offset position.
*/
char b_getc(Buffer *const pBD)
{
	if (!pBD)
		return LOAD_FAIL;
	if (pBD->getc_offset == pBD->addc_offset)
	{
		pBD->eob = END;
		return R_FAIL_1;
	}
	else
	{
		pBD->eob = NOT_END;
		return pBD->cb_head[pBD->getc_offset++];
	}
}

/*Purpose: display the contents of the buffer for debugging purposes
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: printf(), b_eob(), b_getc()
Parameters: Buffer* pBD
Return value: int i, or -1 if error
Algorithm: If addc_offset is 0, buffer is empty. Set getc_offset to 0.
			While counter less than addc_offset and EOB isn't hit, get next char and print to console.
			Increment counter. Return getc_offset to 0.
*/
int b_print(Buffer *const pBD)
{
	int i = 0;	/* count of chars being printed*/
	char c;		/* the character being printed */
	if (pBD)
	{
		if (pBD->addc_offset == BEGINNING)
			printf("The buffer is empty.");
		pBD->getc_offset = NEW_SIZE;

		/* display each character in order until  EOB or addc_offset is hit */
		do {
			c = b_getc(pBD);
			if (i == (int)pBD->getc_offset)
				break;
			printf("%c", c);
			i++;
		}while (b_eob(pBD) != END);

		pBD->getc_offset = NEW_SIZE; /*reset getc_offset to 0*/
		
		printf("\n");
		return i;/*return the number of characters displayed */
	}
	return R_FAIL_1;
}

/*
Purpose: Resize the buffer to be exactly one character bigger then the number of symbols in the buffer
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: realloc()
Parameters: Buffer* pBD
Return value: Buffer* pBD, or NULL if error
Algorithm: verify existance, and limits. Get new size at the add-char location + 1.
			reallocate buffer with new size. If needed, reset mark.
*/
Buffer *b_pack(Buffer *const pBD)
{
	/* local variables */
	short new_size;				/* new buffer size */
	char *new_buffer = NULL;	/* pointer to reallocated buffer */
	pBD->r_flag = R_FLAG_NO;	/*reset R flag*/

	if (pBD)
	{
		if (pBD->capacity == SHRT_MAX)/*verify capacity isn't at MAX*/
			return NULL;

		/* get the new size, return NULL if it's impossible to resize */
		new_size = pBD->addc_offset + 1;
		if (new_size < 0)
			return NULL;

		/* resize the buffer, return NULL if there is an error */
		new_buffer = realloc(pBD->cb_head, new_size);
		if (!new_buffer)
			return NULL;

		/* update the buffer */
		pBD->cb_head = new_buffer;
		pBD->capacity = new_size;

		if (new_buffer != pBD->cb_head){
			pBD->cb_head = new_buffer;
			pBD->r_flag = SET_R_FLAG; /*indicate reallocation succeeded*/
		}

		return pBD;
	}
	return NULL;
}

/*
Purpose: Returns r_flag to the calling function
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: None
Parameters: Buffer* pBD
Return value: char r_flag, or -1 if error
Algorithm: N/A
*/
char b_rflag(Buffer *const pBD)
{
	if (pBD)
		return pBD->r_flag;
	return (char)R_FAIL_1;
}

/*
Purpose: retract add-char offset location by one
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: None
Parameters: Buffer* pBD
Return value: short getc_offset, or -1 if error
Algorithm: decrement getc_offset by one
*/
short b_retract(Buffer *const pBD)
{
	if (!pBD)
		return R_FAIL_1;
	pBD->getc_offset--;
	return pBD->getc_offset;
}

/*
Purpose: retract add-char offset location to the mark location
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: None
Parameters: Buffer* pBD
Return value: short getc_offset, or -1 if error
Algorithm: Assign mark offset to the add-char offset
*/
short b_retract_to_mark(Buffer *const pBD)
{
	if (!pBD)
		return R_FAIL_1;
	pBD->getc_offset = pBD->mark_offset;
	return pBD->getc_offset;
}

/*
Purpose: Returns getc_offset to the calling function
Author: Joshua Robertson
History: 1.0 September 29th, 2015
Called functions: None
Parameters: Buffer* pBD
Return value: short getc_offset, or -1 if error
Algorithm: N/A
*/
short b_getc_offset(Buffer *const pBD)
{
	if (pBD)
		return pBD->getc_offset;
	return R_FAIL_1;
}


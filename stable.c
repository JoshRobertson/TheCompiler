/* Filename: stable.c
* Compiler: MS Visual Studio 2013
* Author: Joshua Robertson
* Course: CST8152 - Compilers, Lab Section 011
* Assignment: 3
* Date: November 20, 2015
* Professor: Sv. Ranev
* Purpose: creates a Symbol Table to store tokens from the Scanner 
*			as required for CST8152, Assignment #3
* Version: 1.0
* Date: 20 November 2015
* Function List:st_create()
*				st_install()
*				st_lookup()
*				st_update_type()
*				st_update_value()
*				st_get_type()
*				st_destroy()
*				st_print()
*				st_setsize()
*				st_incoffset()
*				st_store()
*				st_compare_ascending()
*				st_compare_descending()
*				st_sort()
*/
#define _CRT_SECURE_NO_WARNINGS
#pragma warning( push)
#pragma warning( disable : 4001) /*disable single line comment errors from standard files*/
#include <string.h>
#pragma warning(pop)

#include "buffer.h"
#include "stable.h"

STD sym_table; /*the symbol table*/

/*Purpose: set the size of the symbol table to 0
Author: Joshua Robertson
History: 1.0 November 20, 2015
Called functions: none
Parameters: none
Return value: none
Algorithm: N/A
*/
static void st_setsize(void){
	sym_table.st_size = 0;
}

/*Purpose: increment st_offset position by 1
Author: Joshua Robertson
History: 1.0 November 20, 2015
Called functions: none
Parameters: none
Return value: none
Algorithm: N/A
*/
static void st_incoffset(void){
	sym_table.st_offset++;
}

/*Purpose: To create the symbol table and dynamically allocate it's members
Author: Joshua Robertson
History: 1.0 November 20, 2015
Called functions: malloc(), sizeof(), b_create(), free()
Parameters: int st_size
Return value: STD table
Algorithm: create new STD instance, allocate memory for members, and return if passes NULL check
*/
STD st_create(int st_size){
	STD table;
	table.plsBD = NULL;
	table.pstvr = NULL;
	table.st_offset = 0;
	table.st_size = 0;

	/*check parameter for negative values*/
	if (st_size <= 0)
		return table;

	/*malloc space for pstvr and check for NULL*/
	if ((table.pstvr = (STVR*)malloc(st_size * sizeof(STVR))) == NULL)
		return table;/* return empty table if error*/

	/*create buffer for plsBD and check for NULL. Clear pstvr if NULL check fails.*/
	if ((table.plsBD = b_create(STBL_INIT_SIZE, STBL_INC, 'a')) == NULL){
		free(table.pstvr);
		table.pstvr = NULL;
		return table; /*clear pstvr before returning empty table*/
	}

	table.st_size = st_size;
	return table;
}

/*Purpose: install a new symbol in the symbol table
Author: Joshua Robertson
History: 1.0 November 20, 2015
Called functions: strlen(), st_lookup(), b_setmark(), b_size(), b_retract_to_mark(), b_rflag(), st_incoffset()
Parameters: STD sym_table, char *lexeme, char type, int line
Return value: int sym_table.st_offset number of elements in symbol table
Algorithm: check parameters, is symbol exists, return it. if not, add lexeme to symbol table.
			set attributes to appropriate values. Increment global symbol table.
*/
int st_install(STD sym_table, char *lexeme, char type, int line){

	int exists;
	unsigned int i;
	int j;

	/*check incoming symbol table's storage buffer and STVR for NULL and STVIR for invalid range*/
	if (sym_table.st_size <= 0 || sym_table.plsBD == NULL || sym_table.pstvr == NULL)
		return INVALID_TABLE;

	/*check line number parameter to make sure its valid*/
	if (line < 0) return INVALID_LINE;

	/*check lexeme to make sure it's not NULL or empty*/
	if (lexeme == NULL || strlen(lexeme) == 0) return INVALID_LEXEME;

	/*lookup lexeme and return symbol if it exists in the table*/
	if ((exists = st_lookup(sym_table, lexeme)) != SYMBOL_NOT_FOUND) return exists;

	/*check to make sure table isn't full*/
	if (sym_table.st_offset == sym_table.st_size) return TABLE_FULL;

	/*set plex to start of lexeme, which is the addc_offset of plsBD*/
	sym_table.pstvr[sym_table.st_offset].plex = b_setmark(sym_table.plsBD, b_size(sym_table.plsBD));
	
	/*add lexeme to plsBD*/
	for (i = 0; i <= strlen(lexeme); i++)
	{
		if (b_addc(sym_table.plsBD, lexeme[i]) == NULL)
		{
			/*if error, return to original addc_offset*/
			b_retract_to_mark(sym_table.plsBD);
			return INSTALL_ERROR;
		}

		/*if plsBD has been reallocated, recalculate offsets in the table*/
		if (b_rflag(sym_table.plsBD) == R_FLAG_YES)
		{
			for (j = 0; j <= sym_table.st_offset; j++)
			{
				if (j == 0)
					sym_table.pstvr[j].plex = b_setmark(sym_table.plsBD, BEGINNING);
				else
					/*get previous element's plex, calculate strlen+1 and assign to current element plex*/
					sym_table.pstvr[j].plex = &(sym_table.pstvr[j - 1].plex[strlen(sym_table.pstvr[j - 1].plex) + 1]);
			}
		}
	}

	/*set line and default status*/
	sym_table.pstvr[sym_table.st_offset].o_line = line;
	sym_table.pstvr[sym_table.st_offset].status_field = STATUS_DEFAULT;

	/*apply proper bitmask to status_field depending on type parameter*/
	switch (type)
	{
	case 'S':
		/*set update flag because String cannot be updated*/
		sym_table.pstvr[sym_table.st_offset].status_field |= STATUS_TYPE_STRING | STATUS_UPDATE_FLAG;
		sym_table.pstvr[sym_table.st_offset].i_value.str_offset = -1;
		break;
	case 'I':
		sym_table.pstvr[sym_table.st_offset].status_field |= STATUS_TYPE_INT;
		sym_table.pstvr[sym_table.st_offset].i_value.int_val = 0;
		break;
	case 'F':
		sym_table.pstvr[sym_table.st_offset].status_field |= STATUS_TYPE_FLOAT;
		sym_table.pstvr[sym_table.st_offset].i_value.fpl_val = 0;
		break;
	default:
		return INSTALL_ERROR;
	}

	st_incoffset();

	return sym_table.st_offset;
}

/*Purpose: Search for a lexeme in the Symbol Table
Author: Joshua Robertson
History: 1.0 November 20, 2015
Called functions: strlen(), strcmp()
Parameters: STD sym_table, char* lexeme
Return value: offset of lexeme from start of STVR array, or -1 on error
Algorithm: check parameters, make sure lexeme exists, loop through pstvr backwards until match found
*/
int st_lookup(STD sym_table, char *lexeme){

	int i;

	/*check incoming symbol table's storage buffer and STVR for NULL and STVIR for invalid range*/
	if (sym_table.st_size <= 0 || sym_table.plsBD == NULL || sym_table.pstvr == NULL)
		return INVALID_TABLE;
	
	/* make sure the lexeme exists */
	if (lexeme == NULL || strlen(lexeme) == 0)
		return INVALID_LEXEME;

	/*start at end of array, and decrement through each VID to check for a match*/
	for (i = sym_table.st_offset - 1; i >= 0; i--)
		if (strcmp(sym_table.pstvr[i].plex, lexeme) == 0)
			return i;
	
	/*if no match found, return error*/
	return SYMBOL_NOT_FOUND;

}

/*Purpose:
Author: Joshua Robertson
History: 1.0 November 20, 2015
Called functions:
Parameters: STD sym_table, int vid_offset, char v_type
Return value: int vid_offset position of lexeme in symbol table
Algorithm: check parameters, verify not a String, verify not already updated.
			set update flag, clear old type, set new type.
*/
int st_update_type(STD sym_table, int vid_offset, char v_type){

	/*check incoming symbol table's storage buffer and STVR for NULL and STVIR for invalid range*/
	if (sym_table.st_size <= 0 || sym_table.plsBD == NULL || sym_table.pstvr == NULL)
		return INVALID_TABLE;

	/* make sure vid_offset is a valid position in the symbol table*/
	if (vid_offset < 0 || vid_offset >= sym_table.st_offset)
		return INVALID_RANGE;

	/*String cannot be updated, so if not F or I, error*/
	if (!(v_type == 'F' || v_type == 'I'))
		return INVALID_TYPE;

	/*check if type has already been updated*/
	if ((sym_table.pstvr[vid_offset].status_field & STATUS_UPDATE_FLAG) == STATUS_UPDATE_FLAG)
		return ALREADY_UPDATED;

	/* set the update flag. F and I can only be updated once. */
	sym_table.pstvr[vid_offset].status_field |= STATUS_UPDATE_FLAG;

	/* clear the old type */
	sym_table.pstvr[vid_offset].status_field &= !STATUS_TYPE;

	/* set the new type */
	if (v_type == 'F')
		sym_table.pstvr[vid_offset].status_field |= STATUS_TYPE_FLOAT;
	else
		sym_table.pstvr[vid_offset].status_field |= STATUS_TYPE_INT;

	return vid_offset;
}

/*Purpose: To update the i_value of the variable at vid_offset
Author: Joshua Robertson
History: 1.0 November 20, 2015
Called functions: none
Parameters: STD sym_table, int vid_offset, InitialValue i_value
Return value: vid_offset or -1 on error
Algorithm: check parameters, and set i_value to parameter
*/
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value){

	/*check incoming symbol table's storage buffer and STVR for NULL and STVIR for invalid range*/
	if (sym_table.st_size <= 0 || sym_table.plsBD == NULL || sym_table.pstvr == NULL)
		return INVALID_TABLE;

	/* make sure vid_offset is a valid position in the symbol table*/
	if (vid_offset < 0 || vid_offset >= sym_table.st_offset)
		return INVALID_RANGE;

	/*set i_value to parameter*/
	sym_table.pstvr[vid_offset].i_value = i_value;

	return vid_offset;
}

/*Purpose: Get the type of the symbol at vid_offset in the symbol table
Author: Joshua Robertson
History: 1.0 November 20, 2015
Called functions:
Parameters: STD sym_table, int vid_offset
Return value: char representing status or -1 if error
Algorithm: Check parameters, apply bitmask to status_field, return char based on result
*/
char st_get_type(STD sym_table, int vid_offset){

	/*check incoming symbol table's storage buffer and STVR for NULL and STVIR for invalid range*/
	if (sym_table.st_size <= 0 || sym_table.plsBD == NULL || sym_table.pstvr == NULL)
		return INVALID_TABLE;

	/* make sure vid_offset is a valid position in the symbol table*/
	if (vid_offset < 0 || vid_offset >= sym_table.st_offset)
		return INVALID_RANGE;
	/* check status by applying bitmask. switch based on resulting bitfield*/
	switch (sym_table.pstvr[vid_offset].status_field & STATUS_TYPE)
	{
	case STATUS_TYPE_INT: return 'I';
	case STATUS_TYPE_FLOAT: return 'F';
	case STATUS_TYPE_STRING: return 'S';
	default: return INVALID_TYPE;
	}

}

/*Purpose: Destroy the symbol table's members, set size to 0
Author: Joshua Robertson
History: 1.0 November 20, 2015
Called functions: b_destroy(), free(), st_setsize()
Parameters: STD sym_table
Return value: none
Algorithm: check dynamic members, and free the memory for them, set size to 0
*/
void st_destroy(STD sym_table){
	if (sym_table.plsBD != NULL)
		b_destroy(sym_table.plsBD);
	if (sym_table.pstvr != NULL)
		free(sym_table.pstvr);
	st_setsize();
}

/*Purpose: Print the symbol table contents
Author: Joshua Robertson
History: 1.0 November 20, 2015
Called functions: printf()
Parameters: STD sym_table
Return value: int st_offset number of records, or -1 on failure
Algorithm: check inputs, print header, print each VID and line number, return number of records
*/
int st_print(STD sym_table){
	int i;

	/*check incoming symbol table's storage buffer and STVR for NULL and STVIR for invalid range*/
	if (sym_table.st_size <= 0 || sym_table.plsBD == NULL || sym_table.pstvr == NULL)
		return INVALID_TABLE;

	/*print header info*/
	printf("\nSymbol Table\n____________\n\n");
	printf("%-12s%s\n", "Line Number", "Variable Identifier");

	/*print each VID name and line number where is first occured*/
	for (i = 0; i < sym_table.st_offset; i++)
		printf("%2d%10s%s\n", sym_table.pstvr[i].o_line, " ", sym_table.pstvr[i].plex);

	return sym_table.st_offset;
}

/*Purpose: Write the symbol table to a text file
Author: Joshua Robertson
History: 1.0 November 20, 2015
Called functions: fopen(), fprintf(), strlen(), st_get_type(), flclose()
Parameters: STD sym_table
Return value: int st_offset number of records, or -1 on failure
Algorithm: create $stable.ste output file, print st_size, print contents of each entry in pstvr,
			switch to check type in i_value, print proper result, close file, return number of records
*/
int st_store(STD sym_table){
	FILE* output_file;
	int i;

	/*check incoming symbol table's storage buffer and STVR for NULL and STVIR for invalid range*/
	if (sym_table.st_size <= 0 || sym_table.plsBD == NULL || sym_table.pstvr == NULL)
		return INVALID_TABLE;

	/*create output file*/
	if ((output_file = fopen("$stable.ste", "w+")) == NULL)
		return FILE_ERROR;

	fprintf(output_file, "%d", sym_table.st_size);

	for (i = 0; i < sym_table.st_offset; i++)
	{
		/*print contents of each entry in pstvr*/
		fprintf(output_file, " %X %d %s %d ", sym_table.pstvr[i].status_field, strlen(sym_table.pstvr[i].plex), sym_table.pstvr[i].plex, sym_table.pstvr[i].o_line);

		/*check type to know which Union data member to print*/
		switch (st_get_type(sym_table, i))
		{
		case 'I': fprintf(output_file, "%d", sym_table.pstvr[i].i_value.int_val); break;
		case 'F': fprintf(output_file, "%.2f", sym_table.pstvr[i].i_value.fpl_val); break;
		case 'S': fprintf(output_file, "%d", sym_table.pstvr[i].i_value.str_offset); break;
		/*in case of irregularities, close file and return error*/
		default: fclose(output_file); return INVALID_TYPE;
		}
	}

	printf("Symbol Table stored.\n");
	fclose(output_file);

	return sym_table.st_offset;
}

/*Purpose: comparator to iterate through symbols in descending order
Author: Joshua Robertson
History: 1.0 November 20, 2015
Called functions: strcmp()
Parameters: const void* a, const void* b
Return value: 0 or 1
Algorithm: cast parameter to a pointer to STVR, compare the first char of each lexeme
*/
int st_compare_descending(const void* a, const void* b)
{
	STVR *stvr_a = (STVR*)a;
	STVR *stvr_b = (STVR*)b;
	return strcmp(stvr_b->plex, stvr_a->plex);
}

/*Purpose: comparator to iterate through symbols in ascending order
Author: Joshua Robertson
History: 1.0 November 20, 2015
Called functions: strcmp()
Parameters: const void* a, const void* b
Return value: 0 or 1
Algorithm: cast parameter to a pointer to STVR, compare the first char of each lexeme
*/
int st_compare_ascending(const void* a, const void* b)
{
	STVR *stvr_a = (STVR*)a;
	STVR *stvr_b = (STVR*)b;
	return strcmp(stvr_a->plex, stvr_b->plex);
}

/*Purpose: sort the symbol table entries by variable name in ascending or descending order 
Author: Joshua Robertson
History: 1.0 November 20, 2015
Called functions: qsort(), st_compare_descending, st_compare_ascending
Parameters: STD sym_table, char s_order
Return value: 0
Algorithm: check paramter, call qsort based on s_order parameter.
*/
int st_sort(STD sym_table, char s_order){

	/*check incoming symbol table's storage buffer and STVR for NULL and STVIR for invalid range*/
	if (sym_table.st_size <= 0 || sym_table.plsBD == NULL || sym_table.pstvr == NULL)
		return INVALID_TABLE;

	/*call qsort either descending or ascending based on parameter*/
	if (s_order == 'D')
		qsort((void*)sym_table.pstvr, sym_table.st_offset, sizeof(STVR), st_compare_descending);
	else if (s_order == 'A')
		qsort((void*)sym_table.pstvr, sym_table.st_offset, sizeof(STVR), st_compare_ascending);
	else
		return INVALID_SORT;
	return SORT_SUCCESS;
}

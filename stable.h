/* Filename: stable.h
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
*				st_store()
*				st_sort()
*/
#ifndef STABLE_H
#define STABLE_H

#define STBL_INIT_SIZE 10 /*intial size of STBL*/
#define STBL_INC 5		  /*increment of STBL*/

typedef union InitialValue {
	int int_val; /* integer variable initial value */
	float fpl_val; /* floating-point variable initial value */
	int str_offset; /* string variable initial value (offset) */
} InitialValue;

typedef struct SymbolTableVidRecord {
	unsigned short status_field; /* variable record status field*/
	char * plex; /* pointer to lexeme (VID name) in CA */
	int o_line; /* line of first occurrence */
	InitialValue i_value; /* variable initial value */
	size_t reserved; /*reserved for future use*/
}STVR;

typedef struct SymbolTableDescriptor {
	STVR *pstvr; /* pointer to array of STVR */
	int st_size; /* size in number of STVR elements */
	int st_offset; /*offset in number of STVR elements */
	Buffer *plsBD;  /* pointer to the lexeme storage buffer descriptor */
} STD;

#define INVALID_TABLE		-1
#define INVALID_TYPE		-1
#define INVALID_RANGE		-1
#define INVALID_LEXEME		-1
#define INVALID_LINE		-1
#define INVALID_SORT		-1
#define FILE_ERROR			-1
#define SYMBOL_NOT_FOUND	-1
#define TABLE_FULL			-1
#define INSTALL_ERROR		-1
#define ALREADY_UPDATED		-1

#define SORT_SUCCESS		 1

#define STATUS_DEFAULT		0xFFF8		/*default reserved bits are 1, data type indicators are 00, and update flag is 0*/
#define STATUS_TYPE			0x0006		
#define STATUS_TYPE_INT		0x0004		
#define STATUS_TYPE_FLOAT	0x0002		
#define STATUS_TYPE_STRING	0x0006		
#define STATUS_UPDATE_FLAG	0x0001		

STD st_create(int st_size);
int st_install(STD sym_table, char *lexeme, char type, int line);
int st_lookup(STD sym_table, char *lexeme);
int st_update_type(STD sym_table, int vid_offset, char v_type);
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value);
char st_get_type(STD sym_table, int vid_offset);
void st_destroy(STD sym_table);
int st_print(STD sym_table);
int st_store(STD sym_table);
int st_sort(STD sym_table, char s_order);

#endif

/* Filename: parser.h
* Compiler: MS Visual Studio 2013
* Author: Joshua Robertson
* Course: CST8152 - Compilers, Lab Section 011
* Assignment: 3
* Date: December 11, 2015
* Professor: Sv. Ranev
* Purpose: Declaration of parser constants and function prototypes
* Version: 1.0
* Date: 11 December 2015
Function list:  parser()
*				match()
*				syn_eh()
*				syn_printe()
*				gen_incode()
*				variable_identifier()
*				variable_list_p()
*				variable_list()
*				primary_a_relational_expression()
*				primary_s_relational_expression()
*				a_relational_comparison()
*				s_relational_comparison()
*				relational_expression()
*				logical_AND_expression_p()
*				logical_AND_expression()
*				logical_OR_expression_p()
*				logical_OR_expression()
*				conditional_expression()
*				primary_arithmetic_expression()
*				unary_arithmetic_expression()
*				multiplicative_arithmetic_expression_p()
*				multiplicative_arithmetic_expression()
*				additive_arithmetic_expression_p()
*				additive_arithmetic_expression()
*				arithmetic_expression()
*				primary_string_expression()
*				string_expression_p()
*				string_expression()
*				assignment_expression()
*				input_statement()
*				output_statement_p()
*				output_statement()
*				assignment_statement()
*				iteration_statement()
*				selection_statement()
*				statement()
*				statements_p()
*				statements()
*				opt_statements()
*				program()
*              
*/

#ifndef PARSER_H
#define PARSER_H

#include "buffer.h"
#include "stable.h"
#include "token.h"

/*  constants */
#define NO_ATTR			0

#define KW_ELSE			0
#define KW_IF			1
#define KW_INPUT		2
#define KW_OUTPUT		3
#define KW_PLATYPUS		4
#define KW_REPEAT		5
#define KW_THEN			6
#define KW_USING		7

#define AR_PLUS			0
#define AR_MINUS		1
#define AR_MULT			2
#define AR_DIV			3

#define LOG_AND			0
#define LOG_OR			1

#define REL_EQ			0
#define REL_NE			1
#define REL_GT			2
#define REL_LT			3


static Token lookahead;
static Buffer* sc_buf;
int synerrno;

extern Token mlwpar_next_token(Buffer * sc_buf);
extern int line;
extern STD sym_table;
extern Buffer* str_LTBL;
extern char *kw_table[];

/* function prototypes */
void parser(Buffer *in_buf);
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int pr_token_code);
void syn_printe(void);
void gen_incode(char *string);
void program(void);
void opt_statements(void);
void statements(void);
void statements_p(void);
void statement(void);
void assignment_statement(void);
void assignment_expression(void);
void selection_statement(void);
void iteration_statement(void);
void input_statement(void);
void variable_list(void);
void variable_list_p(void);
void variable_identifier(void);
void output_statement(void);
void output_statement_p(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_p(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_p(void);
void primary_arithmetic_expression(void);
void string_expression(void);
void string_expression_p(void);
void primary_string_expression(void);
void conditional_expression(void);
void logical_OR_expression(void);
void logical_OR_expression_p(void);
void logical_AND_expression(void);
void logical_AND_expression_p(void);
void relational_expression(void);
void a_relational_comparison(void);
void s_relational_comparison(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);

#endif


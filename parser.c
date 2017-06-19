/* Filename: parser.c
* Compiler: MS Visual Studio 2013
* Author: Joshua Robertson
* Course: CST8152 - Compilers, Lab Section 011
* Assignment: 3
* Date: December 11, 2015
* Professor: Sv. Ranev
* Purpose:  a syntax parser for the Platypus language
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

#include "parser.h"
#include <stdlib.h>

/*Purpose: Start the parsing process
Author: Joshua Robertson
History: 1.0 December 11, 2015
Called functions: mlwpar_next_token(),program(),match(),gen_incode()
Parameters: in_buf, source buffer
Return value: none
Algorithm: N/A
*/
void parser(Buffer * in_buf){
	sc_buf = in_buf;
	lookahead = mlwpar_next_token(sc_buf);
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/*Purpose: match the specified token
Author: Joshua Robertson
History: 1.0 December 11, 2015
Called functions: mlwpar_next_token(), syn_printe(), syn_eh()
Parameters: pr_token_code, pr_token_attribute
Return value: none
Algorithm: if SEOF, return. If lookahead code matches, check attribute. if both match,
	match next token. If next token is error, call syun_printe() and advance once more. if no match
	call syn_eh()
*/
void match(int pr_token_code, int pr_token_attribute)
{
	/*deafult attribute match (1) unless switch statement checks to override*/
	int match = 1;

	switch (lookahead.code)
	{
		case SEOF_T: if (pr_token_code == SEOF_T) return;
		case KW_T:
		case LOG_OP_T:
		case ART_OP_T:
		case REL_OP_T:	
			match = (lookahead.attribute.get_int == pr_token_attribute); 
			break;
	}

	/*bitwise AND to check attribute and code both match*/
	match = match && (lookahead.code == pr_token_code);

	/*switch instead of if for efficiency*/
	switch (match)
	{
	case 1:
		/* 1 AND 1 - advance token and check for ERR_T*/
		if ((lookahead = mlwpar_next_token(sc_buf)).code == ERR_T)
		{
			syn_printe();
			lookahead = mlwpar_next_token(sc_buf);
			synerrno++;
		}
		return;

	case 0:	/* either code or attribute didn't match */
		syn_eh(pr_token_code);
	}
}

/*Purpose: error handler
Author: Joshua Robertson
History: 1.0 December 11, 2015
Called functions: syn_printe(), exit(), mlwpar_next_token()
Parameters: sync_token_code
Return value: none
Algorithm: print syntax error, increment error count, advance token until sync token is matched.
exit if its SEOF. Once match (not SEOF) is found, advance one more token.
*/
void syn_eh(int sync_token_code)
{
	syn_printe();
	synerrno++;

	while (1)
	{
		if (lookahead.code == SEOF_T && sync_token_code != SEOF_T)
			exit(synerrno);

		lookahead = mlwpar_next_token(sc_buf);
		if (lookahead.code == sync_token_code)
		{
			if (sync_token_code != SEOF_T)
				lookahead = mlwpar_next_token(sc_buf);
			return;
		}
	}
}

/*Purpose: error message printing
Author: Joshua Robertson
History: 1.0 December 11, 2015
Called functions: printf()
Parameters: none
Return value: none
Algorithm: print error message, line number, token code, and attribute
*/
void syn_printe(){
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code){
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n");
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", sym_table.pstvr[t.attribute.get_int].plex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_setmark(str_LTBL, t.attribute.str_offset));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	} /*end switch*/
} /*end syn_printe()*/

/*Purpose: print parse message
Author: Joshua Robertson
History: 1.0 December 11, 2015
Called functions: printf()
Parameters: string as char* output to print
Return value: none
Algorithm: N/A
*/
void gen_incode(char *string)
{
	printf("%s\n", string);
}

/*<program> -> PLATYPUS {<opt_statements>}

FIRST(<program>)= { KW_T(PLATYPUS) }
*/
void program(void)
{
	match(KW_T, KW_PLATYPUS); match(LBR_T, NO_ATTR); opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*<opt_statements> -> <statements> | ɛ

FIRST(<opt_statements>) = { FIRST(<statements>), ɛ }
= { AVID_T, SVID_T, KW_T(IF), KW_T(USING), KW_T(INPUT) , KW_T(OUTPUT), ɛ }
*/
void opt_statements(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		statements(); break;
	case KW_T:
		switch (lookahead.attribute.kwt_idx)
		{
		case KW_IF:
		case KW_USING:
		case KW_INPUT:
		case KW_OUTPUT:
			statements();
			return;
		}
	default:
		gen_incode("PLATY: Opt_statements parsed");
	}
}

/*<statements> -> <statement><statements_p>

FIRST(<statements>) = { FIRST(<statement>) 
= { FIRST(<assignment statement>), FIRST(<selection statement>),
	FIRST(<iteration statement>), FIRST(<input statement>), FIRST(<output statement>) }
= { AVID_T, SVID_T, KW_T(IF), KW_T(USING), KW_T(INPUT) , KW_T(OUTPUT) }
*/
void statements(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		statement();
		statements_p();
		break;
	case KW_T:
		switch (lookahead.attribute.kwt_idx)
		{
		case KW_IF:
		case KW_USING:
		case KW_INPUT:
		case KW_OUTPUT:
			statement();
			statements_p();
			return;
		}
	default:
		syn_printe();
	}
}

/*<statements_p> -> <statement><statement_p> |  ɛ

FIRST(<statements_p>) = {FIRST(<statement>), ɛ }
= { AVID_T, SVID_T, KW_T(IF), KW_T(USING), KW_T(INPUT) , KW_T(OUTPUT), ɛ }
*/
void statements_p(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		statement();
		statements_p();
		break;
	case KW_T:
		switch (lookahead.attribute.kwt_idx)
		{
		case KW_IF:
		case KW_USING:
		case KW_INPUT:
		case KW_OUTPUT:
			statement();
			statements_p();
			return;
		}
	}
}

/*<statement> ->
	<assignment statement>| <selection statement>| <iteration statement>
	| <input statement>| <output statement>

FIRST(<statement>) = { FIRST(<assignment statement>), FIRST(<selection statement>),
	FIRST(<iteration statement>), FIRST(<input statement>), FIRST(<output statement>) }
= { AVID_T, SVID_T, KW_T(IF), KW_T(USING), KW_T(INPUT) , KW_T(OUTPUT) }
*/
void statement(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		assignment_statement(); break;
	case KW_T:
		switch (lookahead.attribute.kwt_idx)
		{
		case KW_IF:		selection_statement(); return;
		case KW_USING:	iteration_statement(); return;
		case KW_INPUT:	input_statement(); return;
		case KW_OUTPUT:	output_statement(); return;
		}
	default: syn_printe();
	}
}

/*<assignment statement> -> <assignment expression>;

FIRST(<assignment statement>)= { FIRST(<assignment expression>) }
= { AVID_T, SVID_T }
*/
void assignment_statement(void)
{
	assignment_expression(); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/*<assignment expression> -> AVID = <arithmetic expression>
	| SVID = <string expression>

FIRST(<assignment expression>) = { AVID_T, SVID_T }
*/
void assignment_expression(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
		match(AVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR); arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR); string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default: syn_printe();
	}
}

/*<selection statement> -> IF (<conditional expression>)  THEN  <opt_statements> 
	ELSE { <opt_statements> } ;

FIRST(<selection statement>) = { KW_T(IF) }
*/
void selection_statement(void)
{
	match(KW_T, KW_IF); match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, KW_THEN);
	opt_statements();
	match(KW_T, KW_ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);

	gen_incode("PLATY: IF statement parsed");
}

/*<iteration statement> ->
	USING  (<assignment expression> , <conditional expression> , <assignment  expression> )
	REPEAT {<opt_statements>};

FIRST(<iteration statement>) = { KW_T(USING) }
*/
void iteration_statement(void)
{
	match(KW_T, KW_USING);
	match(LPR_T, NO_ATTR);
	assignment_expression();
	match(COM_T, NO_ATTR);
	conditional_expression();
	match(COM_T, NO_ATTR);
	assignment_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, KW_REPEAT);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);

	gen_incode("PLATY: USING statement parsed");
}

/*<input statement> -> INPUT (<variable list>);

FIRST(<input statement>) = { KW_T(INPUT) }
*/
void input_statement(void)
{
	match(KW_T, KW_INPUT); match(LPR_T, NO_ATTR); variable_list();
	match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: INPUT statement parsed");
}

/*<variable list> -> <variable identifier><variable list_p>

FIRST(<variable list>) = { FIRST(<variable identifier>) }
= { AVID_T, SVID_T }
*/
void variable_list(void)
{
	variable_identifier();
	variable_list_p();
	gen_incode("PLATY: Variable list parsed");
}

/*<variable list_p> -> , <variable identifier><variable list_p> | ɛ

FIRST(<variable list p>) = { COM_T, ɛ }
*/
void variable_list_p(void)
{
	switch (lookahead.code)
	{
	case COM_T:
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_p();
		break;
	}
}

/*<variable identifier> ->	<SVID_T> | <AVID_T>

FIRST(<variable identifier>) = { SVID_T, AVID_T }
*/
void variable_identifier(void)
{
	switch (lookahead.code)
	{
	case SVID_T: match(SVID_T, NO_ATTR); break;
	case AVID_T: match(AVID_T, NO_ATTR); break;
	default: syn_printe();
	}
}

/*<output statement> ->	OUTPUT(<output statement_p>);

FIRST(<output statement>)= { KW_T(OUTPUT) }
*/
void output_statement(void)
{
	match(KW_T, KW_OUTPUT);
	match(LPR_T, NO_ATTR);
	output_statement_p();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);

	gen_incode("PLATY: OUTPUT statement parsed");
}

/*<output statement_p> -> <STR_T> | <variable list> | ɛ

FIRST(<output statement p>)= { STR_T, FIRST(<variable list>), ɛ }
= { STR_T, AVID_T, SVID_T, ɛ }
*/
void output_statement_p(void)
{
	switch (lookahead.code)
	{
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	case AVID_T:
	case SVID_T:
		variable_list();
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed");
		break;
	}
}

/*<arithmetic expression> -> <unary arithmetic expression>  
	| <additive arithmetic expression>	

FIRST(<arithmetic expression>)
= { FIRST(<unary arithmetic expression>), FIRST(<additive arithmetic expression>) }
= { ART_OP_T(PLUS), ART_OP_T(MINUS), AVID_T, FPL_T, INL_T, LPR_T }
*/
void arithmetic_expression(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additive_arithmetic_expression();
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case PLUS:
		case MINUS:
			unary_arithmetic_expression();
			gen_incode("PLATY: Arithmetic expression parsed");
			return;
		}
	default: syn_printe();
	}
}

/*<unary arithmetic expression> -> -  <primary arithmetic expression> 
	| + <primary arithmetic expression>

FIRST(<unary arithmetic expression>)
= { ART_OP_T(PLUS), ART_OP_T(MINUS) }
*/
void unary_arithmetic_expression(void)
{
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case PLUS: match(ART_OP_T, AR_PLUS); break;
		case MINUS: match(ART_OP_T, AR_MINUS); break;
		default: syn_printe(); return;
		}
		primary_arithmetic_expression();
		gen_incode("PLATY: Unary arithmetic expression parsed");
		return;
	default:
		syn_printe();
	}
}

/*<additive arithmetic expression> ->
	<multiplicative arithmetic expression><additive arithmetic expression_p>

FIRST(<additive arithmetic expression>)
= { FIRST(<multiplicative arithmetic expression>) }
= { FIRST(<primary arithmetic expression>) }
= { AVID_T, FPL_T, INL_T, LPR_T }
*/
void additive_arithmetic_expression(void)
{
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_p();
}

/*<additive arithmetic expression_p> ->
	+<multiplicative arithmetic expression><additive arithmetic expression_p>
	| -<multiplicative arithmetic expression><additive arithmetic expression_p>
	| ɛ

FIRST(<additive arithmetic expression p>)
= { ART_OP_T(PLUS), ART_OP_T(MINUS), ɛ }
*/
void additive_arithmetic_expression_p(void)
{
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case PLUS:
			match(ART_OP_T, AR_PLUS); multiplicative_arithmetic_expression(); additive_arithmetic_expression_p();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		case MINUS:
			match(ART_OP_T, AR_MINUS); multiplicative_arithmetic_expression(); additive_arithmetic_expression_p();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		}
	}
}

/*<multiplicative arithmetic expression> ->
	<primary arithmetic expression><multiplicative arithmetic expression_p>

FIRST(<multiplicative arithmetic expression>)
= { FIRST(<primary arithmetic expression> }
= { AVID_T, FPL_T, INL_T, LPR_T }
*/
void multiplicative_arithmetic_expression(void)
{
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_p();
}

/*<multiplicative arithmetic expression_p> ->
	*<primary arithmetic expression><multiplicative arithmetic expression_p>
	| /<primary arithmetic expression><multiplicative arithmetic expression_p>
	| ɛ

FIRST(<multiplicative arithmetic expression p>)
= { ART_OP_T(MULT), ART_OP_T(DIV), ɛ }
*/
void multiplicative_arithmetic_expression_p(void)
{
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case MULT:
			match(ART_OP_T, AR_MULT); primary_arithmetic_expression(); multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		case DIV:
			match(ART_OP_T, AR_DIV); primary_arithmetic_expression(); multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		}
	}
}

/*<primary arithmetic expression> -> AVID_T| FPL_T| INL_T| (<arithmetic expression>)	

FIRST(<primary arithmetic expression>)
= { AVID_T, FPL_T, INL_T, LPR_T }
*/
void primary_arithmetic_expression(void)
{
	switch (lookahead.code)
	{
	case AVID_T: match(AVID_T, NO_ATTR); break;
	case FPL_T: match(FPL_T, NO_ATTR); break;
	case INL_T: match(INL_T, NO_ATTR); break;
	case LPR_T: match(LPR_T, NO_ATTR); arithmetic_expression(); match(RPR_T, NO_ATTR); break;
	default: syn_printe();
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*<string expression> ->
	<primary string expression><string expression_p>

FIRST(<string expression>)
= { FIRST(<primary string expression>) }
= {SVID_T, STR_T }
*/
void string_expression(void)
{
	primary_string_expression();
	string_expression_p();
	gen_incode("PLATY: String expression parsed");
}

/*<string expression_p> ->
	#<primary string expression><string expression_p> | ɛ

FIRST(<string expression p>)
= { SCC_OP_T, ɛ }
*/
void string_expression_p(void)
{
	switch (lookahead.code)
	{
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_p();
	}
}

/*<primary string expression> ->
  SVID_T | STR_T

FIRST(<primary string expression>)
= { SVID_T, STR_T }
*/
void primary_string_expression(void)
{
	switch (lookahead.code)
	{
	case SVID_T: match(SVID_T, NO_ATTR); break;
	case STR_T: match(STR_T, NO_ATTR); break;
	default: syn_printe();
	}
	gen_incode("PLATY: Primary string expression parsed");
}

/*<conditional expression> ->
	<logical OR  expression>

FIRST(<conditional expression>)
= { FIRST(<logical OR expression>) }
= { SVID_T, STR_T, AVID_T, FPL_T, INL_T }
*/
void conditional_expression(void)
{
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*<logical OR expression> ->
	<logical AND expression><logical OR expression_p>

FIRST(<logical OR expression>)
= { FIRST(<logical AND expression>) }
= { FIRST(<relational expression>) }
= { SVID_T, STR_T, AVID_T, FPL_T, INL_T }
*/
void logical_OR_expression(void)
{
	logical_AND_expression();
	logical_OR_expression_p();
}

/*<logical OR expression_p> ->
	.OR. <logical AND expression><logical OR expression_p> | ɛ

FIRST(<logical OR expression p>)
= { LOG_OP_T(.OR.), ɛ }
*/
void logical_OR_expression_p(void)
{
	switch (lookahead.code)
	{
	case LOG_OP_T:
		switch (lookahead.attribute.log_op)
		{
		case OR:
			match(LOG_OP_T, LOG_OR);
			logical_AND_expression();
			logical_OR_expression_p();
			gen_incode("PLATY: Logical OR expression parsed");
			break;
		}
	}
}

/*<logical AND expression> ->
	<relational expression><logical AND expression_p>

FIRST(<logical AND expression>)
= { FIRST(relational expression) }
= { SVID_T, STR_T, AVID_T, FPL_T, INL_T }
*/
void logical_AND_expression(void)
{
	relational_expression();
	logical_AND_expression_p();
}

/*<logical AND expression_p> ->
	.AND. <relational expression><logical AND expression_p> | ɛ

FIRST(<logical AND expression p>)
= { LOG_OP_T(.AND.), ɛ }
*/
void logical_AND_expression_p(void)
{
	switch (lookahead.code)
	{
	case LOG_OP_T:
		switch (lookahead.attribute.log_op)
		{
		case AND:
			match(LOG_OP_T, LOG_AND);
			relational_expression();
			logical_AND_expression_p();
			gen_incode("PLATY: Logical AND expression parsed");
			break;
		}
	}
}

/*<relational expression> ->
	<primary a_relational expression><a_relational comparison>
	|<primary s_relational expression><s_relational comparison>

FIRST(<relational expression>)
= { FIRST(<primary a_relational expression>), FIRST(<primary s_relational expression>) }
= { SVID_T, STR_T, AVID_T, FPL_T, INL_T }
*/
void relational_expression(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();
		a_relational_comparison();
		break;
	case STR_T:
	case SVID_T:
		primary_s_relational_expression();
		s_relational_comparison();
		break;
	default: syn_printe();
	}
	gen_incode("PLATY: Relational expression parsed");
}

/*<a_relational comparison> ->
	== <primary a_relational expression>
	| <> <primary a_relational expression>
	| > <primary a_relational expression>
	| < <primary a_relational expression>

FIRST(<a_relational comparison>)
= { REL_OP_T(EQ), REL_OP_T(NE), REL_OP_T(GT), REL_OP_T(LT) }
*/
void a_relational_comparison(void)
{
	switch (lookahead.code)
	{
	case REL_OP_T:
		switch (lookahead.attribute.rel_op)
		{
		case EQ: match(REL_OP_T, REL_EQ); break;
		case NE: match(REL_OP_T, REL_NE); break;
		case LT: match(REL_OP_T, REL_LT); break;
		case GT: match(REL_OP_T, REL_GT); break;
		default: syn_printe(); return;
		}
		primary_a_relational_expression();
		return;
	default:
		syn_printe();
	}
}

/*<s_relational comparison> ->
	== <primary s_relational expression>
	| <> <primary s_relational expression>
	| > <primary s_relational expression>
	| < <primary s_relational expression>

FIRST(<s_relational comparison>)
= { REL_OP_T(EQ), REL_OP_T(NE), REL_OP_T(GT), REL_OP_T(LT) }
*/
void s_relational_comparison(void)
{
	switch (lookahead.code)
	{
	case REL_OP_T:
		switch (lookahead.attribute.rel_op)
		{
		case EQ: match(REL_OP_T, REL_EQ); break;
		case NE: match(REL_OP_T, REL_NE); break;
		case LT: match(REL_OP_T, REL_LT); break;
		case GT: match(REL_OP_T, REL_GT); break;
		default: syn_printe(); return;
		}
		primary_s_relational_expression();
		return;
	default:
		syn_printe();
	}
}

/*<primary a_relational expression> -> AVID_T | FPL_T | INL_T

FIRST(<primary a_relational expression>)
= { AVID_T, FPL_T, INL_T }
*/
void primary_a_relational_expression(void)
{
	switch (lookahead.code)
	{
	case AVID_T: match(AVID_T, NO_ATTR); break;
	case FPL_T: match(FPL_T, NO_ATTR); break;
	case INL_T: match(INL_T, NO_ATTR); break;
	default: syn_printe();
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}

/*<primary s_relational expression> -> <primary string expression>

FIRST(<primary s_relational expression>)
= { FIRST(<primary string expression>) }
= { SVID_T, STR_T }
*/
void primary_s_relational_expression(void)
{
	primary_string_expression();
	gen_incode("PLATY: Primary s_relational expression parsed");
}


/******************************************************************************************************************************
*File name: parse.h
*Compiler: MS Visual Studio 2019 Enterprise
*Author: Yucong Yin, 040792791
*Course: CST 8152 – Compilers, Lab Section: 013
*Assignment: Assignment 3
*Date: 2020-Aug-7
*Professor: Paulo Sousa
*Purpose: To build a parser with c language
*
*You are cordially invited to (in other words, you have to) write a Recursive Descent
*Predictive Parser (RDPP) for the PLATYPUS language. You are to integrate the
*Parser with your existing lexical analyzer in order to complete the front-end of your
*PLATYPUS compiler. The implementation is broken into two tasks.
*
*
*Function list: void parser();match();syn_eh();syn_printe();gen_incode();program();opt_statements();statements();statement();
*statementsP();assignment_statement();assignment_expression();selection_statement();iteration_statement();pre_condition();input_statement();
*variable_list();variable_listP();variable_identifier();output_statement();output_statementP();opt_variable_list();arithmetic_expression();unary_arithmetic_expression();
*additive_arithmetic_expression();additive_arithmetic_expressionP();multiplicative_arithmetic_expression();primary_arithmetic_expression();
*multiplicative_arithmetic_expressionP();string_expression();string_expressionP();primary_string_expression();conditional_expression();logical_OR_expression();
*logical_OR_expressionP();logical_AND_expression();logical_AND_expressionP();relational_expression();primary_a_relational_expression();primary_a_relational_expressionP();
*primary_s_relational_expression();primary_s_relational_expressionP();


*********************************************************************************************************************************/


#include "parser.h"

/**********************************************************************************************************************************
Purpose: Parse the source file
Author: Yucong Yin
History/Versions: 1.0
Called functions: malar_next_token(),program();match();gen_incode()
Parameters: N/A

Return value: N/A
Algorithm:
-load the next token
-parse main program
-match SEOF flag
-print message

********************************************************************************************************************************/
void parser(void) {
	lookahead = malar_next_token();
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}
/**********************************************************************************************************************************
Purpose: match a token with the current token caught from the buffer
Author: Yucong Yin
History/Versions: 1.2
Called functions: malar_next_token(),syn_eh(),syn_printe()
Parameters: int pr_token_code - a token code to match the current token
			int pr_token_attribute - the attribute(if exist)of the matching token
Return value: N/A
Algorithm:
-return and handle error if does not match
-if the token code match, handle error if token attribute does not match
-match SEOF flag
-if it is an error token, print error message

********************************************************************************************************************************/
void match(int pr_token_code, int pr_token_attribute)
{
	//match unsuccessful
	if (lookahead.code != pr_token_code)
	{
		syn_eh(pr_token_code);
		return;
	}

	//the rest situation are successful cases:
	//If the match is successfuland the lookahead is SEOF_T, the function returns.
	if (lookahead.code == SEOF_T)
		return;
	switch (pr_token_code) {
	case KW_T:
	case LOG_OP_T:
	case ART_OP_T:
	case REL_OP_T:
		if (pr_token_attribute != lookahead.attribute.get_int) {
			syn_eh(pr_token_code); return;
		}
		break;
	}
		lookahead = malar_next_token();
	/*If the new lookahead token is ERR_T, the function calls the error printing function
syn_printe(), advances to the next input token by calling malar_next_token () again,
increments the error counter synerrno, and returns. */
	if (lookahead.code == ERR_T)
	{
		syn_printe();
		lookahead = malar_next_token();
		++synerrno;
		return;
	}

}

/**********************************************************************************************************************************
Purpose: error handling function
Author: Yucong Yin
History/Versions: 1.0
Called functions: malar_next_token(),syn_printe()
Parameters: int sync_token_code - use to compare with the current token, if does not match, process to the next
Return value: N/A
Algorithm:
- print error message and count the error number
- advance the current token until it matches the one passed in
- while advancing, handle error when it reaches the end of source file

********************************************************************************************************************************/
void syn_eh(int sync_token_code) {

	syn_printe();
	++synerrno;

	/*implements a panic mode error recovery: the function advances the input token
(lookahead) until it finds a token code matching the one required by the parser
(pr_token_code passed to the function as sync_token_code ).*/

	while (lookahead.code != sync_token_code) {

		lookahead = malar_next_token();
		/*If the function looks for
sync_token_code different from SEOF_T and reaches the end of the source file, the
function calls exit(synerrno).*/
		if (lookahead.code == SEOF_T && sync_token_code != SEOF_T) {
			exit(synerrno);
		}
		if (lookahead.code == sync_token_code ) {
			if (lookahead.code != SEOF_T) {
				lookahead = malar_next_token();
				return;
			}
			else
				return;
		}



	}

}

/* error printing function for Assignment 3 (Parser), S20 */
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
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
	}/*end switch*/
}/* end syn_printe()*/


/**********************************************************************************************************************************
Purpose: print out a string
Author: Yucong Yin
History/Versions: 1.0
Called functions: printf()
Parameters: char* string - a character array to be printed
Return value: N/A
Algorithm:
********************************************************************************************************************************/
void gen_incode(char* string) {
	
	printf("%s\n", string);
}
/**********************************************************************************************************************************
Purpose: parse the main program
Author: Yucong Yin
History/Versions: 1.0
Called functions: match(),opt_statements(),gen_incode()
Parameters: N/A
Return value: N/A
Algorithm:
- PLATYPUS{opt_statement}
********************************************************************************************************************************/
void program(void) {
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR); opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/**********************************************************************************************************************************
Purpose: parse the opt_statements
Author: Yucong Yin
History/Versions: 1.0
Called functions: statements(),gen_incode()
Parameters: N/A
Return value: N/A
Algorithm:
<opt_statements> -> <statements> | ϵ

FIRST(<opt_statements>) = { FIRST(<statements>) }
	 = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), ϵ}

********************************************************************************************************************************/
void opt_statements()
{
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		/* check for IF,WHILE,READ,WRITE in statements_p() as well*/
		if (lookahead.attribute.get_int == IF
			|| lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ
			|| lookahead.attribute.get_int == WRITE) {
			statements();
			break;
		}
	default: /*empty string – optional statements*/;
		gen_incode("PLATY: Opt_statements parsed");
	}

}


/**********************************************************************************************************************************
Purpose: parse the statements
Author: Yucong Yin
History/Versions: 1.0
Called functions: statementsP(),statement()
Parameters: N/A
Return value: N/A
Algorithm:
<statements'> -> <statement><statements'> | ϵ

********************************************************************************************************************************/
void statements()
{
	statement();
	statementsP();
}

/**********************************************************************************************************************************
Purpose: parse the statement
Author: Yucong Yin
History/Versions: 1.0
Called functions: assignment_statement(),selection_statement(),iteration_statement(),input_statement(),output_statement()syn_printe();
Parameters: N/A
Return value: N/A
Algorithm:
<statement> ->
  <assignment statement>
| <selection statement>
| <iteration statement>
| <input statement>
| <output statement>


********************************************************************************************************************************/
void statement()
{
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		assignment_statement();
		break;
	case KW_T:
		switch (lookahead.attribute.get_int) {
		case IF: 
			selection_statement(); 
			break;
		case WHILE: 
			iteration_statement(); 
			break;
		case READ: 
			input_statement(); 
			break;
		case WRITE: 
			output_statement(); 
			break;
		}
		break;
	default: 
		syn_printe(); 
		break;
	}
}


/**********************************************************************************************************************************
Purpose: parse the <statement'>
Author: Yucong Yin
History/Versions: 1.0
Called functions: statement(),statementsP()
Parameters: N/A
Return value: N/A
Algorithm:
FIRST(<statements’>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ),
KW_T(WRITE) , ϵ }



********************************************************************************************************************************/
void statementsP()
{
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		statement(); 
		statementsP();
		break;
	case KW_T: 
		if (lookahead.attribute.get_int == IF
		|| lookahead.attribute.get_int == WHILE
		|| lookahead.attribute.get_int == READ
		|| lookahead.attribute.get_int == WRITE) {
		statement(); 
		statementsP();
		break;
	}
	default:
		break;

			
	}
}


/**********************************************************************************************************************************
Purpose: parse the assignment_statement
Author: Yucong Yin
History/Versions: 1.0
Called functions: assignment_expression(),match(),gen_incode()
Parameters: N/A
Return value: N/A
Algorithm:
<assignment statement> ->
	<assignment expression>;




********************************************************************************************************************************/
void assignment_statement()
{
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");

}


/**********************************************************************************************************************************
Purpose: parse the assignment expression
Author: Yucong Yin
History/Versions: 1.0
Called functions: arithmetic_expression(),string_expression(),match(),gen_incode()
Parameters: N/A
Return value: N/A
Algorithm:

< assignment expression> ->
  AVID = <arithmetic expression>
| SVID = <string expression>

********************************************************************************************************************************/
void assignment_expression()
{
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default:
		syn_printe();
		break;
	}
}



/**********************************************************************************************************************************
Purpose: parse the selection statement
Author: Yucong Yin
History/Versions: 1.0
Called functions: pre_condition(),conditional_expression(),opt_statement(),match(),gen_incode()
Parameters: N/A
Return value: N/A
Algorithm:

<selection statement> ->
  IF <pre-condition>  (<conditional expression>) THEN { <opt_statements> }
  ELSE { <opt_statements> } ;
********************************************************************************************************************************/
void selection_statement() {
	match(KW_T, IF);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");


}



/**********************************************************************************************************************************
Purpose: parse the iteration statement
Author: Yucong Yin
History/Versions: 1.0
Called functions: pre_condition(),conditional_expression(),statements(),match(),gen_incode()
Parameters: N/A
Return value: N/A
Algorithm:
<iteration statement> ->
		  WHILE <pre-condition> (<conditional expression>)
		  REPEAT { <statements>};

********************************************************************************************************************************/
void iteration_statement()
{
	match(KW_T, WHILE);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");
}

/**********************************************************************************************************************************
Purpose: parse the pre-condition
Author: Yucong Yin
History/Versions: 1.0
Called functions: match(),syn_printe()
Parameters: N/A
Return value: N/A
Algorithm:
<pre-condition> ->
	TRUE | FALSE

FIRST(<pre-condition>) = { KW_T(TRUE),KW_T(FALSE) }
********************************************************************************************************************************/
void pre_condition()
{
	switch (lookahead.code) {
	case KW_T:
		if (lookahead.attribute.get_int == TRUE)
			match(KW_T, TRUE);
		else if(lookahead.attribute.get_int == FALSE)
			match(KW_T, FALSE);
		else
			syn_printe();
		break;
	default:
		syn_printe(); 
		break;
	}
}


/**********************************************************************************************************************************
Purpose: parse the input statement
Author: Yucong Yin
History/Versions: 1.0
Called functions: match(),variable_list(),gen_incode()
Parameters: N/A
Return value: N/A
Algorithm:
<input statement> ->
READ (<variable list>);

********************************************************************************************************************************/
void input_statement()
{
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}


/**********************************************************************************************************************************
Purpose: parse the variable list
Author: Yucong Yin
History/Versions: 1.0
Called functions: variable_indentifier(),variable_listP(),gen_incode()
Parameters: N/A
Return value: N/A
Algorithm:
<input statement> ->
READ (<variable list>);
<variable list> ->
<variable identifier> <variable list’>

********************************************************************************************************************************/
void variable_list()
{
	variable_identifier();
	variable_listP();
	gen_incode("PLATY: Variable list parsed");
}

/**********************************************************************************************************************************
Purpose: parse the <variable_list'>
Author: Yucong Yin
History/Versions: 1.0
Called functions: variable_indentifier(),variable_listP()
Parameters: N/A
Return value: N/A
Algorithm:
<variable list’> ->
, <variable identifier><variable list’> | ϵ

********************************************************************************************************************************/
void variable_listP()
{

	if (lookahead.code == COM_T) {
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_listP();
	}
}


/**********************************************************************************************************************************
Purpose: parse the variable identifier
Author: Yucong Yin
History/Versions: 1.0
Called functions: match(),syn_printe()
Parameters: N/A
Return value: N/A
Algorithm:

<variable identifier> -> AVID_T | SVID_T

********************************************************************************************************************************/
void variable_identifier()
{
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: 
		match(lookahead.code, NO_ATTR);
		break;
	default: 
		syn_printe();
		
	}

}



/**********************************************************************************************************************************
Purpose: parse the output statement
Author: Yucong Yin
History/Versions: 1.0
Called functions: match(),output_statementP(),gen_incode()
Parameters: N/A
Return value: N/A
Algorithm:
<output statement> ->
  WRITE (<opt_variable list>);

********************************************************************************************************************************/
void output_statement()
{
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	output_statementP();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}


/**********************************************************************************************************************************
Purpose: parse the <output_statement'>
Author: Yucong Yin
History/Versions: 1.0
Called functions: match(),opt_variable_list(), gen_incode()
Parameters: N/A
Return value: N/A
Algorithm:
<output statement’> ->
	<opt_variable list>); | STR_T);

********************************************************************************************************************************/
void output_statementP() {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T: 
		opt_variable_list();
		break;
	case STR_T: 
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed");
		break;
	}

}


/**********************************************************************************************************************************
Purpose: parse the opt_variable_list
Author: Yucong Yin
History/Versions: 1.0
Called functions: variable_list()
Parameters: N/A
Return value: N/A
Algorithm:

<opt_variable list> -> <variable list> | ϵ

********************************************************************************************************************************/
void opt_variable_list()
{
	if (lookahead.code == AVID_T || lookahead.code == SVID_T) {
		variable_list();

	}
}


/**********************************************************************************************************************************
Purpose: parse the arithmetic expression
Author: Yucong Yin
History/Versions: 1.0
Called functions: unary_arithmetic_expression(),syn_printe(),additive_arithmetic_expression(),gen_incode()
Parameters: N/A
Return value: N/A
Algorithm:

<arithmetic expression> - >
  <unary arithmetic expression>
| <additive arithmetic expression>

<unary arithmetic expression> ->
   -  <primary arithmetic expression>
| + <primary arithmetic expression>

FIRST(<additive arithmetic expression>) } = {-,+,AVID_T,FPL_T,INL_T,( ,ϵ }

********************************************************************************************************************************/
void arithmetic_expression()
{
	switch (lookahead.code) {
	case ART_OP_T:
		
		switch (lookahead.attribute.arr_op) {
		case PLUS:
		case MINUS:
			unary_arithmetic_expression();
			break;

		default:
			syn_printe();
			break;
		}

		break;

	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additive_arithmetic_expression();
		break;

	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Arithmetic expression parsed");
}


/**********************************************************************************************************************************
Purpose: parse the unary arithmetic expression
Author: Yucong Yin
History/Versions: 1.0
Called functions: match(), primary_arithmetic_expression(), syn_printe()
Parameters: N/A
Return value: N/A
Algorithm:

FIRST(<unary arithmetic expression>) = { -, + }

********************************************************************************************************************************/
void unary_arithmetic_expression()
{
	if(lookahead.code == ART_OP_T) {
	
		switch (lookahead.attribute.arr_op) {
		case PLUS:
			match(ART_OP_T, PLUS);
			primary_arithmetic_expression();
			break;
		case MINUS:
			match(ART_OP_T, MINUS);
			primary_arithmetic_expression();
			break;
		default: syn_printe(); break;
		}
	}
	gen_incode("PLATY: Unary arithmetic expression parsed");

}


/**********************************************************************************************************************************
Purpose: parse the additive arithmetic expression
Author: Yucong Yin
History/Versions: 1.0
Called functions: 	multiplicative_arithmetic_expression(),additive_arithmetic_expressionP();
Parameters: N/A
Return value: N/A
Algorithm:
<additive arithmetic expression> ->
<multiplicative arithmetic expression> <additive arithmetic expression’>

********************************************************************************************************************************/
void additive_arithmetic_expression()
{
	multiplicative_arithmetic_expression();
	additive_arithmetic_expressionP();
	
}


/**********************************************************************************************************************************
Purpose: parse the <additive arithmetic expression'>
Author: Yucong Yin
History/Versions: 1.0
Called functions: match(),multiplicative_arithmetic_expression(),additive_arithmetic_expressionP(),gen_incode(),syn_printe()
Parameters: N/A
Return value: N/A
Algorithm:
<additive arithmetic expression’> ->
+ <multiplicative arithmetic expression> <additive arithmetic expression’>
| - <multiplicative arithmetic expression> <additive arithmetic expression’>
| ϵ
********************************************************************************************************************************/
void additive_arithmetic_expressionP()
{
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
		case MINUS:
			match(ART_OP_T, lookahead.attribute.arr_op);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expressionP();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		default:
			syn_printe();
			
		}
	}

}

/**********************************************************************************************************************************
Purpose: parse the multiplicative arithmetic expression
Author: Yucong Yin
History/Versions: 1.0
Called functions: 	primary_arithmetic_expression(),multiplicative_arithmetic_expressionP();
Parameters: N/A
Return value: N/A
Algorithm:
<multiplicative arithmetic expression> ->
	<primary arithmetic expression><multiplicative arithmetic expression’>

********************************************************************************************************************************/
void multiplicative_arithmetic_expression()
{
	primary_arithmetic_expression();
	multiplicative_arithmetic_expressionP();
	

}


/**********************************************************************************************************************************
Purpose: parse the primary arithmetic expression
Author: Yucong Yin
History/Versions: 1.0
Called functions:match(),arithmetic_expression(),syn_printe()
Parameters: N/A
Return value: N/A
Algorithm:
<primary arithmetic expression> ->
  AVID_T
| FPL_T
| INL_T
| (<arithmetic expression>)


********************************************************************************************************************************/
void primary_arithmetic_expression()
{
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		match(lookahead.code, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}


/**********************************************************************************************************************************
Purpose: parse the multiplicative arithmetic expression
Author: Yucong Yin
History/Versions: 1.0
Called functions:match(),arithmetic_expression(),syn_printe()
Parameters: N/A
Return value: N/A
Algorithm:

<multiplicative arithmetic expression’> ->
	*<primary arithmetic expression><multiplicative arithmetic expression’>
	|/<primary arithmetic expression><multiplicative arithmetic expression’>
	| ϵ
********************************************************************************************************************************/
void multiplicative_arithmetic_expressionP()
{
	switch (lookahead.code) {
	case ART_OP_T:
		
		switch (lookahead.attribute.arr_op) {
		case MULT:
			match(ART_OP_T, MULT);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expressionP();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;

		case DIV:
			match(ART_OP_T, DIV);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expressionP();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		}
	}
}


/**********************************************************************************************************************************
Purpose: parse the string expression
Author: Yucong Yin
History/Versions: 1.0
Called functions:primary_string_expression(),string_expressionP(),gen_incode()
Parameters: N/A
Return value: N/A
Algorithm:
<string expression> ->
	<primary string expression><string expression’>

********************************************************************************************************************************/
void string_expression()
{
	primary_string_expression();
	string_expressionP();
	gen_incode("PLATY: String expression parsed");

}

/**********************************************************************************************************************************
Purpose: parse the <string expression'>
Author: Yucong Yin
History/Versions: 1.0
Called functions:match(),primary_string_expression(),string_expressionP()
Parameters: N/A
Return value: N/A
Algorithm:
<string expression’> ->
	##<primary string expression><string expression’>
	| ϵ

********************************************************************************************************************************/
void string_expressionP()
{
	switch (lookahead.code) {
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expressionP();
		break;

	default:
		break;
	}

}

/**********************************************************************************************************************************
Purpose: parse the primary string expression
Author: Yucong Yin
History/Versions: 1.0
Called functions:match(),gen_incode()
Parameters: N/A
Return value: N/A
Algorithm:
<primary string expression> ->
  SVID_T
| STR_T

********************************************************************************************************************************/
void primary_string_expression()
{
	switch (lookahead.code) {
	case SVID_T:
	case STR_T: 
		match(lookahead.code, NO_ATTR);
		break;
	default: 
		break;
	}
	gen_incode("PLATY: Primary string expression parsed");
}

void conditional_expression()
{
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed");

}

/**********************************************************************************************************************************
Purpose: parse the logical OR expression
Author: Yucong Yin
History/Versions: 1.0
Called functions: 	logical_AND_expression(),logical_OR_expressionP();
Parameters: N/A
Return value: N/A
Algorithm:
<logical OR expression> ->
	<logical AND expression> <logical OR expression’>

********************************************************************************************************************************/
void logical_OR_expression()
{

	logical_AND_expression();
	logical_OR_expressionP();

}

/**********************************************************************************************************************************
Purpose: parse the <logical OR expression'>
Author: Yucong Yin
History/Versions: 1.0
Called functions: 	match(),logical_AND_expression(),logical_OR_expressionP(),gen_incode()
Parameters: N/A
Return value: N/A
Algorithm:
<logical OR expression’> ->
	.OR. <logical AND expression> <logical OR expression’> | ϵ

********************************************************************************************************************************/
void logical_OR_expressionP()
{
	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case OR:
			match(LOG_OP_T, OR);
			logical_AND_expression();
			logical_OR_expressionP();
			gen_incode("PLATY: Logical OR expression parsed");
			break;
		
		}

	default: 
		break;
	}
	

}


/**********************************************************************************************************************************
Purpose: parse the logical AND expression
Author: Yucong Yin
History/Versions: 1.0
Called functions: 	relational_expression(),logical_AND_expressionP();
Parameters: N/A
Return value: N/A
Algorithm:
<logical AND expression> ->
	<relational expression> <logical AND expression’>
********************************************************************************************************************************/
void logical_AND_expression()
{
	relational_expression();
	logical_AND_expressionP();

}

/**********************************************************************************************************************************
Purpose: parse the <logical AND expression'>
Author: Yucong Yin
History/Versions: 1.0
Called functions: 	match(),relational_expression(),logical_AND_expressionP(),gen_incode()
Parameters: N/A
Return value: N/A
Algorithm:
<logical AND expression’> ->
	.AND. <relational expression> <logical AND expression’>
	| ϵ

********************************************************************************************************************************/
void logical_AND_expressionP()
{
	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case AND:
			match(LOG_OP_T, AND);
			relational_expression();
			logical_AND_expressionP();
			gen_incode("PLATY: Logical AND expression parsed");
			break;

		}

	default:
		break;
	}

}

/**********************************************************************************************************************************
Purpose: parse the relational expression
Author: Yucong Yin
History/Versions: 1.0
Called functions: 		primary_a_relational_expression(),primary_a_relational_expressionP(),gen_incode(),syn_printe()
Parameters: N/A
Return value: N/A
Algorithm:
<relational expression> ->
<primary a_relational expression> <primary a_relational expression’>
| <primary s_relational expression><primary s_relational expression’>
FIRST(<relational expression>) = { FIRST(<primary a_relational expression>),   FIRST(primary s_relational expression>) } 
= { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
********************************************************************************************************************************/
void relational_expression()
{
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();
		primary_a_relational_expressionP();
		break;
	case SVID_T:
	case STR_T:
		primary_s_relational_expression();
		primary_s_relational_expressionP();
		break;
	default: 
		syn_printe();
		break;
	}
	gen_incode("PLATY: Relational expression parsed");

}


/**********************************************************************************************************************************
Purpose: parse the primary a relational expression
Author: Yucong Yin
History/Versions: 1.0
Called functions: match(),gen_incode(),syn_printe()
Parameters: N/A
Return value: N/A
Algorithm:
<primary a_relational expression> ->
  AVID_T
| FPL_T
| INL_T

********************************************************************************************************************************/
void primary_a_relational_expression()
{
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T: 
		match(lookahead.code, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}


/**********************************************************************************************************************************
Purpose: parse the <primary a relational expression'>
Author: Yucong Yin
History/Versions: 1.0
Called functions: match(),primary_a_relational_expression(),syn_printe()
Parameters: N/A
Return value: N/A
Algorithm:
FIRST(<primary a_relational expression’>) = { ==, <>, >, < }
********************************************************************************************************************************/
void primary_a_relational_expressionP()
{
	switch (lookahead.code)
	{
	case REL_OP_T:
		switch (lookahead.attribute.rel_op)
		{
		case EQ:
		case NE:
		case GT:
		case LT:
			match(REL_OP_T, lookahead.attribute.rel_op);
			primary_a_relational_expression();
			break;
		default:
			syn_printe();
			break;
		}
		break;
	default:
		break;
	}
}

/**********************************************************************************************************************************
Purpose: parse the primary s relational expression
Author: Yucong Yin
History/Versions: 1.0
Called functions: primary_string_expression(),syn_printe()
Parameters: N/A
Return value: N/A
Algorithm:
<primary s_relational expression> ->
<primary string expression>

********************************************************************************************************************************/
void primary_s_relational_expression()
{
	switch (lookahead.code) {
	case SVID_T:
	case STR_T:
		primary_string_expression();
		break;
	default:
		syn_printe();
		break;
	}
	
	gen_incode("PLATY: Primary s_relational expression parsed");
}

/**********************************************************************************************************************************
Purpose: parse the <primary s relational expression'>
Author: Yucong Yin
History/Versions: 1.0
Called functions: match(),primary_s_relational_expression(),syn_printe()
Parameters: N/A
Return value: N/A
Algorithm:
FIRST(<primary s_relational expression’>) = { ==, <>, >, < }
********************************************************************************************************************************/
void primary_s_relational_expressionP()
{


	switch (lookahead.code)
	{
	case REL_OP_T:
		switch (lookahead.attribute.rel_op)
		{
		case EQ:
		case NE:
		case GT:
		case LT:
			match(REL_OP_T, lookahead.attribute.rel_op);
			primary_s_relational_expression();
			break;
		default:
			syn_printe();
			break;
		}
		break;
	default:
		break;
	}
}


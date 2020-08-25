/******************************************************************************************************************************
File name: parse.h
Compiler: MS Visual Studio 2019 Enterprise
Author: Yucong Yin, 040792791
Course: CST 8152 ¨C Compilers, Lab Section: 013
Assignment: Assignment 3
Date: 2020-Aug-7
Professor: Paulo Sousa
Purpose: To build a parser with c language

You are cordially invited to (in other words, you have to) write a Recursive Descent
Predictive Parser (RDPP) for the PLATYPUS language. You are to integrate the
Parser with your existing lexical analyzer in order to complete the front-end of your
PLATYPUS compiler. The implementation is broken into two tasks.


Function list: void parser();match();syn_eh();syn_printe();gen_incode();program();opt_statements();statements();statement();
statementsP();assignment_statement();assignment_expression();selection_statement();iteration_statement();pre_condition();input_statement();
variable_list();variable_listP();variable_identifier();output_statement();output_statementP();opt_variable_list();arithmetic_expression();unary_arithmetic_expression();
additive_arithmetic_expression();additive_arithmetic_expressionP();multiplicative_arithmetic_expression();primary_arithmetic_expression();
multiplicative_arithmetic_expressionP();string_expression();string_expressionP();primary_string_expression();conditional_expression();logical_OR_expression();
logical_OR_expressionP();logical_AND_expression();logical_AND_expressionP();relational_expression();primary_a_relational_expression();primary_a_relational_expressionP();
primary_s_relational_expression();primary_s_relational_expressionP();


*********************************************************************************************************************************/


#ifndef PARSER_H_
#define PARSER_H_
#endif

#include "buffer.h"
#include "token.h"

//constants
#define NO_ATTR -1
#define ELSE 0
#define FALSE 1
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9

static Token lookahead;
int synerrno;
extern Token malar_next_token();//from scanner.c
extern char* kw_table[];//from table.h
extern int line;//from scanner.c
extern pBuffer str_LTBL; /*String literal table */

//function declaration 
void parser();
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe();
void gen_incode(char* string);
void program();
void opt_statements();
void statements();
void statement();
void statementsP();
void assignment_statement();
void assignment_expression();
void selection_statement();
void iteration_statement();
void pre_condition();
void input_statement();
void variable_list();
void variable_listP();
void variable_identifier();
void output_statement();
void output_statementP();
void opt_variable_list();
void arithmetic_expression();
void unary_arithmetic_expression();
void additive_arithmetic_expression();
void additive_arithmetic_expressionP();
void multiplicative_arithmetic_expression();
void primary_arithmetic_expression();
void multiplicative_arithmetic_expressionP();
void string_expression();
void string_expressionP();
void primary_string_expression();
void conditional_expression();
void logical_OR_expression();
void logical_OR_expressionP();
void logical_AND_expression();
void logical_AND_expressionP();
void relational_expression();
void primary_a_relational_expression();
void primary_a_relational_expressionP();
void primary_s_relational_expression();
void primary_s_relational_expressionP();
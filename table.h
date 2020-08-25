/*
File name: table.h
Compiler: MS Visual Studio 2019 Enterprise
Author: Yucong Yin, 040792791
Course: CST 8152 �C Compilers, Lab Section: 013
Assignment: Assignment 2
Date: 2020-July-17
Professor: Paulo Sousa
Purpose: To build a scanner with c language

Writing a scanner part of the compiler for a programming language called PLATYPUS.
use the given lexical grammar to implement a lexical analyzer (scanner).


Function list: aa_func02(); aa_func03();aa_func05();aa_func08();aa_func10();aa_func11();aa_func12();


*/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
 *    '\0' or one of 255,0xFF,EOF
 */

 /*  Special case tokens processed separately one by one
  *  in the token-driven part of the scanner
  *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
  *  white space
  *  !!comment , ',' , ';' , '-' , '+' , '*' , '/', ## ,
  *  .AND., .OR. , SEOF,
  */

  //PLACE YOUR CONSTANT DEFINITIONS HERE IF YOU NEED ANY
#define RUNTIMEERRORCODE 1;//a non-negative number to be stored in case of run-time error
//REPLACE* ESN*and* ESR* WITH YOUR ERROR STATE NUMBER
#define ES  11 /* Error state  with no retract */
#define ER  12 /* Error state  with retract */
#define IS -1    /* Inavalid state */

/* State transition table definition */

//REPLACE* CN* WITH YOUR COLUMN NUMBER

#define TABLE_COLUMNS 6
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
    /* State 0 */  {1,6,4,ES,ES,ES},
    /* State 1 */  {1,1,1,2,3,2},
    /* State 2 */  {IS,IS,IS,IS,IS,IS},
    /* State 3 */  {IS,IS,IS,IS,IS,IS},
    /* State 4 */  {ES,4,4,7,5,5},
    /* State 5 */  {IS,IS,IS,IS,IS,IS},
    /* State 6 */  {ES,6,ES,7,5,5},
    /* State 7 */  {8,7,7,8,8,8},
    /* State 8 */  {IS,IS,IS,IS,IS,IS},
    /* State 9 */  {9,9,9,9,9,10},
    /* State 10 */  {IS,IS,IS,IS,IS,IS},
    /* State 11*/  {IS,IS,IS,IS,IS,IS},
    /* State 12 */  {IS,IS,IS,IS,IS,IS}

};

/* Accepting state table definition */
//REPLACE * N1*, *N2*,and* N3 * WITH YOUR NUMBERS
#define ASWR     97  /* accepting state with retract */
#define ASNR     98  /* accepting state with no retract */
#define NOAS     99  /* not accepting state */

int as_table[] = {
    /*STATE0*/NOAS,
    /*STATE1*/NOAS,
    /*STATE2*/ASWR,
    /*STATE3*/ASNR,
    /*STATE4*/NOAS,
    /*STATE5*/ASWR,
    /*STATE6*/NOAS,
    /*STATE7*/NOAS,
    /*STATE8*/ASWR,
    /*STATE9*/NOAS,
    /*STATE10*/ASNR,
    /*STATE11*/ASNR,
    /*STATE12*/ASWR };

/* Accepting action function declarations */

/*FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
ONE FUNCTION PROTOTYPE.THEY ALL RETURN Token AND TAKE
ONE ARGUMENT : A string REPRESENTING A TOKEN LEXEME.*/
//the provided argument in table.h and scanner.c does not match, so i have to change them into the array
Token aa_func02(char lexeme[]);
Token aa_func03(char lexeme[]);
Token aa_func05(char lexeme[]);
Token aa_func08(char lexeme[]);
Token aa_func10(char lexeme[]);
Token aa_func11(char lexeme[]);
Token aa_func12(char lexeme[]);

//Replace XX with the number of the accepting state : 02, 03 and so on.

/* defining a new type: pointer to function (of one char * argument)
   returning Token
*/

typedef Token(*PTR_AAF)(char* lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */

PTR_AAF aa_table[] = {


    /*HERE YOU MUST PROVIDE AN INITIALIZATION FOR AN ARRAY OF POINTERS
    TO ACCEPTING FUNCTIONS.THE ARRAY HAS THE SAME SIZE AS as_table[].
    YOU MUST INITIALIZE THE ARRAY ELEMENTS WITH THE CORRESPONDING
    ACCEPTING FUNCTIONS(FOR THE STATES MARKED AS ACCEPTING IN as_table[]).
    THE REST OF THE ELEMENTS MUST BE SET TO NULL.*/
    /*STATE0*/NULL,
    /*STATE1*/NULL,
    /*STATE2*/aa_func02,
    /*STATE3*/aa_func03,
    /*STATE4*/NULL,
    /*STATE5*/aa_func05,
    /*STATE6*/NULL,
    /*STATE7*/NULL,
    /*STATE8*/aa_func08,
    /*STATE9*/NULL,
    /*STATE10*/aa_func10,
    /*STATE11*/aa_func11,
    /*STATE12*/aa_func12

};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char* kw_table[] =
{
"ELSE",
"FALSE",
"IF",
"PLATYPUS",
"READ",
"REPEAT",
"THEN",
"TRUE",
"WHILE",
"WRITE"
};

#endif

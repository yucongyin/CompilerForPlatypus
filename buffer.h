/*
File name: buffer.c
Compiler: MS Visual Studio 2019 Enterprise
Author: Yucong Yin, 040792791
Course: CST 8152 ¨C Compilers, Lab Section: 013
Assignment: Assignment 1
Date: 2020-Jun-8
Professor: Sv. Ranev
Purpose: To build a text buffer with c language

This is a review of and an exercise in C coding style, programming techniques, data types and
structures, memory management, and simple file input/output. It will give you a better
understanding of the type of internal data structures used by a simple compiler you will be building
this semester. This assignment will be also an exercise in ¡°excessively defensive programming¡±.
You are to write functions that should be ¡°overly¡± protected and should not abruptly terminate or
¡°crash¡± at run-time due to invalid function parameters, erroneous internal calculations, or memory
violations.


Function list: b_allocate();b_addc();b_clear();b_free();b_isfull();b_addcoffset();b_capacity();b_markc();b_mode();
 b_incfactor();b_load(); b_isempty();b_getc(); b_eob(); b_print(); b_compact(); b_rflag();b_retract();b_reset(); b_getcoffset();b_rewind();
 b_location();


*/

#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 (-1)         /* operation failure return value 1 */
#define RT_FAIL_2 (-2)         /* operation failure return value 2 */
#define LOAD_FAIL (-2)         /* load fail return value */

#define DEFAULT_INIT_CAPACITY 200   /* default initial buffer capacity */
#define DEFAULT_INC_FACTOR 15       /* default increment factor */


/* You should add your own constant definitions here */
#define ZERO 0 /*minimum init_capacity range*/
#define ONE 1 /*minimum inc_factor range*/
#define MAXIMUM_ALLOWED_POS_VAL_M1 (SHRT_MAX-1) /*maximum init_capacity range with data type short*/
#define FIXED_SIZE 'f' /*fixed mode first letter*/
#define ADDITIVE_SELF_INCREMENTING 'a'/*additive mode first letter*/
#define MULTIPLICATIVE_SELF_INCREMENTING 'm'/*multiplicative mode first letter*/

#define A_INC_FACTOR_MAX 255 /*additive mode maximum inc_factor range*/
#define M_INC_FACTOR_MAX 100 /*multiplicative mode maximum inc_factor range*/

#define FIXED 0/*operation mode: fixed*/
#define ADDITIVE   1/*operation mode: additive*/
#define MULTIPLICATIVE -1/*operation mode: multiplicative*/

#define ERROR_CODE_INCFACTOR 256/*0X100*/ 
#define B_FULL/*MACRO function for b_isfull()*/
/* Add your bit-masks constant definitions here */
#define DEFAULT_FLAGS  0xFFF9/*1111,1111,1111,1001*/
#define SET_EOB  0x0002/*0000,0000,0000,0010*/
#define RESET_EOB 0xFFFD/*1111,1111,1111,1101*/
#define CHECK_EOB 0x0002/*0000,0000,0000,0010*/
#define SET_R_FLAG  0x0004/*0000,0000,0000,0100*/
#define RESET_R_FLAG 0xFFFB/*1111,1111,1111,1011*/
#define CHECK_R_FLAG 0x0004/*0000,0000,0000,0100*/

/* user data type declarations */
typedef struct BufferDescriptor {
    char* cb_head;   /* pointer to the beginning of character array (character buffer) */
    short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
    short addc_offset;  /* the offset (in chars) to the add-character location */
    short getc_offset;  /* the offset (in chars) to the get-character location */
    short markc_offset; /* the offset (in chars) to the mark location */
    char  inc_factor; /* character array increment factor */
    char  mode;       /* operational mode indicator*/
    unsigned short flags;     /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, *pBuffer;
/*typedef Buffer *pBuffer;*/

/* function declarations */
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer* const pBD);
void b_free(Buffer* const pBD);
#ifndef B_FULL
int b_isfull(Buffer* const pBD);
#else
#define b_isfull(pBD)(pBD?pBD->addc_offset == pBD->capacity?1:0:RT_FAIL_1)
#endif // !B_FULL


short b_addcoffset(Buffer* const pBD);
short b_capacity(Buffer* const pBD);
short b_markc(pBuffer const pBD, short mark);
int b_mode(Buffer* const pBD);
size_t b_incfactor(Buffer* const pBD);
int b_load(FILE* const fi, Buffer* const pBD);
int b_isempty(Buffer* const pBD);
char b_getc(Buffer* const pBD);
int b_eob(Buffer* const pBD);
int b_print(Buffer* const pBD, char nl);
Buffer* b_compact(Buffer* const pBD, char symbol);
char b_rflag(Buffer* const pBD);
short b_retract(Buffer* const pBD);
short b_reset(Buffer* const pBD);
short b_getcoffset(Buffer* const pBD);
int b_rewind(Buffer* const pBD);
char* b_location(Buffer* const pBD, short loc_offset);


#endif





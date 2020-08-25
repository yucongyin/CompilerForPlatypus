/*
File name: buffer.c
Compiler: MS Visual Studio 2019 Enterprise
Author: Yucong Yin, 040792791
Course: CST 8152 每 Compilers, Lab Section: 013
Assignment: Assignment 1
Date: 2020-Jun-8
Professor: Sv. Ranev
Purpose: To build a text buffer with c language

This is a review of and an exercise in C coding style, programming techniques, data types and
structures, memory management, and simple file input/output. It will give you a better
understanding of the type of internal data structures used by a simple compiler you will be building
this semester. This assignment will be also an exercise in ※excessively defensive programming§.
You are to write functions that should be ※overly§ protected and should not abruptly terminate or
※crash§ at run-time due to invalid function parameters, erroneous internal calculations, or memory
violations.


Function list: b_allocate();b_addc();b_clear();b_free();b_isfull();b_addcoffset();b_capacity();b_markc();b_mode();
 b_incfactor();b_load(); b_isempty();b_getc(); b_eob(); b_print(); b_compact(); b_rflag();b_retract();b_reset(); b_getcoffset();b_rewind();
 b_location();


*/
#include "buffer.h"


/*
Purpose: This function creates a new buffer in memory (on the program heap).
Author: Yucong Yin
History/Versions: 3.0
Called functions: calloc(); malloc(); free()
Parameters: short init_capacity: initial capacity of the buffer
			char inc_factor: initial increment factor of the buffer
			char o_mode: operational mode of the buffer
Return value: Buffer* a pointer to the buffer structure, NULL is run-time error
Algorithm:
- tries to allocate memory for one Buffer structure using calloc();
- tries to allocates memory for one dynamic character buffer (character array) calling malloc() with
the given initial capacity init_capacity.
-sets the Buffer structure operational mode indicator mode and the inc_factor.
- copies the given init_capacity value into the Buffer structure capacity variable;
- sets the flags field to its default value which is FFF9 hexadecimal.
- Finally, on success, the function returns a pointer to the Buffer structure.

*/

Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode)
{

	/*tries to allocate memory for one Buffer structure using calloc();*/
	pBuffer buffer = (Buffer*)calloc(1, sizeof(Buffer));

	/* The range of the parameter init_capacity must be between 0 and the MAXIMUM ALLOWED POSITIVE VALUE 每 1 inclusive*/
	if (init_capacity >= ZERO && init_capacity <= MAXIMUM_ALLOWED_POS_VAL_M1) {
		buffer->cb_head = (char*)malloc(init_capacity * sizeof(char));
		buffer->inc_factor = inc_factor;
		buffer->capacity = init_capacity;
		/*If the init_capacity is 0,
		the function tries to create a character buffer with default size 200 characters .*/
		if (init_capacity == 0) {
			init_capacity = DEFAULT_INIT_CAPACITY;
			buffer->cb_head = (char*)malloc(DEFAULT_INIT_CAPACITY * sizeof(char));
			/*. If the init_capacity is 0, the function ignores the current value of the parameter inc_factor and sets the buffer structure
			 inc_factor to 15 in mode a and m or to 0 in mode f.*/
			if (o_mode == ADDITIVE_SELF_INCREMENTING || o_mode == MULTIPLICATIVE_SELF_INCREMENTING) {
				buffer->inc_factor = DEFAULT_INC_FACTOR;
				inc_factor = DEFAULT_INC_FACTOR;
			}
			else if (o_mode == FIXED_SIZE)
				buffer->inc_factor = 0;
		}


	}
	else {
		free(buffer);
		return NULL;
	}
	/* If the o_mode is the symbol f, the mode and the buffer inc_factor are set to number 0. */
	if (o_mode == FIXED_SIZE || (inc_factor == 0 && init_capacity != 0) || (unsigned char)buffer->inc_factor == 0) {
		buffer->mode = 0;
		buffer->inc_factor = 0;

	}
	else if (o_mode == ADDITIVE_SELF_INCREMENTING && (unsigned char)buffer->inc_factor >= ONE && (unsigned char)buffer->inc_factor <= A_INC_FACTOR_MAX) {
		buffer->mode = 1;
		buffer->inc_factor = inc_factor;

	}
	else if (o_mode == MULTIPLICATIVE_SELF_INCREMENTING && (unsigned char)buffer->inc_factor >= ONE && (unsigned char)buffer->inc_factor <= M_INC_FACTOR_MAX) {
		buffer->mode = -1;
		buffer->inc_factor = inc_factor;

	}
	else {
		free(buffer);
		return NULL;
	}

	buffer->capacity = init_capacity;
	buffer->flags = DEFAULT_FLAGS;


	return buffer;

}

/*
Purpose: Using a bitwise operation the function resets the flags field r_flag bit to 0 and tries to add the
character symbol to the character array of the given buffer pointed by pBD.
Author: Yucong Yin
History/Versions: 2.0
Called functions: realloc()
Parameters:
			pBuffer const pBD : constant buffer structure pointer
			char symbol: the symbol to be added to the end of the buffer
Return value: pBuffer, return the buffer; return NULL if run-time error
Algorithm:
- If the character buffer is already full, the function will try to resize the buffer by increasing the
current capacity to a new capacity. How the capacity is increased depends on the current
operational mode of the buffer.
- If the operational mode is 0, the function returns NULL.
- If the operational mode is 1, it tries to increase the current capacity of the buffer to a new capacity
by adding inc_factor (converted to bytes) to capacity

- If the result from the operation is negative, it returns NULL.
- If the operational mode is -1 it tries to increase the current capacity of the buffer to a new capacity
- in the following manner:
- If the current capacity can not be incremented anymore because it has already reached the
maximum capacity of the buffer, the function returns NULL.
- The function tries to increase the current capacity using the following formulae:
	available space = maximum buffer capacity 每 current capacity
	new increment = available space * inc_factor / 100
	new capacity = current capacity + new increment
- The function must return NULL on any error.

*/
pBuffer b_addc(pBuffer const pBD, char symbol)
{
	short newCapacity = 0;
	short newIncrement = 0;
	short availableSpace = 0;
	char* tempCharBuffer = NULL;
	/*Make sure the buffer is operational*/
	if (!pBD) {
		return NULL;
	}
	/*bitwise operation resets the flags field bit to 0*/
	pBD->flags &= RESET_R_FLAG;
	/*if the buffer is not full, add the content to the character buffer and returns*/
	if (pBD->addc_offset < pBD->capacity)
	{
		pBD->cb_head[pBD->addc_offset++] = symbol;
		return pBD;

	}
	/*if the buffer is full, resize the buffer depending on mode*/

	switch (pBD->mode) {
		/*if operation mode is 0,return NULL*/
	case FIXED:
		return NULL;
		break;
		/*if operation mode is 1*/
	case ADDITIVE:
		/*If the result from the operation is positive and does not exceed the MAXIMUM ALLOWED POSITIVE VALUE 每1 (minus 1),
		the function proceeds. */
		newCapacity = pBD->capacity + (unsigned char)(pBD->inc_factor);
		/*which means if new capacity is not positive, it will fail*/
		if (newCapacity <= 0)
			return NULL;
		/*if it exceed max allow positive value -1, assign it to new capacity*/
		else if (newCapacity > MAXIMUM_ALLOWED_POS_VAL_M1)
			newCapacity = MAXIMUM_ALLOWED_POS_VAL_M1;


		break;

		/*if the operational mode is -1*/
	case MULTIPLICATIVE:
		/*if the current capacity reaches the max, return NULL*/
		if (pBD->capacity == MAXIMUM_ALLOWED_POS_VAL_M1) {
			return NULL;
		}
		availableSpace = MAXIMUM_ALLOWED_POS_VAL_M1 - pBD->capacity;
		/*for inc_factor is a char type variable and newIncrement is a short type, to make sure the calculation
		is correct, cast inc_factor to unsigned long bytes*/
		newIncrement = availableSpace * ((unsigned long)(pBD->inc_factor)) / 100;

		/*if the new capacity has been incremented successfully*/
		if (pBD->capacity + newIncrement <= MAXIMUM_ALLOWED_POS_VAL_M1) {
			newCapacity = pBD->capacity + newIncrement;
			/*if current capacity cannot be incremented(current capacity + new Increment exceed maximum)*/
			/*but the current capacity is still smaller than maximum(there is still space)*/
		}
		else if (pBD->capacity + newIncrement > MAXIMUM_ALLOWED_POS_VAL_M1 && availableSpace > 0) {
			newCapacity = MAXIMUM_ALLOWED_POS_VAL_M1;
		}
		break;


	default:
		return NULL;
		break;


	}
	/*the function will only reach this part of code when mode 1 or -1 is successful*/

	/*reallocating with new capacity*/
	tempCharBuffer = (char*)realloc(pBD->cb_head, sizeof(char) * newCapacity);
	if (!tempCharBuffer) {
		return NULL;
	}/*if address has been changed, set r_flag to 1*/
	if (tempCharBuffer != pBD->cb_head) {
		pBD->flags |= SET_R_FLAG;
		pBD->cb_head = tempCharBuffer;
	}
	pBD->cb_head[pBD->addc_offset++] = symbol;
	pBD->capacity = newCapacity;//performing the incrementing





	return pBD;
}

/*
Purpose: The function retains the memory space currently allocated to the buffer, but re-initializes all
appropriate data members of the given Buffer structure (buffer descriptor) so that the buffer will
appear as just created to the client functions
Author: Yucong Yin
History/Versions: 1.0
Called functions: NA
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: int, -1 if any run-time error, 0 if it works
Algorithm:

*/
int b_clear(Buffer* const pBD)
{
	/*if crashes, return -1 to warn*/
	if (!pBD) {
		return RT_FAIL_1;
	}
	pBD->flags = DEFAULT_FLAGS;
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;


	return 0;
}

/*
Purpose: The function de-allocates (frees) the memory occupied by the character buffer and the Buffer
structure (buffer descriptor).
Author: Yucong Yin
History/Versions: 1.0
Called functions: free();
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: void
Algorithm:

*/
void b_free(Buffer* const pBD)
{


	if (pBD) {


		free(pBD->cb_head);
		free(pBD);
	}

}

/*
Purpose: The function returns 1 if the character buffer is full; it returns 0 otherwise. If a run-time error is
possible, the function should return -1.
Author: Yucong Yin
History/Versions: 1.0
Called functions: NA
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: int, -1 if run-time errir, 1 if buffer is full, 0 if it is not
Algorithm:
*/
#ifndef B_FULL



int b_isfull(Buffer* const pBD)
{
	printf("Reach function");
	/*return -1 if run-time error*/
	if (!pBD)
		return RT_FAIL_1;
	/*if character array is full, which means the distance from beginning to the next to be added position is equal to the capacity*/
	if (pBD->addc_offset == pBD->capacity)
		return 1;
	else
		return 0;

}
#endif // !B_FULL
/*
Purpose: The function returns the current addc_offset. If a run-time error is possible, the function should
return -1.

Author: Yucong Yin
History/Versions: 1.0
Called functions: NA
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: short, return the current add offset of the buffer, return -1 if run-time error
Algorithm:

*/
short b_addcoffset(Buffer* const pBD)
{
	/*return -1 if run-time error*/
	if (!pBD)
		return RT_FAIL_1;

	return pBD->addc_offset;
}

/*
Purpose: The function returns the current capacity of the character buffer. If a run-time error is possible, the
function should return -1.

Author: Yucong Yin
History/Versions: 1.0
Called functions: NA
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: short. return the current capacity of the buffer, return -1 if run-time error
Algorithm:

*/
short b_capacity(Buffer* const pBD)
{
	/*return -1 if run-time error*/
	if (!pBD)
		return RT_FAIL_1;

	return pBD->capacity;
}

/*
Purpose: The function sets markc_offset to mark.
Author: Yucong Yin
History/Versions: 1.0
Called functions: NA
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: short, return the current set mark offset, return -1 if run-time error
Algorithm: [outline the main steps (sections) only; do not include implementation
details; for small and clear functions leave this empty]

*/
short b_markc(pBuffer const pBD, short mark)
{
	/*return -1 if run-time error*/
	if (!pBD)
		return RT_FAIL_1;

	if (mark >= 0 && mark <= pBD->addc_offset) {

		return pBD->markc_offset = mark;
	}
	else
		return RT_FAIL_1;


}

/*
Purpose: The function returns the value of mode to the calling function.
Author: Yucong Yin
History/Versions: 1.0
Called functions: NA
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: int. return 0,1,-1 depending on the mode, return -2 if runtime error
Algorithm:

*/
int b_mode(Buffer* const pBD)
{	/*return -2 if run-time errorㄗ-1 is a mode optionㄘ*/
	if (!pBD)
		return RT_FAIL_2;

	return pBD->mode;
}

/*
Purpose: The function returns the non-negative value of inc_factor to the calling function.
Author: Yucong
History/Versions: 1.0
Called functions: NA
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: size_t return the non-negative value of increment factor of the buffer converted to bytes
Algorithm:

*/
size_t b_incfactor(Buffer* const pBD)
{
	/*return 0X100 if run-time error*/
	if (!pBD)
		return ERROR_CODE_INCFACTOR;
	return (unsigned char)(pBD->inc_factor);
}

/*
Purpose: The function loads (reads) an open input file specified by fi into a buffer specified by pBD
Author: Yucong Yin
History/Versions: 3.0
Called functions: feof(); fgetc(); b_addc(); ungetc();
Parameters: FILE* const fi: The file pointer
			Buffer* const pBD: constant buffer structure pointer
Return value: int, return -1 if run-time error, or return the number of characters have been loaded.
Algorithm:
- The function must use the standard function fgetc(fi) to read one character at a time and the function
b_addc() to add the character to the buffer.
- If the current character cannot be added to the buffer
(b_addc() returns NULL), the function returns the character to the file stream (file buffer) using
ungetc() library function and then returns -2 (use the defined LOAD_FAIL constant).
- The operation is repeated until the standard macro feof(fi) detects end-of-file on the input file.

*/
int b_load(FILE* const fi, Buffer* const pBD)
{
	char character;
	int counter = 0;
	/*for any other run-time error, return -1*/
	if (!pBD || !fi)
		return RT_FAIL_1;
	/*the operation is repeated until feof(fi) detects the eof*/
	while (!feof(fi)) {
		character = (char)fgetc(fi);
		/*the eof character must not be added*/
		if (feof(fi))
			break;
		/*if the current character cannot be added to the buffer*/
		if (!b_addc(pBD, character)) {

			ungetc(character, fi);
			return LOAD_FAIL;
		}
		counter++;

	}
	return counter;
}

/*
Purpose: To check if the buffer is empty
Author: Yucong Yin
History/Versions: 1.0
Called functions: NA
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: int. Return 0 if it is not empty, -1 if run-time error, otherwise return 1
Algorithm:

*/
int b_isempty(Buffer* const pBD)
{
	/*return -1 if run-time error*/
	if (!pBD)
		return RT_FAIL_1;


	if (pBD->addc_offset == 0)
		return 1;
	else
		return 0;
}

/*
Purpose: This function is used to read the buffer.
Author: Yucong Yin
History/Versions: 2.0
Called functions: NA
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: char. returns -2 for non-validation. otherwise return the character located at getc_offset of the buffer
Algorithm:
- checks the argument for validity (possible run-time error). If it is not valid, it returns -2;
- if getc_offset and addc_offset are equal, using a bitwise operation it sets the flags field eob bit
to 1 and returns number 0; otherwise, using a bitwise operation it sets eob to 0;
- returns the character located at getc_offset. Before returning it increments getc_offset by 1.

*/
char b_getc(Buffer* const pBD)
{

	/*return -2 if run-time error*/
	if (!pBD)
		return RT_FAIL_2;
	/*if getc_offset is equal to addc_offset, set eob flag to 1 and return 0*/
	if (pBD->getc_offset == pBD->addc_offset) {
		pBD->flags |= SET_EOB;
		return 0;
	}
	else
		pBD->flags &= RESET_EOB;
	return pBD->cb_head[pBD->getc_offset++];

}

/*
Purpose: The function returns the value of the flags field determined only by the eob bit.
Author: Yucong Yin
History/Versions: 1.0
Called functions: NA
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: int returen the current eob flag; return -1 if run-time error
Algorithm:

*/
int b_eob(Buffer* const pBD)
{
	int eob;
	/*return -1 if run-time error*/
	if (!pBD)
		return RT_FAIL_1;
	eob = pBD->flags & CHECK_EOB;
	return eob;
}

/*
Purpose:  Using the printf() library
function the function prints character by character the contents of the character buffer to the
standard output (stdout).
Author: Yucong Yin
History/Versions: 1.0
Called functions: b_getc();b_eob();printf();
Parameters: Buffer* const pBD: constant buffer structure pointer
			char nl: if nl is not 0, print a new line
Return value: int.return -1 if run-time error, otherwise print the number of characters that has been printed.
Algorithm:
- check run-time error
- create a loop until the end of buffer
- in the loop print out characters

*/
int b_print(Buffer* const pBD, char nl)
{
	char character;
	int counter = 0;
	/*return -1 if run-time error*/
	if (!pBD)
		return RT_FAIL_1;
	//looping until eob flag is 1
	do {
		character = b_getc(pBD);

		if (b_eob(pBD)) {
			break;
		}


		printf("%c", character);
		counter++;
	} while (!b_eob(pBD));
	/*if nl is not 0*/
	if (nl != 0)
		printf("\n");

	return counter;
}

/*
Purpose: For all operational modes of the buffer the function shrinks (or in some cases may expand) the
buffer to a new capacity.
Author: Yucong Yin
History/Versions: 1.0
Called functions: realloc();
Parameters: Buffer* const pBD: constant buffer structure pointer
			char symbol: symbol to be added tothe end of character buffer
Return value: Buffer*: a pointer to the buffer. return NULL if run-time error, otherwise return the buffer
Algorithm:
 - check buffer validation
 - calculate new capacity
 - reallocate memory using new capacity
 - reset parameters
 - return buffer

*/
Buffer* b_compact(Buffer* const pBD, char symbol)
{
	short newCapacity;
	char* tempCharBuffer;

	/*return NULL if run-time error*/
	if (!pBD)
		return NULL;
	newCapacity = (unsigned short)(pBD->addc_offset + 1);
	tempCharBuffer = (char*)realloc(pBD->cb_head, newCapacity * sizeof(char));
	if (!tempCharBuffer)
		return NULL;
	if (tempCharBuffer != pBD->cb_head) {
		pBD->flags |= SET_R_FLAG;
		pBD->cb_head = tempCharBuffer;
	}
	pBD->cb_head[pBD->addc_offset++] = symbol;
	pBD->capacity = newCapacity;//performing the incrementing

	return pBD;

}

/*
Purpose: The function returns the value of the flags field determined only by the r_flag bit.
Author: Yucong Yin
History/Versions: 1.0
Called functions: NA
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: char. the r_flag bit of the flag field of the buffer
Algorithm:

*/
char b_rflag(Buffer* const pBD)
{
	int rflag;
	/*return -1 if run-time error*/
	if (!pBD)
		return RT_FAIL_1;
	rflag = pBD->flags & CHECK_R_FLAG;
	return rflag;


}

/*
Purpose: The function decrements getc_offset by 1.
Author: Yucong Yin
History/Versions: 1.0
Called functions: NA
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: short, the decremented getc_offset, return -1 if run-time error
Algorithm:

*/
short b_retract(Buffer* const pBD)
{
	/*return -1 if run-time error*/
	if (!pBD)
		return RT_FAIL_1;
	pBD->getc_offset--;
	return pBD->getc_offset;
}

/*
Purpose: The function sets getc_offset to the value of the current markc_offset .
Author: Yucong Yin
History/Versions: 1.0
Called functions: NA
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: short. return -1 if run-time error, otherwise return the current getc_offset
Algorithm:

*/
short b_reset(Buffer* const pBD)
{
	/*return -1 if run-time error*/
	if (!pBD)
		return RT_FAIL_1;
	pBD->getc_offset = pBD->markc_offset;
	return pBD->getc_offset;
}

/*
Purpose: The function returns getc_offset to the calling function.
Author: Yucong Yin
History/Versions: 1.0
Called functions: NA
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: short.Return -1 if run-time error, otherwise return the curren getc_offset
Algorithm:
*/
short b_getcoffset(Buffer* const pBD)
{
	/*return -1 if run-time error*/
	if (!pBD)
		return RT_FAIL_1;
	return pBD->getc_offset;
}

/*
Purpose: The function set the getc_offset and markc_offset to 0
Author: Yucong Yin
History/Versions: 1.0
Called functions: NA
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: int. return -1 if run-time error, return 0 otherwise
Algorithm:

*/
int b_rewind(Buffer* const pBD)
{
	/*return -1 if run-time error*/
	if (!pBD)
		return RT_FAIL_1;

	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	return 0;
}

/*
Purpose: The function returns a pointer to the location of the character buffer indicated by loc_offset.
Author: Yucong Yin
History/Versions: 1.0
Called functions: NA
Parameters: Buffer* const pBD: constant buffer structure pointer
Return value: char*. return NULL if run-time error. otherwise return a pointer to the current location of provided paremeter.
Algorithm:
- check buffer validation
- check location parameter validation
- point to the location in the buffer and return it.

*/
char* b_location(Buffer* const pBD, short loc_offset)
{
	/*return NULL if run-time error*/
	if (!pBD)
		return NULL;
	if (loc_offset >= 0 && loc_offset < pBD->addc_offset)

		return pBD->cb_head + loc_offset;
	else
		return NULL;

}

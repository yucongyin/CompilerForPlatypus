/*
File name: table.h
Compiler: MS Visual Studio 2019 Enterprise
Author: Yucong Yin, 040792791
Course: CST 8152 ¨C Compilers, Lab Section: 013
Assignment: Assignment 2
Date: 2020-July-17
Professor: Paulo Sousa
Purpose: To build a scanner with c language

Writing a scanner part of the compiler for a programming language called PLATYPUS.
use the given lexical grammar to implement a lexical analyzer (scanner).


Function list: aa_func02(), aa_func03(),aa_func05(),aa_func08(),aa_func10(),aa_func11(),aa_func12()
char_class(),get_next_state(),iskeyword(),malar_next_token(),scanner_init()


*/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

 /*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"


#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern pBuffer str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static pBuffer lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char); /* state machine function */
static int iskeyword(char* kw_lexeme); /*keywords lookup functuion */


/*Initializes scanner */
int scanner_init(Buffer* psc_buf) {
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}


/*
Purpose: Performs the token recognition. It reads the lexeme from the input stream and returns a token structure anytime it finds a token pattern
Author: Yucong Yin
History/Versions: 5.2
Called functions: b_getc(),b_retract(),b_markc(),b_reset(),b_getcoffset(),b_allocate(),b_addc(),b_compact(),b_free(),get_next_state(),strcpy()
Parameters: void

Return value: Token, the function returns a certain token structure depends on the character it reads and defines
Algorithm:
-part1 use conditional statements to reconize all pre-defined spacial case tokens from table.h
-part2 implement Finite State Machine (DFA)  or Transition Table driven Scanner, catch illegal lexeme.
-handle error input and runtime error

*/
Token malar_next_token(void) {
	char temp;
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/

	//DECLARE YOUR LOCAL VARIABLES HERE IF NEEDED
	unsigned char commentC;
	unsigned char relaC;
	unsigned char notEqualC;
	unsigned char strC;
	while (1) { /* endless loop broken by token returns it will generate a warning */

		//GET THE NEXT SYMBOL FROM THE INPUT BUFFER

		c = b_getc(sc_buf);


		/* Part 1: Implementation of token driven scanner */
		/* every token is possessed by its own dedicated code */
		//Those input elements that are not white space or comments are tokens.
			//check SEOF
		if (c == '\0' || c == 255) {
			t.code = SEOF_T;

				t.attribute.seof = SEOF_EOF;

			return t;
		}
		//check white space
		//ASCII SP space,ASCII HT horizontal tab, ASCII VT vertival tab, ASCII FF form feed,line terminator(CR,LF)
		if (c == ' ' || c == '\t' || c == '\v' || c == '\f' || c == '\n' || c == '\r') {
			if (c == '\n')
				line++;
			continue;
		}

		//PLATYPUS supports only single-line comments:
		//a comment is a text beginning with the comment prefix characters !! and ending with a line terminator.
		if (c == '!') {
			commentC = b_getc(sc_buf);
			//IF THE FOLLOWING CHAR IS NOT !REPORT AN ERROR
			//ELSE IN A LOOP SKIP CHARACTERS UNTIL line terminator is found THEN continue;
			if (commentC != '!') {
				/*The attribute of the comment error token is a C-type string
				containing the !symbol and the symbol following !.*/
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = commentC;
				t.attribute.err_lex[2] = '\0';//C type string means it always ends with \0

			}
			/*Before returning the error token
			the scanner must ignore all of the symbols of the wrong comment to the end of the
			line.*/
			while (c != '\n') {
				c = b_getc(sc_buf);
				//if reaches the end of source, terminate
				if (c == '\0' || c == 255) {
					t.code = SEOF_T;
					if (c == '\0')
						t.attribute.seof = SEOF_0;
					else
						t.attribute.seof = SEOF_EOF;

					return t;
				}
			}
			//after the while loop c is a line terminal mark
			line++;
			//at the end of line,if it is not a comment sign, report error
			if (commentC != '!')
				return t;
			//if no error, continue
			continue;
		}

		//all the input elements and token
	   /*'=' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
		*
		*',', ';', '-', '+', '*', '/', ##,
		* .AND., .OR. */
		switch (c) {
			//assignment operator or relational operator
		case '=':
			relaC = b_getc(sc_buf);
			if (relaC == '=') {
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}
			//go back to =
			b_retract(sc_buf);
			t.code = ASS_OP_T;

			return t;
		case '(':
			t.code = LPR_T;
			return t;
		case ')':
			t.code = RPR_T;
			return t;
		case '{':
			t.code = LBR_T;
			return t;
		case '}':
			t.code = RBR_T;
			return t;
			//less than or not equal
		case '<':
			t.code = REL_OP_T;
			notEqualC = b_getc(sc_buf);
			if (notEqualC == '>') {
				t.attribute.rel_op = NE;
				return t;
			}
			b_retract(sc_buf);
			t.attribute.rel_op = LT;
			return t;
		case '>':
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
		case ';':
			t.code = EOS_T;
			return t;
		case ',':
			t.code = COM_T;
			return t;
			//below are arithmetic operators
		case '-':
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
		case '+':
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
		case '*':
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
		case '/':
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
			//below is ## string concatenation operator
		case '#':
			strC = b_getc(sc_buf);
			if (strC == '#') {
				t.code = SCC_OP_T;
			}
			else {
				t.code = ERR_T;
				t.attribute.err_lex[0] = '#';
				t.attribute.err_lex[1] = '\0';
				b_retract(sc_buf);
			}

			return t;
			//below is logical operator or, and 
		case '.':
			//mark the current offset
			b_markc(sc_buf, b_getcoffset(sc_buf));
			c = b_getc(sc_buf);
			//IF(c == '.') TRY TO PROCESS.AND. or .OR.
			//IF SOMETHING ELSE FOLLOWS.OR THE LAST.IS MISSING
			if (c == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.') {
				t.code = LOG_OP_T;
				t.attribute.log_op = AND;
				return t;
			}
			else if (c == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.') {
				t.code = LOG_OP_T;
				t.attribute.log_op = OR;
				return t;
			}
			//if it is not logical operator,set error report,reset to mark_offset for next character
			else {
				b_reset(sc_buf);
				t.code = ERR_T;
				t.attribute.err_lex[0] = '.';
				t.attribute.err_lex[1] = '\0';
				return t;
			}

		case '"':
			lexstart = b_markc(sc_buf, b_getcoffset(sc_buf) - 1);
			state = 9;
			while (1) {
				c = b_getc(sc_buf);

				if (c == '"') {
					state = 10;
					t.code = STR_T;
					t.attribute.str_offset = b_addcoffset(str_LTBL);
					lexend = b_getcoffset(sc_buf);
					lex_buf = b_allocate(lexend - lexstart, 0, FIXED);
					if (!lex_buf) {
						scerrnum = RUNTIMEERRORCODE;
						t.code = RTE_T;
						strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
						return t;
					}

					b_reset(sc_buf);
					int k;
					for (k = lexstart; k < lexend; k++) {
						c = b_getc(sc_buf);
						b_addc(lex_buf, c);
					}


					b_compact(lex_buf, '\0');
					t = aa_table[state](b_location(lex_buf, 0));
					b_free(lex_buf);
					return t;
				}
				//ass2w the last bug...
				if (c == 255 || c == '\0') {
					lexend = b_getcoffset(sc_buf);
					lex_buf = b_allocate(lexend - lexstart, 0, FIXED);
					if (!lex_buf) {
						scerrnum = RUNTIMEERRORCODE;
						t.code = RTE_T;
						strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
						return t;
					}
					b_reset(sc_buf);
					int g;
					for (g = lexstart; g < lexend; g++) {
						c = b_getc(sc_buf);
						b_addc(lex_buf, c);
					}


					b_compact(lex_buf, '\0');
					// b_print(lex_buf,0);
					t = aa_table[ES](b_location(lex_buf, 0));
					b_free(lex_buf);
					return t;

				}
			}


		default://default means input except for tokens which are input 
			break;



		}//end of switch


			/* Part 2: Implementation of Finite State Machine (DFA)
					   or Transition Table driven Scanner
					   Note: Part 2 must follow Part 1 to catch the illegal symbols
			*/

		lexstart = b_markc(sc_buf, b_getcoffset(sc_buf) - 1);
		//FSM0.Begin with state = 0 and the input character c.
		//FSM1.Get the next state from the transition table calling
		/*if (c == '"')
			state = 9;
	   else*/
		state = get_next_state(state, c);
		/*FSM2.Use the as_table to get the type of the state.
	   If the state is not an accepting(NOAS) state,
		 get the next character from the input buffer and repeat FSM1.*/
		while (as_table[state] == NOAS) {
			c = b_getc(sc_buf);
			state = get_next_state(state, c);
		}
		// FSM3.If the state is an accepting state, token is found, leave the machine and
		//call an accepting function

		//IF THE ACCEPTING STATE IS A RETRACTING ACCEPTING STATE
		//RETRACT  getc_offset USING AN APPROPRIATE BUFFER FUNCTION.
		if (as_table[state] == ASWR)

			b_retract(sc_buf);

		//SET lexend TO getc_offset USING AN APPROPRIATE BUFFER FUNCTION
		lexend = b_getcoffset(sc_buf);

		//CREATE  A TEMPORRARY LEXEME BUFFER HERE;
		lex_buf = b_allocate(lexend - lexstart, 0, FIXED);
		//handling run time error:
		/*In a case of run-time error, the function must store a non-negative number
		into the global variable scerrnum and return a run-time error token.
		The error token attribute must be the string ¡°RUN TIME ERROR: ¡°.*/
		if (!lex_buf) {
			scerrnum = RUNTIMEERRORCODE;
			t.code = RTE_T;
			strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
			return t;
		}
		//RETRACT getc_offset to the MARK SET PREVIOUSLY AT THE BEGINNING OF THE LEXEME
		b_reset(sc_buf);
		//USING b_getc() COPY THE LEXEME BETWEEN lexstart AND lexend 
		//FROM THE INPUT BUFFER INTO lex_buf USING b_addc(...)
	   // while (b_getcoffset(sc_buf) < lexend && b_getcoffset(sc_buf) >= lexstart) {    **for some reason does not read out of range numbers**
		int j;
		//printf("start: %d, end:  %d   state: %d",lexstart,lexend,state);
		for (j = lexstart; j < lexend; j++) {
			c = b_getc(sc_buf);
			b_addc(lex_buf, c);
		}
		b_compact(lex_buf, '\0');
		/*WHEN VID(KEYWORDS INCLUDED), FPL, IL OR SL IS RECOGNIZED
			.YOU MUST CALL THE ACCEPTING FUNCTION USING THE ARRAY aa_table, WHICH
			.CONTAINS POINTERS TO FUNCTIONS.*/

			//state indicates the function in the table and pass the string in lexeme as argument by pointing to
			//the location of the first character in lex buffer by using b_location

		t = aa_table[state](b_location(lex_buf, 0));

		b_free(lex_buf);
		return t;
	}//end while(1)
}


//  DO NOT MODIFY THE CODE OF THIS FUNCTION
//    YOU CAN REMOVE THE COMMENTS ONLY

int get_next_state(int state, char c)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	/*
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:

	Assertion failed: test, file filename, line linenum

	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	*/
	assert(next != IS);

	/*
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem.
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
	*/
#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	return next;
}


/*
Purpose: This function classify the input charater and return the type to drive the transition table
Author: Yucong Yin
History/Versions: 1.0
Called functions: isalpha(),isdigit()
Parameters: char c: the caught character of the buffer to be classified
Return value: int represents 6 different types of characters including:[a-zA-Z],0,[1-9],.,#,other
Algorithm:
-using conditional statement to classify parameter c.

*/
int char_class(char c)
{
	int val;
	if (isalpha(c))
		val = 0;
	else if (c == '0')
		val = 1;
	else if (c != '0' && isdigit(c))
		val = 2;
	else if (c == '.')
		val = 3;
	else if (c == '#')
		val = 4;
	else
		val = 5;

	return val;
}



// HERE YOU WRITE THE DEFINITIONS FOR YOUR ACCEPTING FUNCTIONS.
//  ************************************************************

  /*   */


 /*
	 Purpose: This is the ACCEPTING FUNCTION FOR THE arithmentic variable identifier AND keywords(VID - AVID / KW)
	 Author: Yucong Yin
	 History/Versions: 2.2
	 Called functions: iskeyword(),strlen(),strncpy()
	 Parameters: array lexeme, lexeme bufferred by malar_next_token() and driven by the transition table, caught in the source file
	 Return value: Token, the function set the token attibute of the lexeme after confirmation and return it.
	 Algorithm:
	 - check if the lexeme is a keyword
	 - set an AVID token


 */
Token aa_func02(char lexeme[]) {
	Token t;
	int index;



	/*WHEN CALLED THE FUNCTION MUST
	1. CHECK IF THE LEXEME IS A KEYWORD.
	IF YES, IT MUST RETURN A TOKEN WITH THE CORRESPONDING ATTRIBUTE
	FOR THE KEYWORD.THE ATTRIBUTE CODE FOR THE KEYWORD
	IS ITS INDEX IN THE KEYWORD LOOKUP TABLE(kw_table in table.h).
	IF THE LEXEME IS NOT A KEYWORD, GO TO STEP 2.*/
	index = iskeyword(lexeme);
	if (index >= 0) {
		t.code = KW_T;
		t.attribute.kwt_idx = index;
		return t;
	}


	/*2. SET a AVID TOKEN.*/
	t.code = AVID_T;
	/*IF THE lexeme IS LONGER than VID_LEN(see token.h) CHARACTERS,
	ONLY FIRST VID_LEN CHARACTERS ARE STORED
	INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[](see token.h) .
	ADD \0 AT THE END TO MAKE A C - type STRING.
	return t;*/
	int i;
	if (strlen(lexeme) > VID_LEN) {
		for (i = 0; i < VID_LEN; i++) {
			t.attribute.vid_lex[i] = lexeme[i];
		}
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	else {
		strncpy(t.attribute.vid_lex, lexeme, strlen(lexeme));
		t.attribute.vid_lex[strlen(lexeme)] = '\0';
	}

	return t;
}

/*ACCEPTING FUNCTION FOR THE string variable identifier(VID - SVID)
	REPLACE XX WITH THE CORRESPONDING ACCEPTING STATE NUMBER*/
	/*
		Purpose: This is the ACCEPTING FUNCTION FOR THE string variable identifier(VID - SVID)
		Author: Yucong Yin
		History/Versions: 1.0
		Called functions: strlen(),strncpy()
		Parameters: array lexeme, lexeme bufferred by malar_next_token() and driven by the transition table, caught in the source file
		Return value: Token, the function set the token attibute of the lexeme after confirmation and return it.
		Algorithm:

		- set a SVID token according to it's length.


	*/
Token aa_func03(char lexeme[]) {
	Token t;
	t.code = SVID_T;

	/*WHEN CALLED THE FUNCTION MUST
		1. SET a SVID TOKEN.
		IF THE lexeme IS LONGER than VID_LEN characters,
		ONLY FIRST VID_LEN - 1 CHARACTERS ARE STORED
		INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[],
		AND THEN THE # CHARACTER IS APPENDED TO THE NAME.
		ADD \0 AT THE END TO MAKE A C - type STRING.
		*/
	int i;
	if (strlen(lexeme) > VID_LEN) {
		for (i = 0; i < VID_LEN - 1; i++) {

			t.attribute.vid_lex[i] = lexeme[i];
		}
		t.attribute.vid_lex[VID_LEN - 1] = '#';
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	else {
		strncpy(t.attribute.vid_lex, lexeme, strlen(lexeme));
		t.attribute.vid_lex[strlen(lexeme)] = '\0';
	}


	return t;
}

/* ACCEPTING FUNCTION FOR THE floating - point literal(FPL)*/
 /*
	 Purpose: This is the ACCEPTING FUNCTION FOR THE floating - point literal(FPL)
	 Author: Yucong Yin
	 History/Versions: 2.0
	 Called functions: strtof()
	 Parameters: array lexeme, lexeme bufferred by malar_next_token() and driven by the transition table, caught in the source file
	 Return value: Token, the function set the token attibute of the lexeme after confirmation and return it.
	 Algorithm:

	 - convert lexeme to a floating point number
	 - make sure the number is under the proper range
	 - set the FPL token


 */
Token aa_func08(char lexeme[]) {

	Token t;
	float convert = strtof(lexeme, NULL);
	/* THE FUNCTION MUST CONVERT THE LEXEME TO A FLOATING POINT VALUE,
		 WHICH IS THE ATTRIBUTE FOR THE TOKEN.*/
		 // THE VALUE MUST BE IN THE SAME RANGE AS the value of 4 - byte float in C.
	if ((convert < FLT_MIN || convert > FLT_MAX) && (convert >= 0 && strlen(lexeme) > 7)) {
		/*set error token in ES state aa function*/
		t = aa_table[ES](lexeme);

	}
	/*IN CASE OF ERROR(OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
	THE ERROR TOKEN ATTRIBUTE IS  lexeme.IF THE ERROR lexeme IS LONGER
	than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
	STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
	err_lex C - type string.(will be implemented in ES state function)
	BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/
	else {
		t.code = FPL_T;
		t.attribute.flt_value = (float)convert;


	}

	return t;
}

/* ACCEPTING FUNCTION FOR THE integer literal(IL) - decimal constant(DIL)*/
 /*
	 Purpose: This is the ACCEPTING FUNCTION FOR THE integer literal(IL) - decimal constant(DIL)
	 Author: Yucong Yin
	 History/Versions: 1.0
	 Called functions: atoi()
	 Parameters: array lexeme, lexeme bufferred by malar_next_token() and driven by the transition table, caught in the source file
	 Return value: Token, the function set the token attibute of the lexeme after confirmation and return it.
	 Algorithm:
	 - convert the string lexeme into an integer
	 - make sure the number is under the range of 2-byte integer in C
	 - set the IL token
 */
Token aa_func05(char lexeme[]) {
	Token t;
	int convert = atoi(lexeme);
	/*THE FUNCTION MUST CONVERT THE LEXEME REPRESENTING A DECIMAL CONSTANT
		TO A DECIMAL INTEGER VALUE, WHICH IS THE ATTRIBUTE FOR THE TOKEN.
		THE VALUE MUST BE IN THE SAME RANGE AS the value of 2 - byte integer in C.
		IN CASE OF ERROR(OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
		THE ERROR TOKEN ATTRIBUTE IS  lexeme.IF THE ERROR lexeme IS LONGER
		than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
		STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
		err_lex C - type string.
		BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/
	if (convert > SHRT_MAX || convert < SHRT_MIN) {
		t = aa_table[ES](lexeme);
	}
	else {
		t.code = INL_T;
		t.attribute.int_value = convert;
	}
	return t;
}

/*ACCEPTING FUNCTION FOR THE string literal(SL)*/
 /*
	 Purpose: This is the ACCEPTING FUNCTION FOR THE string literal(SL)
	 Author: Yucong Yin
	 History/Versions: 3.1
	 Called functions: b_addcofset(),strlen(),b_addc()
	 Parameters: array lexeme, lexeme bufferred by malar_next_token() and driven by the transition table, caught in the source file
	 Return value: Token, the function set the token attibute of the lexeme after confirmation and return it.
	 Algorithm:
	 - set the string literal token
	 - buffer the string literal into the String Literal Table
	 - handle error state


 */
Token aa_func10(char lexeme[]) {
	Token t;

	/*THE FUNCTION MUST STORE THE lexeme PARAMETER CONTENT INTO THE STRING LITERAL TABLE(str_LTBL)
		FIRST THE ATTRIBUTE FOR THE TOKEN MUST BE SET.*/
		//printf("lexeme: %s", lexeme);
	t.code = STR_T;
	/*THE ATTRIBUTE OF THE STRING TOKEN IS THE OFFSET FROM
	THE BEGINNING OF THE str_LTBL char buffer TO THE LOCATION
	WHERE THE FIRST CHAR OF THE lexeme CONTENT WILL BE ADDED TO THE BUFFER.*/

	//the location of the buffer where will be added to is the addc_offset
	//use b_addcoffset() to access
	t.attribute.str_offset = b_addcoffset(str_LTBL);
	/* USING b_addc(..)COPY THE lexeme content INTO str_LTBL.

	 THE OPENING AND CLOSING " MUST BE IGNORED DURING THE COPING PROCESS.
	 ADD '\0' AT THE END MAKE THE STRING C - type string
	 IF THE STING lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
	 SET THE STRING TOKEN CODE.*/

	int i;
	
		for (i = 0; i < (int)strlen(lexeme); i++) {
			if (lexeme[i] != '"')
				b_addc(str_LTBL, lexeme[i]);

			if (lexeme[i] == '\n')
				line++;
		}

		b_addc(str_LTBL, '\0');
		t.code = STR_T;

		return t;


}

/*ACCEPTING FUNCTION FOR THE ERROR TOKEN*/
 /*
	 Purpose: This is the ACCEPTING FUNCTION FOR THE ERROR TOKEN
	 Author: Yucong Yin
	 History/Versions: 1.2
	 Called functions: strlen(),strncpy()
	 Parameters: array lexeme, lexeme bufferred by malar_next_token() and driven by the transition table, caught in the source file
	 Return value: Token, the function set the token attibute of the lexeme after confirmation and return it.
	 Algorithm:
	 - set the error token
	 - fill the error token attribute with proper length of the lexeme


 */
Token aa_func11(char lexeme[]) {
	Token t;
	//printf("lexeme: %s", lexeme);
/*THE FUNCTION SETS THE ERROR TOKEN.lexeme[] CONTAINS THE ERROR
	THE ATTRIBUTE OF THE ERROR TOKEN IS THE lexeme CONTENT ITSELF
	AND IT MUST BE STORED in err_lex.IF THE ERROR lexeme IS LONGER
	than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
	STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
	err_lex C - type string.
	IF THE ERROR lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
	BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/
	t.code = ERR_T;
	int i;
	if (strlen(lexeme) > ERR_LEN) {
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3);
		t.attribute.err_lex[ERR_LEN - 3] = '.';
		t.attribute.err_lex[ERR_LEN - 2] = '.';
		t.attribute.err_lex[ERR_LEN - 1] = '.';
		t.attribute.err_lex[ERR_LEN] = '\0';


	}
	else {

		strncpy(t.attribute.err_lex, lexeme, strlen(lexeme));
		t.attribute.err_lex[strlen(lexeme)] = '\0';
	}
	//check line terminator
	for (i = 0; i < strlen(lexeme); i++) {

		if (lexeme[i] == '\n')
			line++;
	}


	return t;
}
//will not reach here...
Token aa_func12(char lexeme[]) {
	Token t;
	t.code = ERR_T;
	return t;


}

//match the kw_lexeme with kw_table
 /*
	 Purpose: this function is used to match the kw_lexeme with kw_table
	 Author: Yucong Yin
	 History/Versions: 1.0
	 Called functions: strcmp()
	 Parameters: kw_lexeme, lexeme which could be matching keywords from the kw_table
	 Return value: int, return the index of the keyword in kw_table, return a negative value if it does not match
	 Algorithm:
	 - check the validity of lexeme
	 - match the ke_table and return the index


 */
int iskeyword(char* kw_lexeme) {
	if (!kw_lexeme)
		return -1;
	else {

		int i;
		for (i = 0; i < KWT_SIZE; i++) {
			if (strcmp(kw_table[i], kw_lexeme) == 0)
				return i;//return the index

		}
	}
	return -1;


}
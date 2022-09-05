# ABAP Reference

Code snippets and cheat sheets used for learning ABAP programming.

## Notes
Various notes:
* ABAP is not a case-sensitive language (hence `snake_case` is common instead of `camelCase`))
* ABAP Development class is now Package
* in the case of ABAP development, a project is a link to a SAP instance with a specific client, user, and language; there are two types of projects: **ABAP project** is dedicated to on-premise systems and **ABAP Cloud project** is used with the SAP Cloud Platform ABAP Environment
* chaining statements:
```
	WRITE:  'Hello World!',
        	'Here I am!'.
```
* `d_` preceding variable names denotes a global var, however this is a company convention and thus it may differ in j&s-soft

## Data Types & Variables

### Numeric Data Types
Basic numeric data types:
* `b*` - byte, one-byte integer
* `s*` - short, two-byte integer
* `i` - integer, four-byte integer
* `p` - packed number, allows you to define the number of decimal places (up to 14) in the number (if none defined it is treated as an integer), useful for distances and money for example
* `decfloat16` - decimal floating point with 16 places
* `decfloat34` - ...34 places
* `f*` - binary floating point with 17 decimal places (may produce some unpredictable rounding errors and should be avoided if possible)

Declaration:
```
	DATA: 	ld_integer 		TYPE i 					VALUE 200,
			ld_packed 		TYPE p 		DECIMALS 2 	VALUE '3.115'.
```

### Character Data Types
Basic character data types:
* `c*` - any fixed amount (up to 262,143 - if no `LENGTH` is provided it is treated as a single character) of alphanumeric characters
* `string` - a variable length of alphanumeric characters
* `n` - a fixed length of numeric characters (useful for IDs that have leading zeros)
* `d` - a date in the form `yyyymmdd` (e.g. `20220905`))
* `t` - a time in the form `hhmmss` (e.g. `120255`)

Declaration:
```
	DATA: 	d_chars     	TYPE c 		LENGTH 5 	VALUE 'fiver',
      		ld_single_char 	TYPE c 					VALUE 'A',
			d_string 		TYPE string 			VALUE 'Hello World'.
```

### Booleans in ABAP
When working with Booleans, it is best to use the Boolean data objects `abap_true` and `abap_false`:
```
	IF true_variable = abap_true.
	...
	DATA: 	ld_boolean TYPE boolean.
			ld_boolean = abap_true.
```

### Inline Data Declarations
The data type is inferred based on how the variable is used when it is declared:
```
	DATA(d_integer) = 10.
```
Equivalent to:
```
	DATA: d_integer TYPE integer.
	d_integer = 10.
```

## Operations

### Basic Operations
Available operations (`++`, `+=`, `-=` are not present):
* `=` - assigns the value
* `+`, `-`, `*`, `/` - basic arithmetical operations
* `DIV`, `MOD` - division with no remainder & remainder of a division operation
* `**` - raise to the power of the right-hand number

### Logic Operations
Available operations:
* `=` - equal
* `<>` - not equal
* `<`, `>`, `<=`, `>=` - less/greater than, less equal, greater equal
* `BETWEEN n0 AND n1` - `true` if number on the left is between `n0` and `n1`

### Math Functions
Available functions:
* `abs` - absolute value
* `sign` - sign of the argument
* `ceil` - value rounded to the next highest number
* `floor` - value rounded to the next lowest number
* `trunc` - drops any decimal values
* `frac` - returns only decimal values
* `ipow` - raises the base argument to the power of the expression, e.g. `ipow( base = 10 exp = 3 )` (note the **whitespaces**)

## Flow Control

Example of `IF` statement (note the **whitespaces** in the condition):
```
	IF ( d_test_0 = 1 AND d_test_1 = 2 ).
    	WRITE: 'Value of d_test is 1'.
	ELSE.
	    WRITE: 'Value of d_test something other than 1'.
	ENDIF.
```

Example of `CASE` statement:
```
	CASE d_test_0.
	    WHEN 0 OR 2.
    	    WRITE: 'd_test_0 is now 0 or 2 in CASE statement'.
	    WHEN 1.
    	    WRITE: 'd_test_0 is now 1 in CASE statement'.
	ENDCASE.
```

`DO` loop example:
```
	DO 5 Times.
    	WRITE: 'Running a loop...', /.
	ENDDO.
```

`WHILE` loop example:
```
	WHILE d_loop_counter < 5.
	    WRITE: 'Running a WHILE loop...', /.
	    d_loop_counter = d_loop_counter + 1.
	ENDWHILE.
```

## Screens

Selection screen example:
```
	SELECTION-SCREEN BEGIN OF BLOCK selection WITH FRAME TITLE text-001.
	    PARAMETER:
	         p_input  TYPE boolean AS LISTBOX VISIBLE LENGTH 10 OBLIGATORY,
	         p_input2 TYPE boolean AS CHECKBOX,
	         p_input3 TYPE boolean RADIOBUTTON GROUP grp,
        	 p_input4 TYPE boolean RADIOBUTTON GROUP grp,
    	     p_input5 TYPE string.
	SELECTION-SCREEN END OF BLOCK selection.
```

The parameter variable names must be eight characters in length or less. Any parameters entered by the user will be converted to all uppercase when passed to your program.

A simple program that takes user input and prints it out on screen:
```
	REPORT znv_hello_world_input.

	SELECTION-SCREEN BEGIN OF BLOCK selection WITH FRAME TITLE text-001.
   		parameter p_input TYPE string.
	SELECTION-SCREEN END OF BLOCK selection.

	WRITE: 'User typed in : ', p_input.
```

## Program Lifecycle

Lifecycle events:
* `LOAD-OF-PROGRAM` - triggered once before the program is loaded into memory
* `INITIALIZATION` - called before any selection screens are processed
* `AT SELECTION-SCREEN` - can be used for enforcing validations for example
* `START-OF-SELECTION` - triggered after the user has pressed the `Execute` button (optional to use if no other events are defined, because it’s the default event that will begin executing your code)

## Quick Reference

Code elements & special characters:
* `/` - new line character
* `sy` - the table containing all the system fields
* whitespace **IS IMPORTANT** for ABAP compiler, e.g. `(3*3)` is considered bad while `( 3 * 3 )` is considered good practice
* `CTRL + 7` - comment out a block of code (in Eclipse ADT)

## Literature
[O'Neill Brian. Getting Started with ABAP, 1st Edition. SAP PRESS, 2015](https://www.amazon.com/ABAP-Introduction-Beginners-Guide-PRESS/dp/1493212427)


# Advanced ABAP

## ABAP Language Foundation and Evolution

### Moving from Statements to Expressions and Functions

An expression is a combination of constants, variables, operators, functions, and values that is evaluated at runtime and returns a result. It can therefore be used in program statements in places where a constant or variable could also be used.

Replacing Statements with Expressions:
```ABAP
	MULTIPLY gv_int BY 2.
	ADD 1 TO gv_int.
	" or
	COMPUTE gv_int = gv_int * 2 + 1.
	" or
	gv_int = gv_int * 2 + 1.
```

Functions instead of Statements:
```ABAP
	DATA:
		gt_text TYPE TABLE of string,
		gv_lines TYPE i.
	DESCRIBE TABLE gt_text LINES gv_lines.
	" or
	gv_lines = lines( gt_text ).
```

In **SAP NetWeaver 7.02**, expressions and built-in functions were introduced for string processing. As of **SAP NetWeaver 7.40** ABAP supports expressions and functions for converting data types and casting of reference variables, assigning values based on conditions, creating (data) objects, processing internal tables and in-line declarations for data objects and field symbols.

Benefits of using expressions:
* concise - one expression can replace multiple statements
* compact - the enhanced support for expressions can eliminate the need for certain helper variables

Potential pitfalls of using expressions:
* difficult to read - where do I begin to read the code when expressions are nested
* difficult to debug - limited support for checking intermediate results

Chaining of functional methods:
```ABAP
	DATA go_airplane TYPE REF TO lcl_airplane.

	go_airplane = go_carrier->get_airplane( 'Berlin' ).
	go_airplane->display_attributes( ).
	" or
	go_carrier->get_airplane( 'Berlin' )->display_attributes( ).
```

Expression `CONV` converts the contents of the data object enclosed in the parentheses into the data type before the opening parenthesis. The following examples display the explicite target type or symbol `#` in conversion:
```ABAP
	CLASS lcl_class DEFINITION.
		PUBLIC SECTION.
		CLASS-METHODS do_something CHANGING cv_data TYPE STRING. " formal param is fully typed
		ENDCLASS.
	...
	DATA: gv_data TYPE C LENGTH 12.
	lcl_class=>do_something( CONV #( gv_data ) ). " target type can be taken from the context
```
```ABAP
	CLASS lcl_class DEFINITION.
		PUBLIC SECTION.
		CLASS-METHODS do_something CHANGING cv_data TYPE ANY. " formal param is typed generically
		ENDCLASS.
	...
	DATA: gv_data TYPE C LENGTH 12.
	lcl_class=>do_something( CONV string( gv_data ) ). " target type can be specified explicitely
```

The expression `CORRESPONDING` is an expression-based replacement for the statement `MOVE-CORRESPONDING`. If the argument is a structure, it searches for components that exist with identical names in the source structure and the target structure and copies the values of these components. You can use the expression on the right-hand side of a value assignment or — as in the example — in a parameter assignment:
```ABAP
	CLASS lcl_class DEFINITION.
		PUBLIC SECTION.
			CLASS-METHODS do_something IMPORTING is_conn TYPE ty_s_key.
		PRIVATE SECTION.
			TYPES:	BEGIN OF ty_s_key
						carrid TYPE spfli-carrid,
						connid TYPE spfli-connid,
					END OF ty_s_key.

	" with statement
	DATA:
		gs_conn TYPE spfli,
		gs_key TYPE lcl_class=>ty_s_key.
	MOVE-CORRESPONDING gs_conn TO gs_key.
	lcl_class=>do_something( gs_key ).

	" with expression
	DATA:
		gs_conn TYPE spfli.
	lcl_class=>do_something( CORRESPONDING #( gs_conn ) ).
```

Expressions for conditional assignments:
```ABAP
	DATA:	gv_class TYPE sbook-class,
			gv_text  TYPE string.
	
	gv_text = SWITCH #( gv_class
							WHEN 'Y' THEN 'Economy'
							WHEN 'C' THEN 'Business'
							WHEN 'F' THEN 'First' ).
```

Assignment with ternary operator:
```ABAP
	DATA:	gv_seatsocc   TYPE sflight-seatsocc,
			gv_seatsmax   TYPE sflight-seatsmax,
			gv_overbooked TYPE abap_bool.
	
	gv_overbooked = COND #(	WHEN gv_seatsocc > gv_seatsmax
							THEN abap_true
							ELSE abap_false ).
```

### Using ABAP Data Types and Data Objects

We differentiate between data objects and data types in ABAP. Data types are mere descriptions that are not linked with an address in the memory. Data objects are instances of data types and occupy memory to store the specific data that a program uses at runtime. You can define data objects in a program using the `DATA` statement. As shown in the figure Defining Data Objects, the name of the data object is followed by a `TYPE` addition. The type is linked to the data object statically and cannot be changed at runtime.

In terms of memory requirements and memory management, you can also differentiate between flat and deep data objects, as follows:
* flat data objects - they have a static length and do not refer to other objects. All elementary data objects apart from strings and all structures with exclusively flat components (regardless of potential nesting) are flat
* deep data objects - these include references, strings, and internal tables. Structures are also called deep if they contain at least one deep component, taking all includes and substructures into account

Literals are data objects that you define in the source code of a program by specifying a string representing a value.

Inline declarations are a new way of declaring variables (and field symbols) at operand positions. Inline declarations are available as of NetWeaver 7.40. Benefits include context (var is declared where it is needed), fewer type conflicts (data type is derived automatically and always fits the purpose) and faster implementation (no need to look up the required data type). Recommendations for using inline declarations: do not use inline declarations simply out of laziness, do not reuse inline declared variables, do not use an inline declared var far away from its definition, avoid using inline declarations for global data objects.

### Reading Data from Only One Database Table with Open SQL

New Open SQL syntax:
```ABAP
	SELECT carrid, connid, fldate, paymentsum, currency
	FROM sflight INTO TABLE @gt_flights
	WHERE fl_date > @sy-datum.
```

New sequence of clauses:
```ABAP
	SELECT FROM sflight				" FROM-clause before field list
	FIELDS carrid, connid, fldate, paymentsum, currency
	WHERE fldate > @sy-datum
	INTO TABLE @gt_flights.			" INTO-clause at the very end
```

Inline declaration in Open SQL:
```ABAP
	SELECT carrid, connid, fldate, paymentsum, currency
	FROM sflight
	INTO TABLE @data(gt_flights)	" standard table with non-unique default key
	WHERE fldate > @sy-datum.
```

## Program Calls and Memory Management

### Calling Programs Synchronously

To call an executable (type 1) program, use the `SUBMIT` statement. If you use the `VIA SELECTION-SCREEN` addition, the system displays the standard selection screen. If you use the `AND RETURN` addition, the system resumes processing with the first statement of the calling program after the `SUBMIT` statement, once the execution of the called program finishes.

An example of [passing parameters to a program can be found here](http://zevolving.com/2014/03/abap-submit-pass-parameters-to-program/).



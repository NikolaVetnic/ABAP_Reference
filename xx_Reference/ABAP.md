# ABAP Language

## Overview

### Characteristics

Advanced Business Application Programming (ABAP) is a programming language developed by SAP for programming business applications in the SAP environment. 

Characteristics:
* is typed
* enabled multi-lingual applications
* enables SQL access
* has been enhanced as an object-oriented language
* is platform-independent
* is upward-compatible

### Syntax

ABAP programs are comprised of individual sentences with key characteristics:
* start with a keyword
* end with a period (without exception)
* may contain additions and operands (depending on the keyword used)
* can span multiple lines
* words must be separated by at least one space

ABAP runtime system does not differentiate between cases. There exists a concept of chained statements (ending with `,`) which allows you to combine consecutive statements with an identical keyword.

### Comments

Two different types:
* the `*` character at the start of a program line indicates that the entire line is a comment, therefore, the ABAP runtime system ignores the whole line
* the `"` character, which can be entered at any position in the line, indicates that the remaining content in the line is a comment

### Keyword Documentation

There are two ways to open this documentation (this works with **ABAP Editor** - `SE38`):
* place your cursor on the keyword that you want to learn about, then press `F1` - this opens the documentation for that specific statement
* Choose the `Help On` button

### `PARAMETERS` Keyword

The `PARAMETERS` keyword defines an input field on what is known as a **Selection Screen**. Every parameter you define in your ABAP program has a single corresponding input field on the selection screen. The selection screen also includes an `Execute` button.

Example:
```ABAP
	REPORT znv_bc100_01_first_program.

	PARAMETERS pa_name TYPE string.
	WRITE: 'Hello ', pa_name, /.
```

### Selection Screen Texts

To maintain selection texts from the ABAP Editor, choose *Goto → Text Elements → Selection Texts*. To maintain translations for selection texts, choose *Goto → Translation*. When in Eclipse, open the program in SAP GUI.

### System Variables

Frequently used system variables:
* `SY-DATUM`, current date
* `SY-UZEIT`, current time
* `SY-UNAME`, User ID of current user

All system variable names begin with `SY-`.

In regards to system variable `SY-SYBRC` consider the following example:
```ABAP
	PARAMETERS:
	    pa_name TYPE string.

	FIND 'Smith' IN pa_name IGNORING CASE.

	IF sy-subrc = 0.
	  WRITE: 'Name contains "Smith".', /.
	ELSE.
	  WRITE: 'Name does not contain "Smith".', /.
	ENDIF.
```

When the program processes these statements, it sets the system variable `sy-subrc`. If this variable is set to `0`, it indicates that the statement was successful. In the `FIND` statement, this means that the value or pattern was found in the variable. In the `REPLACE` statement, it means that the value was found and successfully replaced with the new value.

## Procedural ABAP Programs

### Subroutines

Example of a subroutine definition and call within an ABAP program:
```ABAP
	TYPES 
		gty_perc TYPE p DECIMALS 2.

	DATA:
		gv_int1 TYPE i,
		gv_int2 TYPE i,
		gv_result TYPE gty_perc.
	...

	PERFORM calc_perc
		USING
			gv_int1
			gv_int2
		CHANGING
			gv_result.

	FORM calc_perc
		USING
			value(pv_act) TYPE i
			value(pv_max) TYPE i
		CHANGING
			value(cv_pc) TYPE gty_perc.

		cv_pc = pv_act * 100 / pv_max.
	ENDFORM.
```

### Components

Typical procedural ABAP program consists of type definitions and data declarations (describes the data structure the program uses):
```ABAP
	REPORT ... .
	*----------
	TYPES: ...

	DATA: ...

	...

	PERFORM form1 ...

	CALL FUNCTION 'FB1'
	...
	CALL FUNCTION 'FB2'
	...

	*----------
	FORM form1 ...
	...
	ENDFORM.
```

It is possible to encapsulate logic in modularization units, but there is not protection for the global data objects. It is not possible to access the global data of a function group directly from the main program.

### Function Groups

When a function module is called in the main program, its function group is loaded into the internal session. The function group remains active until the main program finishes. A function group brings together data and functions:
```ABAP
	FUNCITON-POOL s_vehicle.

	* speed is a global var used in the function-pool
	DATA: speed TYPE i.
		...
	FUNCTION inc_speed.
		...
		ADD imp_speed TO speed.
	ENDFUNCTION.

	FUNCTION dec_speed.
		...
		SUBTRACT imp_speed FROM speed.
	ENDFUNCTION.

	...
```

The function group `S_VEHICLE` provides a user with services `inc_speed`, `dec_speed` - these are the **interface** of the function group (all have access to global data object speed, which belongs to the function group).

Example of using the function group:
```ABAP
	REPORT ... .
	
	TYPES: ...
	DATA: wa_car TYPE ...

	* no direct access to speed, instead use functions of pool
	CALL FUNCTION 'INC_SPEED' ...
	CALL FUNCTION 'DEC_SPEED' ...
```

A function group can only refer to one vehicle at a time.

Detailed description of how function groups are created can be found in the **BC400_EN_Col18** e-book from the Learning Hub.

### Comparing Functions and Subroutines to Objects

The features of encapsulation using function groups and subroutines:
* use modularization units to encapsulate functions and data
* it is possible to work with the global data of the main program

The features of encapsulation using objects:
* use objects to encapsulate functions and data
* it is possible to create multiple instances (objects) and perform multiple instantiation

Objects are stored in the same internal session as the program in use. All data areas are separate from each other. Using multiple instantiation in object-oriented programming allows you to create a direct abstraction of a real object.

## Classic Exception Handling

If application errors occur when a function module is processed (for example, a value is not suitable for the calculation), the function module raises a corresponding exception. The exception cancels the processing of the function module and returns the system to the calling program. If the exception is listed (caught) in the `EXCEPTIONS` block of the call, the specified return code is entered in the `sy-subrc` system field of the calling program. By checking this field after the call, you can determine if and, where applicable, which exception was raised so you can react accordingly. If no exception is raised by the function module, the sy-subrc of the calling program is set to zero.

An example of classic exception handling can be seen here:
```ABAP
	" function call
	CALL FUNCTION 'BC400_MOS_POWER'
		EXPORTING
			iv_base = pa_int1
			iv_power = pa_int2
		IMPORTING
			ev_result = gv_result
		EXCEPTIONS
			POWER_VALUE_TOO_HIGH = 1
			OTHERS               = 2.

	" class static method call
	CALL METHOD cl_bc400_compute=>get_power
		EXPORTING
			iv_base = pa_int1
			iv_power = pa_int2
		IMPORTING
			ev_result = gv_result
		EXCEPTIONS
			POWER_VALUE_TOO_HIGH = 1
			OTHERS               = 2.

	CASE sy-subrc.
		WHEN 0.
			WRITE gv_result.
		WHEN 1.
			WRITE 'Power Value Max 4'.
		WHEN 2.
			WRITE 'Unknown Error'.
	ENDCASE.
```

You also have the option of setting a return code explicitly for some of the possible exceptions and setting a different return code for all unmentioned exceptions. To do so, list the formal exception OTHERS with the desired return code.

Catch all exceptions in your call and react to them in the program. If a raised exception is not caught, a runtime error occurs.

## ABAP Language

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
```
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
```
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

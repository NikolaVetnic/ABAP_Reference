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

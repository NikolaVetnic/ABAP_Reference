# Screens

A screen not only consists of its layout with input/output fields, buttons, and other screen elements, but also processing logic (source code excerpts that are processed as the preprocessing or postprocessing of the screen display). Screens have the same formatting options as lists and selection screens.

Selection screen example:
```ABAP
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
```ABAP
	REPORT znv_hello_world_input.

	SELECTION-SCREEN BEGIN OF BLOCK selection WITH FRAME TITLE text-001.
   		parameter p_input TYPE string.
	SELECTION-SCREEN END OF BLOCK selection.

	WRITE: 'User typed in : ', p_input.
```

The following program types may contain screens:
* **executable program** (**report**): screens are used in executable programs to display data in addition to the list output, or to replace the list output completely. In addition, you can also use screens to enter and change data in the list. For the purpose of reusability and data encapsulation, however, SAP recommends that you use screens from function groups rather than create screens directly in reports
* **function group**: screens in function groups can be addressed using dialog transactions. You can also start such screens from the source code of a function module using the CALL SCREEN statement. SAP recommends that you use function groups to create screens and screen sequences easily available for reuse
* **module pool**: screens in module pools can only be addressed using dialog transactions. In contrast to screens in function groups, you cannot encapsulate screens in module pools, nor can you provide them with a well-defined external interface - for this reason, SAP recommends that you use function groups rather than create new module pools

The **flow logic** of a screen consists of Process Before Output (PBO) and Process After Input (PAI). PBO contains references to processing blocks (PBO modules) that are processed in preparation for the screen display (for example, data selection) before the screen is sent. PAI contains references to processing blocks (PAI modules) that are processed as a reaction to user input and actions (for example, save data).

Using the graphical Screen Painter, you can design the layout of the screen.

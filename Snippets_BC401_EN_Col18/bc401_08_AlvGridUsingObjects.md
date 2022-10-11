# A Simple Example of ALV Grid using Objects

Taken from [this website](https://henrikfrank.dk/abaptips/abap%20language/other%20stuff/alv%20grid%20using%20objects%20-%20simple%20example.html).

Start off by creating a report, in my case it's `ZNV_OBJ_EX_ALV_GRID`. Next, start the `SE51` Screen Painter transaction and create a screen with number `0100`. Open the **Screen Painter** editor via **Layout** button and create a Custom Control on the screen, naming it `MY_CONTAINER`. Close the editor and add the following line into the code editor available via **Flow Logic** tab:
```ABAP
	PROCESS BEFORE OUTPUT.
		MODULE create_objects. " line to add.

	PROCESS AFTER INPUT.
	* MODULER USER_COMMAND_0100.
```

The remaining code goes into the report itself:
```ABAP
	*&---------------------------------------------------------------------*
	*& Report znv_obj_ex_alv_grid
	*&---------------------------------------------------------------------*
	*&
	*&---------------------------------------------------------------------*
	REPORT znv_obj_ex_alv_grid.
	
	*&---------------------------------------------------------------------*
	*& CLASS lcl_event_handler
	*&---------------------------------------------------------------------*
	*&
	*&---------------------------------------------------------------------*
	CLASS lcl_event_handler DEFINITION.
	  PUBLIC SECTION.
	    METHODS: on_double_click FOR EVENT double_click OF cl_gui_alv_grid
	      IMPORTING
	        es_row_no
	        e_column.
	ENDCLASS. "lcl_event_handler DEFINITION
	
	CLASS lcl_event_handler IMPLEMENTATION.
	  METHOD on_double_click.
	    DATA: msg_string TYPE string,
	          row_string TYPE string.
	
	    row_string = es_row_no-row_id.
	
	    CONCATENATE 'Row: ' row_string ' Column: ' e_column-fieldname
	    INTO msg_string.
	
	    MESSAGE msg_string TYPE 'I'.
	  ENDMETHOD. "on_double_click
	ENDCLASS.



	DATA:
	  r_cont    TYPE REF TO cl_gui_custom_container,
	  r_grid    TYPE REF TO cl_gui_alv_grid,
	  itab      TYPE TABLE OF spfli,
	  r_handler TYPE REF TO lcl_event_handler.



	START-OF-SELECTION.
	  SELECT * FROM spfli
	  INTO TABLE itab.

	  CALL SCREEN 100.



	*&---------------------------------------------------------------------*
	*& Module create_objects OUTPUT
	*&---------------------------------------------------------------------*
	*& text
	*&---------------------------------------------------------------------*
	MODULE create_objects OUTPUT.
	  CHECK r_cont IS INITIAL.

	  CREATE OBJECT r_cont EXPORTING container_name = 'MY_CONTAINER'.
	  CREATE OBJECT r_grid EXPORTING i_parent = r_cont.

	  r_grid->set_table_for_first_display(
	    EXPORTING i_structure_name = 'SPFLI'
	    CHANGING it_outtab = itab ).

	  CREATE OBJECT r_handler.

	  SET HANDLER r_handler->on_double_click FOR r_grid.
	ENDMODULE.
```

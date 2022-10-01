## ABAP Language

### Overview

#### Characteristics

Advanced Business Application Programming (ABAP) is a programming language developed by SAP for programming business applications in the SAP environment. 

Characteristics:
* is typed
* enabled multi-lingual applications
* enables SQL access
* has been enhanced as an object-oriented language
* is platform-independent
* is upward-compatible

#### Syntax

ABAP programs are comprised of individual sentences with key characteristics:
* start with a keyword
* end with a period (without exception)
* may contain additions and operands (depending on the keyword used)
* can span multiple lines
* words must be separated by at least one space

ABAP runtime system does not differentiate between cases. There exists a concept of chained statements (ending with `,`) which allows you to combine consecutive statements with an identical keyword.

#### Comments

Two different types:
* the `*` character at the start of a program line indicates that the entire line is a comment, therefore, the ABAP runtime system ignores the whole line
* the `"` character, which can be entered at any position in the line, indicates that the remaining content in the line is a comment

#### Keyword Documentation

There are two ways to open this documentation (this works with **ABAP Editor** - `SE38`):
* place your cursor on the keyword that you want to learn about, then press `F1` - this opens the documentation for that specific statement
* Choose the `Help On` button

#### `PARAMETERS` Keyword

The `PARAMETERS` keyword defines an input field on what is known as a **Selection Screen**. Every parameter you define in your ABAP program has a single corresponding input field on the selection screen. The selection screen also includes an `Execute` button.

Example:
```
	REPORT znv_bc100_01_first_program.

	PARAMETERS pa_name TYPE string.
	WRITE: 'Hello ', pa_name, /.
```

#### Selection Screen Texts

To maintain selection texts from the ABAP Editor, choose *Goto → Text Elements → Selection Texts*. To maintain translations for selection texts, choose *Goto → Translation*. When in Eclipse, open the program in SAP GUI.

#### System Variables

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

### Procedural ABAP Programs

#### Components

Typical procedural ABAP program consists of type definitions and data declarations (describes the data structure the program uses):
```
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

#### Function Groups

When a function module is called in the main program, its function group is loaded into the internal session. The function group remains active until the main program finishes. A function group brings together data and functions:
```
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
```
	REPORT ... .
	
	TYPES: ...
	DATA: wa_car TYPE ...

	* no direct access to speed, instead use functions of pool
	CALL FUNCTION 'INC_SPEED' ...
	CALL FUNCTION 'DEC_SPEED' ...
```

A function group can only refer to one vehicle at a time.

#### Comparing Functions and Subroutines to Objects

The features of encapsulation using function groups and subroutines:
* use modularization units to encapsulate functions and data
* it is possible to work with the global data of the main program

The features of encapsulation using objects:
* use objects to encapsulate functions and data
* it is possible to create multiple instances (objects) and perform multiple instantiation

Objects are stored in the same internal session as the program in use. All data areas are separate from each other. Using multiple instantiation in object-oriented programming allows you to create a direct abstraction of a real object.

### ABAP Objects

ABAP Objects is not a new language, but a systematic extension of ABAP. You can use ABAP Objects statements in procedural ABAP programs. ABAP Objects syntax example:
```
	REPORT ... .

	DATA:
		gv_counter TYPE i,
		gs_kna1 TYPE kna1.
	...

	CLASS lcl_car DEFINITION.
		...
	ENDCLASS.

	* =-=-=-= main program =-=-=-=

	gv_counter = gv_counter + 1.

	CREATE OBJECT ...

	MOVE gs_kna1 TO ...
```

Type checks in the object-oriented contexts of ABAP Objects are stricter than those in the procedural contexts. For a list of obsolete language elements, refer to the ABAP keyword documentation. 

#### Client/Server Relationship

Objects behave like client/server systems. When one object sends a message to another object to ask it to behave in a certain way, the first object is defined as a client and the second object is defined as a server.

#### Key Characteristics

Key characteristics of OO model of ABAP Objects:
* objects are a direct abstraction of the real world
* objects are units made up of data and functions
* processes can be implemented in a better way than in procedural programming

Advantages of the OO approach in regards to procedural programming model:
* improved software structure and consistency in the development process
* reduced maintenance effort and less susceptibility to errors
* better integration of the customer or user into the analysis, design, and maintenance process
* simpler and more secure extension of the software

#### Classes

Classes have **public** (usually methods and events, accessible from outside of a class) and **private** (data types and attributes, inaccessible from outside) components. 

Class needs a `DEFINITION` and `IMPLEMENTATION`:
```
	CLASS lcl_vehicle DEFINITION.
	...
	ENDCLASS.

	CLASS lcl_vehicle IMPLEMENTATION.
	...
	ENDCLASS.
```

##### Attributes

Attributes contain the data that can be stored in the objects of a class and can be elementary data objects, structured or table-type objects. They can also consist of data types (local or global) or reference types:
```
	CLASS class_name DEFINITION.
		...
		TYPES: ... .
		CONSTANTS: ... .
		DATA:
			var1 TYPE local_type,
			var2 TYPE global_type,
			var3 LIKE var1,
			var4 TYPE built_in_type VALUE val,
			var5 TYPE ... READ-ONLY,
			" ref. vars reference classes, interfaces, types
			var6 TYPE REF TO class_name,
			var7 TYPE REF TO interface_name,
			var8 TYPE REF TO type_name.
	ENDCLASS.
```

In class `DATA` statements, you can only use the `TYPE` addition to refer to data types. The `LIKE` addition is only allowed for local data objects or `SY` fields. The `READ-ONLY` addition indicates that a public attribute declared with `DATA` can be read from outside (however, the attribute can only be changed by methods in the same class). With `TYPE REF TO`, you can type an attribute as a reference. If you use the `TYPES` statement in the class definition, you are declaring a local type, which is specific to this local class.

Attributes can be private (defined in the `PRIVATE SECTION`) and public (defined in the `PUBLIC SECTION`):
```
	CLASS lcl_vehicle DEFINITION.
		PUBLIC SECTION.
		...

		PRIVATE SECTION.
			DATA:
				mv_make TYPE string,
				mv_model TYPE string.
	ENDCLASS.
```

Another division of attributes is in regards to whether they are static or not:
* instance attributes - these exist once per object, i.e. runtime instance of the class
* static attributes - these exist once for each class and are visible for all runtime instances (usually contain types and constants, central app data buffers and admin information such as counters)

##### Methods

Method definition syntax:
```
	CLASS classname DEFINITION.
		...
			METHODS method_name
				[	IMPORTING iv_par TYPE type_name
					EXPORTING ev_par TYPE type_name
					CHANGING  cv_par TYPE type_name
					RETURNING value(rv_par) TYPE type_name
					EXCEPTIONS exception		" 1) classic
					RAISING exception_class ].	" 2) class-based (can only use one)

	ENDCLASS.

	CLASS classname IMPLEMENTATION.
		METHOD method_name.
			...
		ENDMETHOD.
	ENDCLASS.
```

Methods also support the `SY-SUBRC` return value, but only when you define the signature exceptions with the use of `EXCEPTIONS`. Use the `RAISING` addition in place of `EXCEPTIONS` to propagate class-based exceptions. The caller then handles these class-based exceptions without evaluating the `SY-SUBRC` return value.

Methods can also be private or public:
```
	CLASS lcl_vehicle DEFINITION.
		PUBLIC SECTION.
			METHODS set_type IMPORTING iv_make  TYPE string
									   iv_model TYPE string.

		PRIVATE SECTION.
			METHODS init_type.
			DATA:
				mv_make  TYPE string,
				mv_model TYPE string.
	ENDCLASS.

	CLASS lcl_vehicle IMPLEMENTATION.
		METHOD init_type.
			CLEAR: mv_make, mv_model.
		ENDMETHOD.

		METHOD set_type.
			IF iv_make IS INITIAL.
				* calling METHOD init_type ...
			ELSE
				mv_make = iv_make.
				mv_model = iv_model.
			ENDIF.
		ENDMETHOD.
	ENDCLASS.
```

Methods can also be:
* instance methods - uses the `METHODS` syntax keyword
* static methods - uses the `CLASS-METHODS` syntax keyword

Example:
```
	CLASS lcl_vehicle DEFINITION.
		PUBLIC SECTION.
			METHODS set_type ... .
			CLASS-METHODS get_n_o_vehicles ... .

		PRIVATE SECTION.
			DATA:
				mv_make ... ,
				mv_model ... .
			CLASS-DATA
				gv_n_o_vehicles ... .
			METHODS init_type ... .
	ENDCLASS.
```

##### Objects

The statement `DATA go_vehicle1 TYPE REF TO lcl_vehicle` defines a reference variable - this variable can point to instances of the class `lcl_vehicle`. The initial value of a reference variable is an empty reference, that is, the reference points to nothing at all:
```
	CLASS lcl_vehicle DEFINITION.
		PUBLIC SECTION.
			...
		PRIVATE SECTION.
			...
	ENDCLASS.

	CLASS lcl_vehicle IMPLEMENTATION.
		...
	ENDCLASS.

	DATA:
		go_vehicle1 TYPE REF TO lcl_vehicle,
		go_vehicle2 LIKE go_vehicle.

	CREATE OBJECT go_vehicle1.
	CREATE OBJECT go_vehicle1.


	START-OF-SELECTION.
		...
```

Independent references are references that have not been defined within a class. If no independent reference points to an object it can no longer be accessed - all such objects are deleted by the garbage collector.

To keep several objects from the same class in your program, you can define an internal table with one column that contains the object references for this class. To maintain the objects in the table, you can use statements for internal tables, such as `APPEND`, `READ`, or `LOOP`:
```
	DATA:
		go_vehicle  TYPE REF TO lcl_vehicle,
		gt_vehicles TYPE TABLE OF REF TO lcl_vehicle.

	CREATE OBJECT go_vehicle.
	APPEND go_vehicle TO gt_vehicles.
```

Example of aggregation:
```
	DATA:
		go_vehicle TYPE REF TO lcl_vehicle,
		go_wheel   TYPE REF to lcl_wheel.
	
	CREATE OBJECT go_vehicle.

	DO ... TIMES.
		CREATE OBJECT go_wheel.
		* add wheel to vehicle
		...
	ENDDO.
```

##### Accessing Methods and Attributes

Syntax for calling instance methods:
```
	CALL METHOD ref->method_name
		EXPORTING  iv_par = val_ex ...
		IMPORTING  ev_par = val_im ...
		CHANGING   cv_par = val_chg ...
		RECEIVING  rv_par = val_res ...
		EXCEPTIONS except = val_rc ... .
	
	" shorter syntax as of SAP Web AS 6.10
	" notice the WHITESPACE AFTER AND BEFORE BRACKETS 
	ref->method_name(
		EXPORTING  iv_par = val_ex ...
		IMPORTING  ev_par = val_im ...
		CHANGING   cv_par = val_chg ...
		RECEIVING  rv_par = val_res ...
		EXCEPTIONS except = val_rc ... ).

	" example
	DATA:
		go_vehicle TYPE REF TO lcl_vehicle,
		gv_make TYPE string, gv_model TYPE string.
	...
	go_vehicle->get_type(
		IMPORTING
			ev_make = gv_make
			ev_model = gv_model ).
```

Syntax for static method calls:
```
	CALL METHOD class_name=>method_name
		EXPORTING ... EXCEPTIONS ... . " all as above

	class_name=>method_name(
		EXPORTING ... EXCEPTIONS ... ). " all as above

	DATA gv_number TYPE i.
	...
	lcl_vehicle=>get_n_o_vehicles( IMPORTING ev_count = gv_number ).
```

Methods that have a `RETURNING` parameter are described as functional methods. The `RETURNING` parameter must always be passed by value - `RETURNING VALUE(...)` - and not by reference:
```
	CLASS lcl_vehicle DEFINITION.
		PUBLIC SECTION.
			METHODS get_average_fuel
				IMPORTING
					iv_distance    TYPE s_distance
					iv_fuel        TYPE s_capacity
				RETURNING
					value(rv_fuel) TYPE s_consum.
	" =-=-=-=
	DATA:
		go_veh1 TYPE REF TO lcl_vehicle,
		go_veh2 TYPE REF TO lcl_vehicle,
		gv_avg_fuel TYPE s_consum.
	...
	gv_avg_fuel = 
		go_veh1->get_average_fuel( iv_distance = 500 iv_fuel = '50.0' ) +
		go_veh2->get_average_fuel( iv_distance = 600 iv_fuel = '60.0' ).
	" =-=-=-=
	DATA gv_number TYPE i.
	...
	gv_number = lcl_vehicle=>get_n_o_vehicles( ).

	" detailed syntax for the same call
	CALL METHOD lcl_vehicle=>get_n_o_vehicles
		RECEIVING rv_count = gv_number.
```

You can access public attributes from outside the class in the same way as method calls:
```
	CLASS lcl_vehicle DEFINITION.
		PUBLIC SECTION.
			DATA:
				mv_make  TYPE string READ-ONLY,
				mv_model TYPE string READ-ONLY.
			CLASS-DATA:
				gv_n_o_vehicles TYPE i READ-ONLY.
			...
	ENDCLASS.
	...
	DATA:
		go_vehicle TYPE REF TO lcl_vehicle,
		gv_make    TYPE string, " these two are in a separate DATA block in the book
		gv_number  TYPE i.
	...
	CREATE OBJECT go_vehicle.
	...
	gv_make = go_vehicle->make.
	gv_number = lcl_vehicle=>gv_n_o_vehicles.
```

##### Constructors

The instance constructor is a special instance method in a class and is always named `CONSTRUCTOR`. The constructor is automatically called at runtime with the `CREATE OBJECT` statement. The constructor signature can only include importing parameters and exceptions (no instances are created and no main memory space is occupied in case of exceptions):
```
	CLASS lcl_vehicle DEFINITION.
		PUBLIC SECTION.
			METHODS
				constructor
					IMPORTING
						iv_make  TYPE string
						iv_model TYPE string.

		PRIVATE SECTION.
			DATA:
				mv_make TYPE string,
				mv_model TYPE string.
			CLASS-DATA
				gv_n_o_vehicles TYPE i.
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_vehicle IMPLEMENTATION.
		METHOD constructor.
			mv_make = iv_make. " iv_make comes from DEFINITION
			mv_model = iv_model.
			ADD 1 TO gv_n_o_vehicles.
		ENDMETHOD.
	ENDCLASS.
	" =-=-=-=
	DATA
		go_vehicle TYPE REF TO lcl_vehicles.
	...
	CREATE OBJECT go_vehicle
		EXPORTING
			iv_make  = 'Ferrari'
			iv_model = 'F40'
```

##### Static Constructors

These are special static methods and are always named `CLASS_CONSTRUCTOR`. The static constructor is executed only once per program. The static constructor is called by the system automatically before the class is first accessed and before the first execution of the following actions: `CREATE OBJECT` command, accessing the static attributes or calling static methods, registering the event handler method for an event in the class.

Example:
```
	CLASS lcl_vehicle DEFINITION.
		PUBLIC SECTION.
			CLASS-METHODS
				class_constructor.
			METHODS
				constructor IMPORTING ... .
		PRIVATE SECTION.
			CLASS-DATA gv_n_o_vehicle TYPE i.
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_vehicle IMPLEMENTATION.
		METHOD class_constructor.
			...
		ENDMETHOD.

		METHOD constructor.
			...
		ENDMETHOD.
	ENDCLASS.
	" =-=-=-=
	gv_number = lcl_vehicle=>get_n_o_vehicles( ).
```

When you define static constructors, always consider the following points:
* each class does not have more than one static constructor
* the static constructor must be defined in the public area
* the static constructor does not have any parameters or exceptions
* the static constructor cannot be called explicitly

##### Self-Reference

You can address an object itself by using the predefined reference variable `ME` within its instance methods:
```
	CLASS lcl_vehicle DEFINITION.
		PUBLIC SECTION.
			...
			METHODS set_type
				IMPORTING iv_make TYPE string
				... .
		PRIVATE SECTION.
			DATA: make TYPE string,
				...
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_vehicle IMPLEMENTATION.
		METHOD set_type.
			DATA make TYPE string.

			me->make = iv_make.
			TRANSLATE me->make TO UPPER CASE.

			make = me->make.
			CONCATENATE '_' make INTO make.
			...
		ENDMETHOD.
	ENDCLASS.
```

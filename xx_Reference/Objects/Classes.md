## Classes

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

### Attributes

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

### Methods

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

### Objects

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

### Accessing Methods and Attributes

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

### Constructors

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

### Static Constructors

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

### Self-Reference

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

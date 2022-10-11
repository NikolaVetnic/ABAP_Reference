## Object-Oriented Design Patterns

Abstract classes and abstract methods:
```
	CLASS lcl_... DEFINITION ABSTRACT. " cannot be instantiated
		...
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_... DEFINITION ABSTRACT.
		...
		METHODS ... ABSTRACT ... " method not implemented in this class
		...
	ENDCLASS.
```

Final classes and methods:
```
	CLASS lcl_... DEFINITION FINAL " other class cannot inherit from this one
				[ INHERITING FROM ... ] .
		...
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_... DEFINITION.
		...
		METHODS ... FINAL ... " method cannot be redefined
		...
	ENDCLASS.
```

### Internal Tables with Object References

Object references can be stored in internal tables which is used to implement associations. The following code block gives an example of how we should retrieve a specific object from such a list:
```
	TYPES:
		BEGIN OF gty_s_vehicle,
			make TYPE string,
			model TYPE string,
			ref TYPE REF TO lcl_vehicle,   " struct with key info and a ref to obj 
		END OF gty_s_vehicle,

		gty_t_vehicles TYPE TABLE OF gty_s_vehicle.
	
	DATA:
		gs_vehicle TYPE gty_s_vehicle,
		gt_vehicles TYPE gty_t_vehicles.
	" =-=-=-=
	READ TABLE gt_vehicles INTO gs_vehicle " use the key info to 
		WITH KEY make = ... modle = ... .  " retreive an object from list
	CALL METHOD gs_vehicle-ref->...
	" =-=-=-=
	LOOP AT gt_vehicles INTO gs_vehicle
		WHERE make = ... .
		...
	ENDLOOP.
```

The example explains how to retrieve a specific object from such a list. The object reference is stored in the table along with some key information (the make and model of the vehicle). The object reference can easily be retrieved through this key information. This technique implies a redundant storage of information as the key values are already stored in attributes of the object.

Read access using public attributes:
```
	CLASS lcl_vehicle DEFINITION.
		PUBLIC SECTION.
			DATA:
				make TYPE string READ-ONLY,  " public attributes required for
				model TYPE string READ-ONLY. " direct access
	ENDCLASS.
	" =-=-=-=
	DATA:
		go_vehicle TYPE REF TO lcl_vehicle,
		gt_vehicles TYPE TABLE OF REF TO lcl_vehicle.
	" =-=-=-=
	READ TABLE gt_vehicles INTO go_vehicle WITH KEY 
		table_line->make = ...
		table_line->model = ... .
	" =-=-=-=
	LOOP AT gt_vehicles INTO go_vehicle WHERE
		table_line->make = ... .
		...
	ENDLOOP.
```

The key information redundancy can be avoided by making the key attributes public. Public attributes can be used directly in the statements to access internal tables like `READ TABLE` and `LOOP AT`. The expression `TABE_LINE` used here is a built-in part of the ABAP language. When the line type of an internal table is an elementary data type instead of a structure type, you need to use `TABLE_LINE`. Such internal tables have just one unnamed column. `TABLE_LINE` is used as the generic column name for this single column.

### Other Patterns

Navigation methods and chain method calls example (this technique is not restricted to the chaining of two methods - in more complicated object models, it is possible to chain any number of methods):
```
	DATA:
		go_rental TYPE REF TO lcl_rental,
		go_vehicle TYPE REF TO lcl_vehicle.
	go_vehicle = go_rental->get_vehicle( ... ). " a var to store a ref to a vehicle
	go_vehicle->display_attributes( ).
	" =-=-=-=
	DATA:
		go_rental TYPE REF TO lcl_rental.
	go_rental->get_vehicle( ... )->display_attributes( ). " chaining the methods
```

Creating objects with the `NEW` operator:
```
	DATA:
		go_vehicle TYPE REF TO lcl_Vehicle.
	CREATE OBJECT go_vehicle
		EXPORTING
			iv_make = 'VW'
			iv_model = '1200'.
	" =-=-=-= NEW can be used instead of CREATE OBJECTS (implicit typing)
	DATA:
		go_vehicle TYPE REF TO lcl_vehicle.
	go_vehicle = NEW #(
		iv_make = 'VW'
		iv_model = '1200'
	).
	" =-=-=-= explicit typing allows to create a subclass instance
	DATA:
		go_vehicle TYPE REF TO lcl_vehicle.
	go_vehicle = NEW lcl_truck(
		iv_make = 'Daimler'
		iv_model = 'Actros'
		iv_cargo = 25000
	).
```

It is possible to use the `NEW` operator at many expression positions, especially as an actual parameter of a method:
```
	" =-=-=-= NEW at APPEND workarea position
	DATA:
		gt_vehicles TYPE TABLE OF REF TO lcl_vehicle.
	APPEND NEW #(
		iv_make = 'Porsche'
		iv_model = '911'
	) TO gt_vehicles.
	" =-=-=-= NEW at method call parameters position
	DATA:
		go_rental TYPE lcl_rental.
	...
	go_rental->add_vehicle( io_vehicle = NEW lcl_truck(
		iv_make = 'Daimler'
		iv_model = 'Actros'
		iv_cargo = 25000
	) ).
	" =-=-=-= NEW at method call parameters position
	DATA:
		go_alv TYPE REF TO cl_gui_alv_grid.
	go_alv = NEW #(
		i_parent = NEW cl_gui_custom_container( 
			container_name = 'CONTAINER_1'
		).
	).
```

You can use the expression `IS INSTANCE OF` to find out whether an object reference points to an instance of a particular class:
```
	DATA:
		gt_vehicles TYPE TABLE OF REF TO lcl_Vehicle,
		go_vehicle TYPE REF TO lcl_vehicle,
		go_truck TYPE REF TO lcl_truck.
	LOOP AT gt_vehicles INTO go_vehicle.
		IF go_vehicle IS INSTANCE OF lcl_truck.
			go_truck ?= go_vehicle.
			...
		ENDIF.
	ENDLOOP.
```

The control structure CASE TYPE OF allows you to distinguish between different possible types of an object reference:
```
	DATA:
		gt_vehicles TYPE TABLE OF REF TO lcl_vehicle,
		go_vehicle TYPE REF TO lcl_vehicle,
		go_truck TYPE REF TO lcl_truck,
		go_bus TYPE REF TO lcl_bus.
	LOOP AT gt_vehicles INTO go_vehicle.
		CASE TYPE OF go_vehicle.
			WHEN TYPE lcl_truck INTO go_truck.
				...
			WHEN TYPE lcl_bus " INTO go_bus. -> optional
				...
			WHEN OTHERS. " optional
				...
		ENDCASE.
	ENDLOOP.
```

We can restrict the visibility of the instance constructor. If the visibility of the instance constructor is restricted, the `CREATE OBJECT` statements to instantiate this class is only allowed in certain parts of the coding:
```
	CLASS lcl_... DEFINITION CREATE PUBLIC / PROTECTED / PRIVATE.
		...
	ENDCLASS.
```

The types of visibilities for the instance constructor are as follows:
* `PRIVATE` : if a class has a private instance constructor, it can only be instantiated from within the class itself, typically in static methods defined for that purpose. These methods are sometimes called Factory-Methods
* `PROTECTED` : if the instance constructor is protected, the visibility is extended to all of its subclasses, that is, the subclasses can also create instances of the class
* `PUBLIC` : a public instance constructor is the default visibility setting



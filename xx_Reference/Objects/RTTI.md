# Using Runtime Type Identification (RTTI)

Since the introduction of ABAP Objects, a class-based concept has been developed, called Run Time Type Identification (RTTI). RTTI determines type attributes at runtime. RTTI includes all ABAP types and covers all the functions of the now obsolete statements `DESCRIBE FIELD` and `DESCRIBE TABLE`. RTTI includes a description class for each type with special attributes for special type attributes.

The class hierarchy of the description classes corresponds to the hierarchy of types in ABAP Objects (:black_square_button: - abstract, :large_blue_circle: - instantiatable):
* :black_square_button: `CL_ABAP_TYPEDESCR`
	* :black_square_button: `CL_ABAP_DATADESCR`
		* :large_blue_circle: `CL_ABAP_ELEMDESCR`
		* :large_blue_circle: `CL_ABAP_REFDESCR`
		* :black_square_button: `CL_ABAP_COMPLEXDESCR`
			* :large_blue_circle: `CL_ABAP_STRUCTDESCR`
			* :large_blue_circle: `CL_ABAP_TABLEDESCR`
* :black_square_button: `CL_ABAP_OBJECT_DESCR`
	* :large_blue_circle: `CL_ABAP_CLASSDESCR`
	* :large_blue_circle: `CL_ABAP_INTFDESC`

To obtain a reference to a description object of a type, use the static methods of the class `CL_ABAP_TYPEDESCR` or the navigation methods of the special description class:
```ABAP
	" PUBLIC METHODS
	describe_by_name( )
	describe_by_data( )
	describe_by_object_ref( )
	get_property( )
	get_relative_name( )
	...

	" PUBLIC ATTRIBUTES
	absolute_name
	type_kind
	length
	decimals
	...
```

The description objects are then created from one of the subclasses. At runtime, exactly one description object exists for each type. The attributes of the description object contain information on the attributes of the type.

## Structured Type RTTI Descriptions

The following example shows how to identify the attributes of a structure using the subclass `CL_ABAP_STRUCTDESCR` from RTTI:
```ABAP
	DATA:
		gs_spfli TYPE spfli,
		go_descr TYPE REF TO cl_abap_structdescr,
		gs_comp TYPE aba_compdescr.

	START-OF-SELECTION.
		...
		go_descr ?= cl_abap_typedescr=>describe_by_data( gs_spfli ).
		LOOP AT go_descr->components INTO gs_comp.
			WRITE gs_comp-name.
		ENDLOOP.
```

What happens is this:
|INPUT|`CL_ABAP_TYPEDESCR`|OUTPUT|
|--|--|--|
|`gs_spfli`|`describe_by_data( )`|instance of `CL_ABAP_STRUCTDESCR`|

To identify the attributes of a structure, we first define a reference to the appropriate description class. The description class has a `COMPONENTS` attribute that you can use to describe the components of the relevant structure. Because the `COMPONENTS` attribute is an internal table, you also need to define a work area with a compatible line type.

The functional method call provides the reference to the description instance of the structure that you want to query.

The abstract class `CL_ABAP_TYPEDESCR` contains the static method `DESCRIBE_BY_DATA`. Its returning parameter is typed as a reference to this superclass. However, since the actual parameter is a reference to the subclass `CL_ABAP_STRUCTDESCR`, you need to assign the object using a down-cast.

You can then access the attributes of the description instance in any form. In this example, the program displays the component names as column headers. (For clarity, we have omitted the formatting options.)

## Object Type RTTI Descriptions

In our previous business example with the travel agency and its business partner, we specified that an instance of the vehicle rental class (`LCL_RENTAL`) reacts to an event by including the vehicle instance that triggered the event in a list. The triggering instances include both buses (`LCL_BUS`) and trucks (`LCL_TRUCK`).

To extend the example, assume that the vehicle rental company is only interested in buses. The `SENDER` parameter of the event handler method contains the reference to the triggering vehicle instance. Its dynamic object type must be analyzed to determine whether the vehicle in question is a bus or a truck:
```ABAP
	...
	METHOD on_vehicle_created.
		DATA: lo_descr TYPE REF TO cl_abap_classdescr.
		...
		lo_descr ?= cl_abap_typedescr=>describe_by_object_ref( sender ).
		IF lo_Descr->get_relative_name( ) = 'LCL_BUS'.
			APPEND sender TO mt_vehicles.
		ENDIF.
		...
	ENDMETHOD.
```

What happens is this:
|INPUT|`CL_ABAP_TYPEDESCR`|OUTPUT|
|--|--|--|
|`lcl_bus`|`describe_by_object_ref( )`|instance of `CL_ABAP_OBJECTDESCR`|

The abstract class `CL_ABAP_TYPEDESCR` has `DESCRIBE_BY_OBJECT_REF` method. You type `DESCRIBE_BY_OBJECT_REF` returning parameter as a reference to `CL_ABAP_TYPEDESCR`. However, since you type the actual parameter `GO_DESCR` on the subclass `CL_ABAP_CLASSDESCR`, you need to assign the object using a down-cast.

Now, you can access the attributes of the description instance in any way. The functional method `GET_RELATIVE_NAME` supplies the class name.

## ABAP Objects

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

### Client/Server Relationship

Objects behave like client/server systems. When one object sends a message to another object to ask it to behave in a certain way, the first object is defined as a client and the second object is defined as a server.

### Key Characteristics

Key characteristics of OO model of ABAP Objects:
* objects are a direct abstraction of the real world
* objects are units made up of data and functions
* processes can be implemented in a better way than in procedural programming

Advantages of the OO approach in regards to procedural programming model:
* improved software structure and consistency in the development process
* reduced maintenance effort and less susceptibility to errors
* better integration of the customer or user into the analysis, design, and maintenance process
* simpler and more secure extension of the software

### Classes

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

#### Attributes

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

#### Methods

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

#### Objects

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

#### Accessing Methods and Attributes

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

#### Constructors

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

#### Static Constructors

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

#### Self-Reference

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

### Inheritance

Example of inheritance implementation in ABAP:
```
	CLASS lcl_vehicle DEFINITION.
		PUBLIC SECTION.
			METHODS set_type
				IMPORTING iv_make TYPE string.
		PRIVATE SECTION.
			DATA: mv_make TYPE string.
				...
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_truck DEFINITION INHERITING FROM lcl_vehicle.
		PUBLIC SECTION.
			METHODS get_cargo
				RETURNING value(rv_cargo) TYPE s_plan_car.
		PRIVATE SECTION.
			DATA mv_cargo TYPE s_plan_car.
	ENDCLASS.
```

Example of method redefinition in ABAP:
```
	CLASS lcl_vehicle DEFINITION.
		PUBLIC SECTION.
			METHODS display_attributes.
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_truck DEFINITION INHERITING FROM lcl_vehicle.
		PUBLIC SECTION.
			METHODS display_attributes REDEFINITION. " signature is retained
				...
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_truck IMPLEMENTATION.
		METHOD display_attributes. " new implementation of the method
			...
				super->display_attributes(...) " reference to superclass method
		ENDMETHOD.
	ENDCLASS.
```

In ABAP Objects, the constructor is not inherited like normal methods. Any class can define its own constructor that is fully independent from the definition of the constructor in its superclass. A subclass can even define a constructor if there is no constructor in the superclass. 

However, when implementing the subclass constructor, it is mandatory to call the constructor of the immediate superclass. If a subclass has not changed its instance constructor, the constructor is adopted from the superclass. The implementation is also inherited from the superclass.

Example of a subclass constructor calling the superclass constructor:
```
	CLASS lcl_vehicle DEFINITION.
		PUBLIC SECTION.
			METHODS constructor IMPORTING
				iv_make_v TYPE string.
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_vehicle IMPLEMENTATION.
		METHOD CONSTRUCTOR.
			mv_make = iv_make_v.
		ENDMETHOD.
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_truck DEFINITION INHERITING FROM lcl_vehicle.
		PUBLIC SECTION.
			METHODS constructor
				IMPORTING
					iv_make_t TYPE string
					iv_cargo_t TYPE s_plan_car.
		PRIVATE SECTION.
			DATA mv_cargo TYPE s_plan_car.
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_truck IMPLEMENTATION.
		METHOD constructor.
			super->constructor( iv_make_v = iv_make_t ).
			mv_cargo = iv_cargo_t.
		ENDMETHOD.
	ENDCLASS.
```

Important to note: the concept of overloading is not supported in ABAP Objects. The usual work-around is one signature with different sets of optional parameters.

Inheritance provides an extension of the visibility concept through protected components (`PROTECTED SECTION`). The visibility of protected components lies between public and private components. Protected components are visible to all subclasses and the class itself.

The following is a summary of static components and inheritance:
* a class that defines a public or protected static attribute shares this attribute with all its subclasses
* static methods cannot be redefined
* the static constructor of a superclass is executed when the superclass or one of its subclasses is accessed for the first time
* a subclass can always have a static constructor, irrespective of the superclass
* if a subclass and its superclass both have a static constructor, then both constructors are executed when we access the subclass for the first time. If access to the superclass and subclass static constructor occurs at the same time, the superclass static constructor will execute first

#### Implementing Upcasts Using Inheritance

If you assign a subclass reference to a superclass reference, this subclass ensures that all components that you can access syntactically after the cast assignment are actually available in the instance. The user can address only those methods and attributes from the subclass instance that they can from the superclass instance.

Example in Java:
```
	interface Animal {
		public void animalSound();
	}

	class Cat implements Animal {
		public void animalSound() { ... }
		public void sleep() { ... }
	}

	class Main {
		public static void main(String[] args) {
			Animal myCat = new Cat();
			// only animalSound() is available to variable myCat
		}
	}
```

The following example explains the static and dynamic type of variable:
```
	DATA:
		go_vehicle TYPE REF TO lcl_vehicle,
		go_truck TYPE REF TO lcl_truck.

	CREATE OBJECT go_truck.
	go_vehicle = go_truck.
```

At runtime a reference variable can either be static or dynamic - in the previous code block `lcl_vehicle` is the static type of `go_vehicle`, while `lcl_truck` is its dynamic type.

#### Implementing Polymorphism Using Inheritance

A typical use for up-cast assignments is to prepare for generic access. Imagine a scenario where one list should hold all types of vehicles, be it cars, trucks or motorcycles - in such case the list would be a list of vehicles and such a list could hold all the class that inherit the vehicle class.

When objects of different classes (`LCL_BUS`, `LCL_TRUCK` and `LCL_CAR`) are specified as type superclass references (`LCL_VEHICLE`), these objects can be stored in an internal table. The shared components of the subclass objects can then be accessed uniformly. For this example, you need the method `ADD_VEHICLE` to copy the references to the vehicle types into the internal table:
```
	METHODS add_vehicle IMPORTING io_vehicle TYPE REF TO lcl_vehicle.
```

In the following example, the up-cast assignment occurs when the vehicle reference is transferred to the formal parameter of `ADD_VEHICLE` method. The shared component is generically accessed within the loop around the internal table containing all of the vehicle references. The `DISPLAY_ATTRIBUTES` method was inherited from the `LCL_VEHICLE` superclass and may have been redefined:
```
	CLASS lcl_rental DEFINITION.
		PUBLIC SECTION.
			METHODS add_vehicle IMPORTING io_vehicle TYPE REF TO lcl_vehicle.
			METHODS display_attributes.
		PROTECTED SECTION.
			DATA mt_vehicles TYPE TABLE OF REF TO lcl_vehicle.
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_rental IMPLEMENTATION.
		METHOD add_vehicle.
			APPEND io_vehicle TO mt_vehicles.
		ENDMETHOD.

		METHOD display_attributes.
			DATA lo_vehicle TYPE REF TO lcl_vehicle.
				LOOP AT mt_vehicles INTO lo_vehicle.
					lo_vehicle->display_attributes( ).
				ENDLOOP.
		ENDMETHOD.
	ENDCLASS.
```

#### Polymorphism

Consider the following code:
```
	" lcl_vehicle class
	METHOD display_attributes.
		DATA lo_vehicle TYPE REF TO lcl_vehicle.
		LOOP AT mt_vehicles INTO lo_vehicle.
			lo_vehicle->display_attributes( ).
		ENDLOOP.
	ENDMETHOD.
	" lcl_truck class
	METHOD display_attributes.
		super->display_attributes( ).
		WRITE: / 'Cargo : ', mv_cargo.
	ENDMETHOD.
	" lcl_bus class
	METHOD display_attributes.
		super->display_attributes( ).
		WRITE: / 'Passengers : ', mv_passengers.
	ENDMETHOD.
```

The implementation to be executed when `DISPLAY_ATTRIBUTES` is called depends on which object the superclass reference `LO_VEHICLE` refers to (the dynamic type, not the static type is considered).

Using dynamic function module calls, you can program generically in ABAP, even without an object-oriented programming model. When you compare dynamic function module call
with polymorphism through inheritance, the source code is less self-explanatory and is more susceptible to errors in dynamic function module call. I have not explored this topic further.

#### Implementing Downcasts Using Inheritance

Reference variables of the superclass can also refer to the subclass instances at runtime. You may copy this reference back to a reference variable of the subclass type:
```
	DATA:
		go_vehicle TYPE REF TO lcl_vehicle,
		go_truck TYPE REF TO lcl_truck,
		go_truck2 LIKE go_truck.

	CREATE OBJECT go_truck.
	go_vehicle = go_truck " upcasting
	go_truck2 ?= go_vehicle " downcasting
```

To assign a superclass reference to a subclass reference, you must use the downcast assignment operator `MOVE ... ?TO ...` or its short form `?=`. Downcast assignments are used when you need to address specific components of instances and keep the references of these components in variables that are typed on the superclass.

In regards to error handling, consider the following example:
```
	METHOD get_max_cargo.
		DATA:
			lo_vehicle TYPE REF TO lcl_vehicle,
			lo_truck TYPE REF TO lcl_truck.
		LOOP AT mt_vehicles INTO lo_vehicle.
			TRY.
				lo_truck ?= lo_vehicle.
				" determine maximum cargo of truck
				CATCH cx_sy_move_cast_error.
				" react on that cast error.
			ENDTRY.
		ENDLOOP.
```

There may be a problem if there is no truck reference in the `LO_VEHICLE` superclass reference at runtime but the downcast assignment operator tries to copy the reference to the now invalid reference `LO_TRUCK`. You can identify this exception of the error class `CX_SY_MOVE_CAST_ERROR` using the `TRY-ENDTRY` and the `CATCH` statement.

#### Usage of Class Hierarchies

If there is a suitable way to link classes in terms of inheritance this results in the following advantages:
* centralized maintenance
* safe, generic method of access

This means that the entire software component can be extended very easily.

### Interfaces

Interfaces can be seen as superclasses that cannot be instantiated, do not contain implementations, and have only public components. You can simulate multiple inheritances using interfaces.

In ABAP Objects, interfaces primarily serve to define uniform interface protocols for services. Various classes can implement these services in different ways, but you need to keep the same semantics.

Generalization and specialization relationships using interfaces:
```
	INTERFACE lif_partner.
		METHODS display_partner.
	ENDINTERFACE.
	" =-=-=-=
	CLASS lcl_rental DEFINITION.
		PUBLIC SECTION.
			INTERFACES lif_partner.
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_rental IMPLEMENTATION.
		METHOD lif_partner~display_partner.
			...
		ENDMETHOD.
	ENDCLASS.
```

Access to interface components:
```
	DATA go_rental TYPE REF TO lcl_rental.
```

Simplifying access to interface components with alias names:
```
	INTERFACE lif_partner.
		METHODS display_partner.
		DATA gv_partner_id TYPE n LENGTH 10.
	ENDINTERFACE.
	" =-=-=-=
	CLASS lcl_rental DEFINITION.
		PUBLIC SECTION.
			INTERFACES lif_partner.
			ALIASES display_partner1
				FOR lif_partner~display_partner.
		PRIVATE SECTION.
			ALIASES gv_partner_id FOR lif_partner~gv_partner_id.
	ENDCLASS.
	" =-=-=-=
	DATA go_rental TYPE REF TO lcl_Rental.
	...
	CREATE OBJECT go_rental ... .
	...
	" go_rental->lif_partner~display_partner( ).
	go_rental->display_partner1( ).
```

An interface reference can only refer to instances of classes that have implemented the interface because interfaces themselves cannot be instantiated (a typical area of use for upcast assignments is preparation for generic access):
```
	DATA:
		go_rental TYPE REF TO lcl_rental,
		go_partner TYPE REF TO lif_partner.
	CREATE OBJECT go_rental.
	go_partner = go_rental. " up-cast
```

To assign an interface reference to a class reference where the class has implemented the interface, you must use the down-cast assignment operator `MOVE ... ?TO ...` or its short form `?=`:
```
	METHOD book_flight.
		DATA:
			lo_carrier TYPE REF TO lcl_carrier,
			lo_partner TYPE REF TO lif_partner.
		LOOP AT mt_partners INTO lo_partner.
			TRY.
				lo_carrier ?= lo_partner.
				" call method of lcl_carrier to book flight
				CATCH cx_sy_move_cast_error.
					" react to cast error
			ENDTRY.
		ENDLOOP.
```

Interfaces like regular superclasses can include other interfaces. As with regular inheritance, the interface hierarchies can be of any depth. The including interface is a specialization of the included interface. The including interface is known as a compound interface, represents an extension of the included interface:
```
	INTERFACE lif_partner.
		METHODS display_partner.
	ENDINTERFACE.
	" =-=-=-=
	INTERFACE lif_lodging.
		INTERFACES lif_partner.
		METHODS book_room.
	ENDINTERFACE.
	" =-=-=-=
	CLASS lcl_hotel DEFINITION.
		PUBLIC SECTION.
			INTERFACES lif_lodging.
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_hotel IMPLEMENTATION.
		METHOD lif_partner~display_partner.
			...
		ENDMETHOD.
		METHOD lif_lodging~book_room.
			...
		ENDMETHOD.
	ENDCLASS.
```

As with regular inheritance, the implementing class only needs to list the compound interface in order to integrate all components. Nevertheless, the components of the component interfaces keep their original names:
```
	component_interface_name~component_name
```

Addressing components in compound interfaces syntax:
```
	DATA:
		go_hotel TYPE REF TO lcl_hotel,
		go_partner TYPE REF TO lif_partner,
		go_lodging TYPE REF TO lif_lodging.

	go_hotel->lif_partner~display_partner( ).
	go_hotel->lif_lodging~bok_room( ).

	" up-casts for generic access
	go_lodging = go_hotel.
	go_lodging->lif_partner~display_partner( ).
	go_lodging->book_room( ).

	go_partner = go_hotel.
	go_partner->display_partner( ).

	" down-casts for specific access again:
	go_lodging ?= go_partner.
	" or:
	go_hotel ?= go_partner.
```

If there is no suitable way to link classes in terms of inheritance creating generalization/specialization relationships using interfaces can have the following advantages:
* separation of the protocol interface - often defined by user and the service implementing class
* safe, generic method of access
* ability to simulate multiple inheritance

### Events

Instance events can be triggered by the instances of the class, but static events can be triggered by the class itself. Events can also be defined as interface components.

Given the right circumstances, handler methods react to the triggering of this event. This means that the runtime system may call these handler methods after the event has been triggered. In other words, the client usually does not call the handler method directly.

This results in a completely different modeling concept. While you are developing the class that triggers the event, you do not need to know anything about the class that is handling it. The triggering class sends a specific message to all classes and, if required, their instances. At the time of development, type of handlers and the number of handlers, which may be used are not known.



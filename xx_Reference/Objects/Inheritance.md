## Inheritance

Example of inheritance implementation in ABAP:
```ABAP
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
```ABAP
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
```ABAP
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

### Implementing Upcasts Using Inheritance

If you assign a subclass reference to a superclass reference, this subclass ensures that all components that you can access syntactically after the cast assignment are actually available in the instance. The user can address only those methods and attributes from the subclass instance that they can from the superclass instance.

Example in Java:
```ABAP
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
```ABAP
	DATA:
		go_vehicle TYPE REF TO lcl_vehicle,
		go_truck TYPE REF TO lcl_truck.

	CREATE OBJECT go_truck.
	go_vehicle = go_truck.
```

At runtime a reference variable can either be static or dynamic - in the previous code block `lcl_vehicle` is the static type of `go_vehicle`, while `lcl_truck` is its dynamic type.

### Implementing Polymorphism Using Inheritance

A typical use for up-cast assignments is to prepare for generic access. Imagine a scenario where one list should hold all types of vehicles, be it cars, trucks or motorcycles - in such case the list would be a list of vehicles and such a list could hold all the class that inherit the vehicle class.

When objects of different classes (`LCL_BUS`, `LCL_TRUCK` and `LCL_CAR`) are specified as type superclass references (`LCL_VEHICLE`), these objects can be stored in an internal table. The shared components of the subclass objects can then be accessed uniformly. For this example, you need the method `ADD_VEHICLE` to copy the references to the vehicle types into the internal table:
```ABAP
	METHODS add_vehicle IMPORTING io_vehicle TYPE REF TO lcl_vehicle.
```

In the following example, the up-cast assignment occurs when the vehicle reference is transferred to the formal parameter of `ADD_VEHICLE` method. The shared component is generically accessed within the loop around the internal table containing all of the vehicle references. The `DISPLAY_ATTRIBUTES` method was inherited from the `LCL_VEHICLE` superclass and may have been redefined:
```ABAP
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

### Polymorphism

Consider the following code:
```ABAP
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

Using dynamic function module calls, you can program generically in ABAP, even without an object-oriented programming model. When you compare dynamic function module call with polymorphism through inheritance, the source code is less self-explanatory and is more susceptible to errors in dynamic function module call. I have not explored this topic further.

### Implementing Downcasts Using Inheritance

Reference variables of the superclass can also refer to the subclass instances at runtime. You may copy this reference back to a reference variable of the subclass type:
```ABAP
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
```ABAP
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

### Usage of Class Hierarchies

If there is a suitable way to link classes in terms of inheritance this results in the following advantages:
* centralized maintenance
* safe, generic method of access

This means that the entire software component can be extended very easily.

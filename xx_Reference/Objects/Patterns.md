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

### Implementing Factory Methods

Classes that restrict the visibility of the instance constructor provide a public static method that creates an instance of this class and returns the reference to this instance to the caller. Such methods are sometimes called factory methods. Factory methods are executed when an instance of the class is created and has some advantages as compared to the instance constructor.

Advantages of factory methods:
* *a factory method can have coding that executes before the actual instantiation*: for example, the method can perform checks, read data from the database, or set locks, before actually creating the instance. If an error occurs, no instance is created; and, no memory is allocated for it
* *classes can have more than one factory method. Each of the methods can have a different implementation and signature as well*: the different factory method makes it possible to create instances of the same class based on different information. For example, one factory method creates a new instance that offers input parameters for all attributes, while another factory method only imports key information and then retrieve the other attribute values from the database
* *use factory methods to administrate the instances of a class within the class itself*: the administration of the instance within the class is often used to avoid the creation of many identical instances (a reference of every new instance is stored in a private static attribute of the class - an internal table)
* *use a factory method to instantiate one of the subclasses rather than the class itself*: as an example, the factory method of an airplane class can check the plane type, then instantiate either a cargo or a passenger plane

An example for a class with factory method:
```ABAP
	CLASS-METHODS:
		factory
			IMPORTING
				...
			RETURNING value(ro_plane) ...
	" =-=-=-=
	METHOD factory.
		READ TABLE gt_instances INTO ro_plane ...

		IF sy-subrc <> 0.
			CREATE OBJECT ro_plane ... .
			APPEND ro_plane TO gt_instances.
		ENDIF.
	ENDMETHOD.
```

### Implementing the Singleton Pattern

Use the singleton concept to prevent a class from being instantiated more than once for the same program context.

An example of a singleton class using a factory method:
```ABAP
	CLASS-METHODS:
		get_instance
			...
			RETURNING value(ro_instance).
	" =-=-=-=
	METHOD get_instance.
		IF go_instance IS BOUND.
			ro_instance = go_instance.
		ELSE.
			CREATE OBJECT go_instance ... .
			ro_instance = go_instance.
	ENDMETHOD.
```

The class uses its static constructor to create the single instance in advance. The `GET_INSTANCE` method does not create the instance but only returns a reference to the already existing instance:
```ABAP
	CLASS-METHODS:
		class_constructor,
		get_instance
			IMPORTING ...
			RETURNING value(ro_instance).
	" =-=-=-=
	METHOD get_instance.
		ro_instance = go_instance.
	ENDMETHOD.

	METHOD class_constructor.
		CREATE OBJECT go_instance ... .
	ENDMETHOD.
```

### Implementing Factory Classes Using Friendship

In some cases, classes have to work together so closely that one class needs access to the other classes protected and private components. Similarly, one class might need to be able to create instances of the other class regardless of the visibility of the constructor. To avoid making these options available to all users of the class, use the concept of class friendship. A class can grant friendship to other classes and interfaces and, hence, to all classes that implement the interface.

To create friendship, use the `FRIENDS` addition of the `CLASS` statement or the `FRIENDS` tab page in the **Class Builder**. All classes and interfaces to which friendship is granted are listed there.

Granting friendship is unilateral.

A class granting friendship is not automatically a friend of the classes’ friends. If a class granting friendship wants to access the non-public components of a friend, this friend must also explicitly grant friendship to it.

Definition of friendship relationships:
```ABAP
	CLASS lcl_1 DEFINITION CREATE PRIVATE FRIENDS lcl_2.
		...
		PRIVATE SECTION.
			DATA a1 ... .
		...
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_2 DEFINITION.
		...
		METHOD meth.
			DATA lo_1 TYPE REF TO lcl_1. " LCL_1 grants friendship to LCL_2
			CREATE OBJECT lo_1.			 " instantiation despite PRIVATE
			MOVE lo_1->a1 TO ...		 " direct access to a1
		ENDMETHOD.
		...
	ENDCLASS.
```

A typical application of the friends concept is the definition of a factory class. Like the factory method, a factory class creates and administrates the instances of a class. By outsourcing the administration to a dedicated class, the class itself is kept smaller and easier to understand:
```ABAP
	CLASS-METHODS:
		create_airplane
			IMPORTING ...
			RETURNING value(ro_plane).
	" =-=-=-=
	METHOD create_airplane.
		READ TABLE gt_airplanes INTO ro_plane ...

		IF sy-subrc <> 0.
			CREATE OBJECT ro_plane ... .
			APPEND ro_plane TO gt_airplanes.
		ENDIF.
	ENDMETHOD.
```

In the example, `LCL_FACTORY` serves as a factory class for airplanes. `LCL_FACTORY` class provides a public method `CREATE_AIRPLANE` in which `CREATE_AIRPLANE` either instantiates class `LCL_AIRPLANE` or returns a reference to an already existing instance. To restrict the instantiation, class `LCL_AIRPLANE` is defined with addition `CREATE PRIVATE`. By adding `FRIENDS LCL_FACTORY`, the friendship allows the factory class and only the factory class to create airplane instances and to access the private attributes.

The friend attribute is inherited. Classes that inherit from friends and interfaces containing a friend as a component interface, also become friends. Therefore, we advise that extreme caution must be taken when granting friendship. The higher up a friend is in the inheritance tree, the more subclasses can access all components of a class that grants friendship.

Conversely, granting friendship is not inherited. A friend of a superclass is, therefore, not automatically a friend of its subclasses.

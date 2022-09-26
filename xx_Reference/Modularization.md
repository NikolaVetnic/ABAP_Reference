## Modularization

Modularization involves placing specific sequences of ABAP statements in a module, instead of placing all the statements in a single main program, for reasons of readability and ease of reusing. Additionally, modularization has the added benefit of making ABAP programs easier to support and enhance after they have been written.

### Implementation vs. Definition

The definition lists the different attributes and methods of a class (must come before the implementation and before an object is created from the class), whereas the implementation contains the actual code for all of the methods. The class is defined by using the `CLASS` keyword, followed by the name of the class and then either `DEFINITION` or `IMPLEMENTATION`. 

Definition of a class with no attributes or methods (`lcl` indicates that it’s a local class):
```
	CLASS lcl_car DEFINITION.
	ENDCLASS.

	CLASS lcl_car IMPLEMENTATION.
	ENDCLASS.
```

### Creating Objects

Instead of `TYPE` we use `TYPE REF TO` to indicate the class to be used when creating the object (use the prefix `o_` to indicate an object):
```
	CLASS lcl_car DEFINITION.
	ENDCLASS.

	DATA:
	    o_car TYPE REF TO lcl_car.
    
	CREATE OBJECT o_car.

	CLASS lcl_car IMPLEMENTATION.
	ENDCLASS.
```

### Public and Private Sections

Appropriate sections are defined in the class implementa- tion using the `PUBLIC SECTION` and `PRIVATE SECTION` keywords:
```
	CLASS lcl_car DEFINITION.
	  PUBLIC SECTION.
	    DATA:
	      d_fuel         TYPE i,
	      d_speed        TYPE i,
	      d_brand        TYPE string,
	      d_manufacturer TYPE string READ-ONLY.
	  PRIVATE SECTION.
	    DATA:
	        d_gear TYPE i.
	ENDCLASS.

	CLASS lcl_car IMPLEMENTATION.
	ENDCLASS.
```

### Class Methods

Methods are defined using the keyword `METHODS` in the class definition, and then the method’s code is written in the class implementation in between the words `METHOD` and `ENDMETHOD`:
```
	CLASS lcl_car DEFINITION.
	  PUBLIC SECTION.
	    DATA:
	      d_fuel         TYPE i,
	      d_speed        TYPE i,
	      d_brand        TYPE string,
	      d_manufacturer TYPE string.
	    METHODS:
	      accelerate,
	      decelerate,
	      refuel.
	  PRIVATE SECTION.
	    DATA:
	        d_gear TYPE i.
	ENDCLASS.

	CLASS lcl_car IMPLEMENTATION.
	  METHOD accelerate.
	    d_speed = d_speed + 5.
	    d_fuel = d_fuel - 5.
	  ENDMETHOD.

	  METHOD decelerate.
	    d_speed = d_speed - 5.
	    d_fuel = d_fuel - 2.
	  ENDMETHOD.

	  METHOD refuel.
	    DATA:
	        ld_max TYPE i VALUE 100.
	    d_fuel = ld_max.
	  ENDMETHOD.
	ENDCLASS.
```

The code for calling methods has to be included after the object has been defined and created, but it can come before the definition of the object:
```
	...
	  PRIVATE SECTION.
	    DATA:
	        d_gear TYPE i.
	ENDCLASS.

	DATA:
	    o_car TYPE REF TO lcl_car.

	CREATE OBJECT o_car.
	o_car->accelerate( ).

	CLASS lcl_car IMPLEMENTATION.
	  METHOD accelerate.
	...

```

### Importing, Returning, Exporting, and Changing

There are a few ways to pass data to and from methods:
|Importing|A copy of one or more variables is passed to the method|
|--|--|
|Returning|The actual variable is returned by the method. Returning can only be used to return one variable.|
|Exporting|A copy of one or more variables are returned from the method|
|Changing|The actual variable is passed to the method, and any changes to that variable will change the original (also known as passing by reference).|

You can change the accelerate method to import a variable to indicate the amount of speed that you want to increase by:
```
	...
	    METHODS:
      accelerate IMPORTING ip_accel_rate TYPE i,
	...
	CREATE OBJECT o_car.
	o_car->accelerate( 5 ).
	...
	  METHOD accelerate.
	    d_speed = d_speed + ip_accel_rate.
	...
```

You can also mark parameters as optional by adding OPTIONAL after the parameter’s definition, which means that the parameter will have an initial value when the method runs if a value is not entered for that parameter.

Next, you can change the method to check if the fuel is at zero - return a `Boolean` parameter that will be true if the method worked and false if it did not:
```
	...
	    METHODS:
	      accelerate IMPORTING ip_accel_rate TYPE i RETURNING VALUE(rp_is_success) TYPE BOOL,
	...
	DATA:
	    o_car TYPE REF TO lcl_car,
	    d_is_success TYPE bool.

	CREATE OBJECT o_car.
	d_is_success = o_car->accelerate( 5 ).
	...
	  METHOD accelerate.
	    IF d_fuel - 5 > 0.
	      d_speed = d_speed + ip_accel_rate.
	      d_fuel = d_fuel - 5.
	      rp_is_success = abap_true.
	    ELSE.
	      rp_is_success = abap_false.
	    ENDIF.
	  ENDMETHOD.
	...
```

The returned variable can also be used in line with other ABAP key- words using *method chaining*:
```
	DATA:
		o_car TYPE REF TO lcl_car,
		d_is_success TYPE bool.

	CREATE OBJECT o_car.
	IF o_car->accelerate( 5 ) = abap_true.
	  WRITE: 'It worked!', /.
	ENDIF.
```

The `RETURNING` parameter only allows you to return one parameter, but if you need to return multiple parameters, you can use `EXPORTING` parameter (the parameters that are `EXPORTING` from the method will be `IMPORTING` into the main program) - you cannot use both `EXPORTING` and `RETURNING` in the same method definition:
```
	...
	    METHODS:
	      accelerate IMPORTING ip_accel_rate TYPE i 
	                 EXPORTING ep_is_success TYPE bool,
	      decelerate,
	      refuel.
	...
	CREATE OBJECT o_car.
	o_car->accelerate( EXPORTING ip_accel_rate = 5
	                   IMPORTING ep_is_success = d_is_success ).
	
	IF d_is_success = abap_true.
	...
```

The last option for passing variables to or from methods is `CHANGING`. `CHANGING` parameters are passed into a method like `IMPORTING` parameters but can also be changed and returned like `EXPORTING` parameters. When possible, use `RETURNING` parameters to help write more concise, easier-to-read code.

### Constructors

A constructor is created by creating a method called constructor. This method will be called by the `CREATE OBJECT` keyword, and any required parameters must be passed when using `CREATE OBJECT`:
```
	CLASS lcl_car DEFINITION.
	  PUBLIC SECTION.
	    DATA:
	      d_fuel         TYPE i,
	      d_speed        TYPE i,
	      d_model        TYPE string,
	      d_manufacturer TYPE string.
	
	    METHODS:
	      accelerate
	        IMPORTING ip_accel_rate        TYPE i
	        RETURNING VALUE(rp_is_success) TYPE bool,
	      decelerate,
	      refuel,
	      constructor
	        IMPORTING
	          ip_manufacturer TYPE string
	          ip_model        TYPE string.
	  PRIVATE SECTION.
	    DATA:
	        d_gear TYPE i.
	ENDCLASS.
	
	DATA:
	  o_car        TYPE REF TO lcl_car,
	  d_is_success TYPE bool.
	
	CREATE OBJECT o_car
	  EXPORTING
	    ip_manufacturer = 'Toyota'
	    ip_model        = 'Tundra'.
	d_is_success = o_car->accelerate( 5 ).
	
	CLASS lcl_car IMPLEMENTATION.
	  METHOD constructor.
	    d_manufacturer = ip_manufacturer.
	    d_model = ip_model.
	  ENDMETHOD.
	...
	ENDCLASS.
```

The constructor is typically used to set variables that determine the state of the object, but it can be used to initialize the object in any way (e.g. it can read data from a database table to fill some private or public variables). Remember that the constructor will only be run once when creating the object.

### Recursion

A method to calculate a given number of the Fibonacci sequence:
```
	CLASS lcl_fibonacci DEFINITION.
	  PUBLIC SECTION.
	    METHODS: calculate IMPORTING ip_place        TYPE i
	                       RETURNING VALUE(rp_value) TYPE i.
	ENDCLASS.

	DATA: o_fib    TYPE REF TO lcl_fibonacci,
	      d_result TYPE i.
      
	CREATE OBJECT o_fib.
	d_result = o_fib->calculate( 5 ).
	WRITE: d_result, /.

	CLASS lcl_fibonacci IMPLEMENTATION.
	  METHOD calculate.
	    IF ip_place <= 1.
	      rp_value = ip_place.
	    ELSE.
	      rp_value = calculate( ip_place - 1 ) + calculate( ip_place - 2 ).
	    ENDIF.
	  ENDMETHOD.
	ENDCLASS.
```

### Inheritance

Class `lcl_truck` inherits from the car class by using the `INHERITING FROM` keyword as part of the class definition:
```
	...
	CLASS lcl_truck DEFINITION INHERITING FROM lcl_car.
	ENDCLASS.

	DATA:
	  o_car        TYPE REF TO lcl_car,
	  o_truck      TYPE REF TO lcl_truck,
	  d_is_success TYPE bool.
	
	CREATE OBJECT o_truck
	  EXPORTING
	    ip_manufacturer = 'Volvo'
	    ip_model        = 'Roadster'.
	o_truck->accelerate( 5 ).
	...
	CLASS lcl_truck IMPLEMENTATION.
	ENDCLASS.
```

We also have the option of redefining a method so that we use the code inside the `lcl_truck` implementation instead of the `lcl_car` implementation:
```
	...
	CLASS lcl_truck DEFINITION INHERITING FROM lcl_car.
	  PUBLIC SECTION.
	    METHODS: accelerate REDEFINITION.
	ENDCLASS.
	...
	CLASS lcl_truck IMPLEMENTATION.
	  METHOD accelerate.
	    d_speed = 1.
	    rp_is_success = abap_true.
	  ENDMETHOD.
	ENDCLASS.
```

If you want to create attributes or methods that are private but can also be accessed from inside of a subclass, then you must define them in the protected section:
```
	...
	PROTECTED SECTION.
	  DATA: d_protected TYPE i.
	...
```

### Global Classes

Local classes are created and run inside of a local program, however, there are many use cases in which you’ll want to use classes across multiple programs. A common use of global classes is to create interfaces with custom tables that you have written, which allows for:
* handle locking of tables within the methods of your class, instead of having to lock and unlock tables in the programs that access the table, and
* writing of methods that allow you to access the data in your cus- tom table(s) without having to write your own SELECT statements.

The code for creating and changing the classes is mostly the same as what you saw for local classes.

One thing that is different is the way that they’re broken up into different pieces. Each piece has its own source control history and must be activated on its own. The breakup of a class into different pieces makes it possible for multiple developers to work on the same class simultaneously.

The different pieces of a global class are the public, private, and protected sections and each method implementation. This allows you to treat each method as an individual program in terms of source control and activation. The public, private, and protected sections are a bit of a special case, however. These sections are automatically generated when using the forms view in SAP GUI, so any comments entered in these areas will be overwritten. You can still create and edit these sections with your own code, which will be kept, but any comments will be lost.

Global class structure:
```
	CLASS znvcl_global_class DEFINITION
	  PUBLIC
	  FINAL
	  CREATE PUBLIC .

	  PUBLIC SECTION.
	  PROTECTED SECTION.
	  PRIVATE SECTION.
	ENDCLASS.

	CLASS znvcl_global_class IMPLEMENTATION.
	ENDCLASS.
```

The only change is the addition of the `PUBLIC` keyword, indicating a global class. Additionally, `FINAL` keyword is optional and indicates that the class cannot be inherited from and the `CREATE PUBLIC` keyword, allows an object to be created from the class anywhere where the class is visible

### Functions

Start by inspecting the function using transaction `SE37` and checking out the *Attributes* tab page. Next, examine the function module interface: it consist of *Import*, *Export*, and *Changing* parameters, and can also include *Exceptions*. 

*Import* parameters specify the data that the function module requires from the calling program. *Export* parameters specify the data that is returned to the calling program. 

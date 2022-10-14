## Class-Based Exceptions

An exception is a situation that arises while a program is being executed during which there is no point to continue the normal program flow. SAP NetWeaver Application Server (SAP NetWeaver AS) S 6.10 introduced a new ABAP Objects exception concept that exists parallel to the existing concept based on `sy-subrc`. Exceptions and exception handling are now based on classes. This new ABAP Objects exception concept enhanced the classic way of handling exception using `sy-subrc`.

In the new exception concept, an exception is represented by an exception object (an instance of an exception class) whose attribute values contain information about the respective error situation. **Raising a class-based exception** means instantiating an exception class and setting the attributes. **Handling a class-based exception** involves evaluating the exception object and its attribute values.

Class-based exceptions are raised either by the `RAISE EXCEPTION` statement or by the runtime environment. You catch and handle class-based exceptions with the `TRY...CATCH...ENDTRY` structure.

The names of global exception classes always start with `<namespace>CX_`. Those global exception classes that the runtime environment uses start with `CX_SY_`. We recommend that you start the names of local exception classes with `LCX_`.

All exception classes are derived from one exception class, `CX_ROOT`. Therefore, you can generically access any exception object through a reference variable, `REF TO CX_ROOT`. However, a new exception class is not allowed to inherit directly from `CX_ROOT`, derive any new exception class directly or indirectly from one of the subclasses `CX_ROOT` - `CX_NO_CHECK`, `CX_DYNAMIC_CHECK`, or `CX_STATIC_CHECK`.

Through this, all exception classes are subdivided into three groups. Depending on the group to which a given exception belongs, the exception is treated differently by syntax check and runtime environment. The default group is `CX_STATIC_CHECK`, which ensures maximum syntax check and program stability. Use the other groups only in special cases.

Through this, all exception classes are subdivided into three groups. Depending on the group to which a given exception belongs, the exception is treated differently by syntax check and runtime environment. The default group is `CX_STATIC_CHECK`, which ensures maximum syntax check and program stability. Use the other groups only in special cases.

### Class-Based Exception Handling

You can handle an exception if the statement that raised it is enclosed inside a `TRY-ENDTRY` control structure:
```ABAP
	TRY.
		... " statements for which exceptions are to be handled
		CATCH cx_... cx_... cx_... [INTO gx_exc1].
			... " exec'ed if specific exc classes or their subclasses are raised
		CATCH cx_... [INTO gx_exc2].
			... " exec'ed if specific exc classes or their subclasses are raised
		CLEANUP.
			... " exec'ed if there is no lcl exc handler (exc is to be propagated)
	ENDTRY.
```

Example syntax for handling predefined exceptions:
```ABAP
	PARAMETERS:
		pa_int1 TYPE i,
		pa_int2 TYPE i.
	
	DATA:
		gv_result TYPE i,
		gv_text TYPE string,
		gx_exc TYPE REF TO cx_root.

	TRY.
		gv_result = pa_int1 * pa_int2.
		WRITE gv_result.

		CATCH cx_sy_arithmetic_overflow INTO gx_exc.
			gv_text = gx_exc->get_text( ).
			MESSAGE gv_text TYPE 'I'. " displays an info message pop-up
	ENDTRY.
```

### Defining and Raising Exceptions

To define global exception classes:
1. in the `SE24` Class Builder in the unnamed dropdown list on the left of the screen choose **Class / Interface**
2. in the unnamed field enter the new exception class name, use the prefix `ZCX_`, press **Enter**
3. to create the class choose **Yes**, after which a **Create Class** dialogue box displays
4. select the **Exception Class** radio button and, if you want the exception texts to come from an existing message class, choose the **With Message Class** checkbox
5. enter a short description
6. do not change the default superclass `CX_STATIC_CHECK`, then choose **Save** and then the **Create Object Directory Entry** dialogue box displays
7. in the **Create Object Directory Entry** dialog box enter your package name and choose **Save** - a prompt for transportable **Workbench** request dialog box displays
8. in the **Create Object Directory Entry** dialog box accept the defaults and choose **Continue**
9. define additional attributes in your exception class if necessary (e.g. to use in the exception texts)

Use of the `RAISE EXCEPTION` statement:
```ABAP
	" =-=-=-= raise exception with new exception object
	RAISE EXCEPTION TYPE cx_invalid_planetype " create and raise in one statement
		EXPORTING
			PLANETYPE = ... . " set values of attributes with constructor
	" =-=-=-= raise exception with existing object
	DATA:
		lx_exception TYPE REF TO cx_invalid_planetype.
	...
	* [ CATCH cx_invalid_planetype INTO lx_exception.
	*   or
	*   CREATE OBJECT lx_exception ] " first catch or create exc object
	...
	RAISE EXCEPTION lx_exception " then raise exc with existing object
```

The second statement uses an existing exception object that the one `<object_ref>` points to. The exception object was either created directly using a `CREATE OBJECT` statement or caught in a previous `CATCH ... INTO ...` statement.

When using the first variant (with new exception object), it is possible to provide values for the constructor parameters using the `EXPORTING` addition. One of these parameters is used to set the exception text in the new exception object.

### Exception Texts

All exception classes offer an optional parameter `TEXTID` in their constructors. Use this parameter if more than one message text is available in the exception class and you do not want to raise the exception with the default text:
```ABAP
	" =-=-=-= using the default text (const_name = class_name)
	RAISE EXCEPTION TYPE cx_invalid_planetype
		EXPORTING
			...
			* TEXTID = cx_invalid_planetype=>cx_invalid_planetype " default text
			... .
	" =-=-=-= using alternative text
	RAISE EXCEPTION TYPE cx_invalid_planetype
		EXPORTING
			...
			TEXTID = cx_invalid_planetype=>cx_invalid_planetype2 " alt text
	" =-=-=-= returns the text chosen when the object was created
	DATA lx_exception TYPE REF TO cx_invalid_planetype.
	...
	CATCH cx_invalid_planetype INTO lx_exception.
	lv_text = lx_exception->get_text( ).
	...
```

For each text defined on the Texts tab page, the Class Builder generates a public constant of the same name. Constants may also be inherited from the superclasses of the exception class. By default, the system raises an exception with the text that has the same name as the exception class. To raise the exception with another text, use the corresponding constant as the actual parameter for constructor parameter `TEXTID`.

### Exception Propagation

The procedure can propagate the exception to its caller, and the caller can then handle the exception or propagate it to its own caller. To propagate an exception use the `RAISING` addition when you define the procedure interface:
```ABAP
	METHODS meth_name ... 
		RAISING cx_... cx_... 
	" =-=-=-= or 
	FORM subr_name ... 
		RAISING cx_... cx_....
```

The `RAISING` addition is followed by a list of the exception classes whose instances are to be propagated.

### The Hierarchy of Predefined Exception Classes

The choice of the superclass influences the way that the syntax check and the runtime environment handle a given exception:
* `CX_STATIC_CHECK` : if an exception class inherits from `CX_STATIC_CHECK`, you must either handle the relevant exception or propagate it using the `RAISING` addition. If the exception is neither handled nor propagated using the `RAISING` addition, the syntax check displays a warning. When you define a new global exception class, `CX_STATIC_CHECK` is defined as the superclass by default
* `CX_DYNAMIC_CHECK` : for subclasses of `CX_DYNAMIC_CHECK`, the syntax check displays no warning if exceptions are neither handled nor propagated with the `RAISING` addition. If an exception is raised at runtime and you neither handle nor propagate it, the system ends the program with a runtime error.
* `CX_NO_CHECK` : for subclasses of `CX_NO_CHECK`, you cannot propagate the corresponding exceptions explicitly using the `RAISING` addition. If you do not handle these exceptions in the processing block where they occur, they are automatically propagated. If the calling block does not handle the exceptions, they are automatically propagated further on to the highest call hierarchy level. If the exceptions are not handled on the top level, a runtime error occurs at the point where they were raised. Some predefined exceptions with the prefix `CX_SY_...` for error situations in the runtime environment are subclasses of `CX_NO_CHECK`

### Exception Handling

After an exception was caught in a `CATCH` statement, you can handle it in many different ways:
1. continue the program behind an `ENDTRY` statement after taking one of the following actions:
	* ignoring the exception (do nothing)
	* issuing a warning
	* writing to a protocol
	* correcting the situation
2. remove the cause of the error and start again from one of the following points:
	* from the beginning of the corresponding `TRY` block using statement `RETRY`
	* from where the exception occurred using statement `RESUME`
3. raise and propagate one of the following exceptions:
	* the same exception object again using `RAISE EXCEPTION <obj_ref>`
	* a new exception using `RAISE EXCEPTION TYPE <exc_class>`

### The `RETRY` Statement

When you handle an exception in a `CATCH` block, use the `RETRY` statement to go back to the `TRY` statement of the respective `TRY-ENDTRY` structure:
```ABAP
	...
	METHODS
		constructor
			IMPORTING ...
			RAISING cx_exc.
	...
	" =-=-=-=
	METHOD constructor.
		...
		RAISE EXCEPTION TYPE cx_exc
			EXPORTING
				... .
		...
	ENDMETHOD.
	" =-=-=-=
	DATA:
		go_plane TYPE REF TO lcl_airplane,
		go_exc TYPE REF TO cx_exc.
	...
	TRY.
		CREATE OBJECT go_plane
			EXPORTING ... . " constructor raises an exception
		...
		CATCH cx_exc INTO gx_exc. " exception is caught here
		" < remove the cause of the exception >
		RETRY. " retries the entire TRY-ENDTRY structure
	ENDTRY.
```

**CAUTION:** if you do not remove the cause of the exception properly, your program will go into an infinite loop.

### Implementation of Resumable Exceptions

Use the `RESUME` statement to resume a program immediately after the statement that raised the exception in the source code. Following are the prerequisites to use the `RESUME` statement:
1. the exception must be caught with `CATCH` statement using the addition `BEFORE UNWIND`. This ensures that the context of the exception is kept alive for a possible `RESUME`. If the `CATCH` block exited without the `RESUME` statement, the system deletes the context of the exception after the `CATCH` block is exited.
2. the exception must be raised with the `RAISE RESUMABLE ...` variant of the `RAISE EXCEPTION` statement. This prepares the raising processing lock for the `RESUME`.
3. if the exception is propagated, you must mark it as resumable on all hierarchy levels by using the `RAISING RESUMABLE ( ... )` addition with the name of the exception class inside the brackets. This prepares all methods that propagate the exception for a possible `RESUME`.

The handler of a given exception checks whether, at runtime a given exception was raised and propagated resumable or not. All exception objects provide public instance attribute `IS_RESUMABLE`, which is set to `’X’` or `’ ’` by the framework. If you resume a non-resumable exception, you cause a runtime error (exception class `CX_SY_ILLEGAL_HANDLER`).

In the following example the method `GET_TECH_ATTR` raises and propagates the exception; the constructor propagates the exception further. All raising and propagating is setup to be resumable. The main program handles the exception with `CATCH BEFORE UNWIND ...`, checks that the exception indeed is resumable, and issues the `RESUME` statement. The system resumes the execution of `GET_TECH_ATTR` immediately after the `RAISE RESUMABLE EXCEPTION` statement is executed:
```ABAP
	" =-=-=-= class of object go_plane DEFINITION
		METHODS:	
			constructor
				IMPORTING ...
				RAISING RESUMABLE(cx_exc).
			get_tech_attr
				IMPORTING ...
				EXPORTING ...
				RAISING RESUMABLE(cx_exc).
		...
	" =-=-=-= class of object go_plane IMPLEMENTATION
		METHOD constructor.
			...
				CALL METHOD ... " calls get_tech_attr
					EXPORTING ...
					IMPORTING ... .
			...
		ENDMETHOD.

		METHOD get_tech_attr.
			...
				RAISE RESUMABLE EXCEPTION TYPE cx_exc EXPORTING ... .
			...
		ENDMETHOD.
	" =-=-=-= main program
	...
		TRY.
			CREATE OBJECT go_plane
				EXPORTING ... . " calls constructor which calls get_tech_attr
			...
			CATCH BEFORE UNWIND cx_exc INTO gx_exc.
			IF gx_exc->is_resumable = 'X'.
				RESUME.
			ENDIF.
			...
		ENDTRY.
	...
```

### Re-Raising and Mapping of Exceptions

You raise class-based exceptions with one of the following variants of statement `RAISE EXCEPTION`:
* `RAISE EXCEPTION TYPE <exception_class> [EXPORTING ...]` : this statement creates a new exception object, which is an instance of class `<exception_class>`. Optionally, values can be provided for the constructor using the `EXPORTING` addition.
* `RAISE EXCEPTION <object_ref>` : this statement uses an existing exception object, namely whichever exception object `<object_ref>` points to. This exception object is either created directly, using a `CREATE OBJECT` statement or, more commonly, caught in a previous `CATCH ... INTO ...` statement and pass explicitly to the caller.

In the example, the constructor catches exception `CX_EXC`, which is raised by the `GET_TECH_ATTR` method. The constructor analyzes the exception object, performs necessary adjustments, issues a warning, and so on. The constructor then decides to pass the exception to the main program, where the exception is handled again:
```ABAP
	" =-=-=-= class of object go_plane DEFINITION
		METHODS:	
			constructor
				IMPORTING ...
				RAISING cx_exc2.
			get_tech_attr
				IMPORTING ...
				EXPORTING ...
				RAISING cx_exc.
		...
	" =-=-=-= class of object go_plane IMPLEMENTATION
		METHOD constructor.
			...
			TRY.
				CALL METHOD ... " calls get_tech_attr
				...
				CATCH cx_exc INTO lx_exc.
				...
				RAISE EXCEPTION TYPE cx_exc2
					EXPORTING
						previous = lx_exc.
				...
			ENDTRY.
		ENDMETHOD.

		METHOD get_tech_attr.
			...
				RAISE EXCEPTION TYPE cx_exc EXPORTING ... .
			...
		ENDMETHOD.
	" =-=-=-= main program
	...
		TRY.
			CREATE OBJECT go_plane
				EXPORTING ... . " calls constructor which calls get_tech_attr
			...
			CATCH cx_exc2 INTO gx_exc.
			...
		ENDTRY.
	...	
```

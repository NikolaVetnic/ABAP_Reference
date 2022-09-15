## Error Handling

### `SY-SUBRC`

The most basic type of error handling is to check the value of the system variable `sy-subrc` after completing an action. If `sy-subrc` has a value of `0` it will always mean success:
```
	DATA:
	  s_flight  TYPE sflight,
	  s_flight2 LIKE sflight.

	s_flight-carrid = 'NV'.
	s_flight-connid = 17.
	s_flight-fldate = 20150101.

	MODIFY sflight FROM s_flight.

	" actual error checking
	IF sy-subrc = 0.
	  WRITE: 'All ok!'.
	ENDIF.
```

This approach also allows checking OpenSQL statements for success:
```
	SELECT * FROM sflight INTO TABLE @DATA(flights).

	IF sy-subrc = 0.
	  WRITE: 'All ok!'.
	ELSE.
	  WRITE: 'An error has occurred...'.
	ENDIF.
```

### Message Classes

A message class is used as a single area for success, warning, and error messages that will be displayed to users or used for logging purposes across multiple programs and other development objects.

Calling messages from ABAP program:
```
	MESSAGE s000(ZNV_MY_MESSAGES).
```

Possible message types:
|Message Type|Description|
|--|--|
|`s`|Success Message|
|`i`|Information Message|
|`w`|Warning|
|`e`|Error|

A success message will be displayed with a bar at the bottom of the page (double-click the message to display the long text in a popup). An information message will be displayed as a popup with a checkmark button to continue and a question mark button to view the long text. An error message will be displayed at the bottom of the screen like a success message, but it will show an exclamation mark (it will stop the program from executing any additional code).

You can also use the `WITH` keyword to include your own text or a variable value to be displayed as part of the message instead of the `&` symbol (up to four `&` characters can be placed within the message text and used in the `MESSAGE ... WITH ...` command):
```
	MESSAGE e001(ZMY_MESSAGES) WITH 'An Error Occurred'.
	...
	PARAMETERS: p_value TYPE i.
	  MESSAGE e002(ZMY_MESSAGES) WITH p_value.
```

### Screen Validation

When used within the `AT-SELECTION-SCREEN` event, an error message can be used to validate the data being entered and only allow for valid entries:
```
	SELECTION-SCREEN BEGIN OF BLOCK selection.
	  PARAMETERS:
	    p_param TYPE i.
	SELECTION-SCREEN END OF BLOCK selection.

	AT SELECTION-SCREEN.
	  IF p_param = 0.
	    MESSAGE e002(znv_my_messages) WITH p_param.
	  ENDIF.

	START-OF-SELECTION.
	  WRITE: 'END'.
```

A warning message can only be used as part of the selection screen validation and will require the user to press `Enter` to continue (will be displayed at the bottom of the screen).

You can also use the message keyword without a message class by declaring the message and the type only (not possible to use long text this way):
```
	MESSAGE ‘This is my message’ TYPE ‘S’.
```

### Exception Classes

An exception is *thrown* or *raised* when it occurs, after which it can be *caught* or *handled*. After the exception is raised, it will stop executing current code and start executing code within a `CATCH` statement, which can be in the current program or any parent calling program. 

#### Unhandled Exceptions

When an exception goes unhandled, a runtime error occurs and a *short dump* is created. The following snippet throws exception if `0` is entered as parameter:
```
	SELECTION-SCREEN BEGIN OF BLOCK selection.
	  PARAMETERS:
	      p_param TYPE i.
	SELECTION-SCREEN END OF BLOCK selection.

	START-OF-SELECTION.
	  DATA:
	      result TYPE i.
	
	  result = 10 / p_param.
	
	  WRITE: ' result : ', result.
```

No matter how the user was accessing the system, the short dump is logged and can be viewed by entering Transaction ST22.

#### `TRY/CATCH` Statements

An example of a `TRY/CATCH` block:
```
	  TRY.
	      result = 10 DIV p_param.
	    CATCH cx_sy_zerodivide.
	      WRITE: 'Exception caught!', /.
	  ENDTRY.
```

You may want to catch all exceptions, no matter what the type is. To do so, catch the exception `CX_ROOT`, which is the superclass for all other exceptions.

You can access the exception that occurred using the `INTO` keyword to save the exception details into an object of the exception type that you’re catching or a superclass of that exception:
```
	...
	  DATA:
	      result TYPE i,
	      my_exception TYPE REF TO cx_sy_zerodivide.

	  TRY.
	      result = 10 DIV p_param.
	    CATCH cx_sy_zerodivide INTO my_exception.
	      MESSAGE e001(znv_my_messages) WITH my_exception->get_text( ).
	  ENDTRY.
```

When using the `INTO` keyword, you don’t need to use the `CREATE OBJECT` keyword because the exception was already created when it was thrown; you’re just copying it into a local object that you can access with your code.

You can raise any standard or custom exception in your own code by using `RAISE EXCEPTION TYPE` followed by the exception class that you want to raise:
```
	RAISE EXCEPTION TYPE cx_sy_zero_divide.
```

Exceptions can also be raised as *resumable*, meaning that after being caught within a program or method, the program can decide to continue the execution after the exception was thrown:
```
	TRY.
	     RAISE RESUMABLE EXCEPTION TYPE cx_sy_zerodivide.
	     WRITE: 'resumed'.
	CATCH BEFORE UNWIND cx_sy_zerodivide.
	     RESUME.
	ENDTRY.
```

#### Nonhandleable Exceptions

Even when the program code has been written to catch and handle all the possible exceptions, a short dump could still occur. Many times, these errors are not caused by bad ABAP code.

### Custom Exception Classes

Before you create a new exception class, you need to determine the category (superclass) of the exception: `CX_STATIC_CHECK`, `CX_DYNAMIC_CHECK` or `CX_NO_CHECK`.

When using `CX_STATIC_CHECK` as the custom exception’s superclass, the compiler will return a warning if the exception is not handled by each method or passed to the calling method using the `RAISING` keyword in the definition. The `CX_DYNAMIC_CHECK` exception type will act in the same way, except that a compiler warning will not be issued. The third type of exception is `CX_NO_CHECK`, which cannot be passed using `RAISING` in any method definition but can still be handled using a `TRY...CATCH` block from either the method that throws the exception or a method that calls that method.

You typically use `CX_NO_CHECK` when the exception can occur almost anywhere. In general, this type of exception cannot be handled cleanly, so you’re not concerned with warning the developer to include a check for the exception.

The `CX_STATIC_CHECK` exception type should be used when the exception should be handled by the method where it can occur or by a method calling the method where it occurs. The compiler warning for these types of exceptions will help to ensure that the exception is handled cleanly.

The `CX_DYNAMIC_CHECK` type of exception should be used when the exception can be avoided by checking a precondition — for example, checking if the denominator in a division method is zero before using it to divide a number. 

When creating a global class, you need to prefix the name of the class with `Z`, and to indicate that it’s an exception class you should use the `ZCX` prefix instead of `ZCL`:
```
	CLASS zcx_custom_exception DEFINITION
	  PUBLIC
	  INHERITING FROM cx_static_check
	  FINAL
	  CREATE PUBLIC .
	  PUBLIC SECTION.
	    INTERFACES if_t100_message .
	    METHODS constructor
	      IMPORTING
	        !textid   LIKE if_t100_message=>t100key OPTIONAL
	        !previous LIKE previous OPTIONAL .
	  PROTECTED SECTION.
	  PRIVATE SECTION.
	ENDCLASS.
	CLASS zcx_custom_exception IMPLEMENTATION.
	  METHOD constructor.
	    CALL METHOD super->constructor
	      EXPORTING
	        previous = previous.
	    CLEAR me->textid.
	    IF textid IS INITIAL.
	      if_t100_message~t100key =
	        if_t100_message=>default_textid.
	    ELSE.
	      if_t100_message~t100key = textid.
		ENDIF.
	  ENDMETHOD.
	ENDCLASS.
```

If you want additional custom attributes stored in your exception, you can add them to the new exception class here. Now you can set the custom data value when raising the exception and then retrieve the value when you catch the exception:
```
	DATA:
	  custom_exception TYPE REF TO zcx_custom_exception,
	  my_string        TYPE string.
	  
	TRY.
	    my_string = |More Information|.
	    "Do Something
	    RAISE EXCEPTION TYPE zcx_custom_exception
	      EXPORTING
	        ip_custom_data = my_string.
	  CATCH zcx_custom_exception INTO custom_exception.
	    WRITE: custom_exception->custom_data.
	ENDTRY.
```

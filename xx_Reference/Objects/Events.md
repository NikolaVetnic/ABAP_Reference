# Events

Instance events can be triggered by the instances of the class, but static events can be triggered by the class itself. Events can also be defined as interface components.

Given the right circumstances, handler methods react to the triggering of this event. This means that the runtime system may call these handler methods after the event has been triggered. In other words, the client usually does not call the handler method directly.

This results in a completely different modeling concept. While you are developing the class that triggers the event, you do not need to know anything about the class that is handling it. The triggering class sends a specific message to all classes and, if required, their instances. At the time of development, type of handlers and the number of handlers, which may be used are not known.

An example of event emitting and handling can be viewed in these files:
* [`znvcl_ev_vehicle`](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/Snippets_BC401_EN_Col18/bc401_05_znvcl_ev_vehicle)
* [`znvcl_ev_rental`](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/Snippets_BC401_EN_Col18/bc401_06_znvcl_ev_rental)
* [`znv_ev_main`](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/Snippets_BC401_EN_Col18/bc401_07_znv_ev_main)

The visibility of an event defines where the event can be handled: `PUBLIC` - can be handled anywhere, `PROTECTED` - can only be handled in its own class or its subclasses, `PRIVATE` - can only be handled in its own class.

The visibility of an event handler defines where the handler method can be registered: `PUBLIC` - can be registered anywhere in the program, `PROTECTED` - can be registered within its own class or its subclasses, `PRIVATE` - can only be registered within its own class.

Events can be defined as interface components - triggering and handling events are done the same way as in classes: **1)** define event in an interface, **2** trigger the interface event in implementing classes, **3)** handle interface event in handler class (client class), **4)** register event handling.

## Events in Procedural ABAP Programs

Structure and event procedures of an executable ABAP program can be seen in the following listing:
```ABAP
	REPORT ...						" program start
	PARAMETERS ...
	DATA ...

	INITIALIZATION.					" initialization
		<statements>				" dynamic preassignment
									" display selection screen
	AT SELECTION-SCREEN.			" at selection screen (process-after-input on selection screen)
		<statements>				" checks

	START-OF-SELECTION.				" start of selection
		<statements> - WRITE ...	" main processing - display list buffer as list
```

Code listing given above illustrates the basic events that are triggered, and the sequence in which they are triggered; the program implements the corresponding processing blocks. An executable ABAP program is thus a collection of processing blocks that are processed for the respective events. Outputs created by means of `WRITE` statements are stored in list buffers, and are only displayed as a list after the system has processed the `START-OF-SELECTION` block.

Characteristics of event blocks:
* introduced with an event keyword
* ends by beginning the next processing block
* cannot be nested (nesting would contradict the concept of ABAP events)
* existence is not absolutely necessary (system does not execute any statement and triggers the next event if the processing block is missing)
* sequence of event blocks is unimportant
* implicit standard event block in executable program: `START-OF-SELECTION` - if no blocks are implemented in the program, the system assigns all statements to the standard processing block `START-OF-SELECTION`

### The `INITIALIZATION` Event

The following example contains a selection screen with an input field for a date. Under normal circumstances, the current date should appear as the default value (`DEFAULT sy-datum`). However, under certain conditions (`IF`), the date of the same weekday of the following week (`pa_date = pa_date + 7.`) is to be displayed as the default value:
```ABAP
	REPORT ...
	PARAMETERS pa_date LIKE sy-datum 
		DEFAULT sy-datum. 					" pa_date == 20051224
	
	INITIALIZATION.
		IF ...								" dynamic preassignment
			pa_date = pa_date + 7.			" pa_date == 20051231
		ENDIF.

	AT SELECTION-SCREEN.
		...									" empty processing if block does not exist
	
	START-OF-SELECTION.
		WRITE pa_date.
```

### The `AT SELECTION-SCREEN` Event

Consider the following code listing:
```ABAP
	REPORT ...
	PARAMETERS ...

	INITIALIZATION
		...

	AT SELECTION-SCREEN.
		<input/authorization check>
		IF ...
			MESSAGE e...
		ENDIF.
	
	START-OF-SELECTION.
		...
		MESSAGE e...						" program terminates with error message
		...
```

From the selection screen, the `AT SELECTION-SCREEN` event is triggered both by choosing Enter as well as F8 (Execute). After the corresponding processing block has been processed, if the users chooses the Execute button, the following `START-OF-SELECTION` event is triggered and the processing starts. However, if the user chooses Enter, the system again displays the selection screen.

### Syntax Example - Authorization Check with Error Dialog

The following code listing illustrates a sipmle program with an authorization check and an error dialog on the selection screen:
```ABAP
	PARAMETERS pa_car TYPE s_carr_id.
	
	CONSTANTS gc_actvt_display TYPE activ_auth VALUE '03'.

	" event processed after leaving the selection screen
	
	AT SELECTION-SCREEN.
		
		TRY.
			CALL METHOD cl_bc400_flightmodel=>check_authority
				EXPORTING
					iv_carrid = pa_car
					iv_activity = gc_actvt_display
		CATCH cx_bc400_no_auth.
			" show selection screen again with error message
			MESSAGE e046(bc400) WITH pa_car.
		ENDTRY.
	
	START-OF-SELECTION.
		...
```

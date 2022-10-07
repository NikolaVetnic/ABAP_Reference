## BAdI

With a BAdI (Business Add-Ins), an SAP application provides the enhancement option through an interface and an adapter class implementing that interface. The interface can be implemented by several users in the delivery chain, thus, multiple BAdI implementations are possible. Additionally, a BAdI implementation itself can provide another BAdI.

When you define a BAdI, you must specify an interface (`IF_EX_<badi>`) with corresponding formally defined methods. The adapter class that is automatically generated during the interface definition (`CL_EX_<badi>`) has, among other capabilities, the function of calling all active implementations of the BAdI. When you have several active implementations, there is no predefined processing sequence:
```
	INTERFACE IF_EX_<badi>. " BAdI interface
		DATA: a1 ...

		METHODS meth1
			EXPORTING
				e_p1
				e_p2
			IMPORTING
				i_p1.
		METHODS meth2. 
		...
	ENDINTERFACE.
	" =-=-=-=
	CLASS CL_EX_<badi> DEFINITION. " BAdI adapter class
		PUBLIC SECTION.
			INTERFACES IF_EX_<badi>.
	ENDCLASS.
	" =-=-=-=
	CLASS CL_EX_<badi> IMPLEMENTATION.
		METHOD IF_EX_BADI~meth1.
			...
		ENDMETHOD.

		METHOD IF_EX_BADI~meth2.
			...
		ENDMETHOD.
	ENDCLASS.
```

The following block of code shows an example of a BAdI call:
```
	REPORT <program_using_badi>.

	DATA: gb_adapter TYPE REF TO IF_EX_<badi>.

	START-OF-SELECTION.
	* =-=-=-=
	*** this demo shows a classical BAdI call
	*** it is available since 4.6 release
		cl_exithandler=>get_instance( 
			CHANGING instance = gb_adapter ).

		gb_adapter->meth1( 
			IMPORTING
				ev_p1 = ... 
				ev_p2 = ...
			EXPORTING
				iv_p1 = ... ).
```

A reference variable for the type of BAdI must be defined. An object of the adapter class is instantiated by the call of the `GET_INSTANCE` static method of the `CL_EXITHANDLER` class. The variable `gb_adapter` points to this instance. The interface methods of the BAdI can be called by `gb_adapter` object reference.

You can search the BAdI using the following strategies:
* `SE84` Repository Information System
* `SE81` Application Hierarchy
* `SPRO` Customizing Guide
* search in the application source code for the `CL_EXITHANDLER=>GET_INSTANCE` statement
* search in the application source code for the occurrence of the BAdI interfaces with the naming convention `IF_EX_`
* search in the application source code for occurrences of the `GET BADI` statement

When the name of the BAdI is determined, the BAdI can be implemented. The implementation of the BAdI is performed through the implementation maintenance under **Tools → ABAP Workbench → Utilities → Business Add-Ins → Implementation** (transaction code `SE19`).

To implement a BAdI, a BAdI implementation name must be issued. The naming convention is Z<impl>. A dialog box then appears for selecting the corresponding BAdI.

The code to be implemented is stored in a method of an automatically generated customer class. For this reason, the name of the implementing class must be given in a final dialog box. The default name from SAP is comprised of `Y` or `Z` (the namespace prefix), `CL_` (for class), `IM_` (for implementation), and `<impl>` (the actual name of the implementation). When you double-click one of the BAdI methods, you can enter the code of the method. You can also create auxiliary methods in the implementing class to better structure the required code of the BAdI method.

How to implement a BAdI:
* run `SE19`
* **Create Implementation → Classic BAdI**, enter the BAdI name `BADI_BC401` and then **Create** button
* enter an implementation name (e.g. `ZBC401_00_IMPL`) and then **Ok** button
* enter a description text and then **Save** button
* double-click the name of the implementing class to navigate to the class and implement your source code there (It is sufficient to implement `WRITE 'Hello World'` statement in method `ENHANCE_LIST.`)
* activate your implementing class
* execute program `SAPBC401_BAD_D1` and demonstrate that the BAdI implemenetation is not yet executed
* return to the BAdI implementation (`SE19`) and choose Activate business add-in implementation
* execute program `SAPBC401_BAD_D1` and demonstrate that now the additional coding is executed

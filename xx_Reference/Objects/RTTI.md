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

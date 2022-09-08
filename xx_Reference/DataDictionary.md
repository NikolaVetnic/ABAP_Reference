## Data Dictionary Data Types

DDic objects are the preferred method to create variables in ABAP programs that work with the database.

Create a variable based on a transparent table (equivalent to a single row of the table), after which one can access any of the components of the `sflight` structure using a dash `-` followed by the field name. Finally, it is possible to modify the table using the structure from the program:
```
	DATA:
    	s_flight  TYPE sflight.
		s_flight2 LIKE sflight.

	s_flight-carrid = 'NV'.
	s_flight-connid = 17.
	s_flight-fldate = 20150101.

	MODIFY sflight FROM s_flight.
```

In the above snippet `LIKE` is used to create another variable of the type of `s_flight`.

You can create your own structures within your ABAP code using the `TYPES` keyword instead of the `DATA` keyword that we used for defining variables. The fields of a structure are defined in between the `BEGIN OF <name>` and `END OF <name>` keywords. Each field is defined with a type, which works just like defining any other variable type, so you can use a basic data type or an ABAP Data Dictionary-based data type. Note that the name is prefixed with `y_` indicating that it is a structure defined in our code. A common use for your own structure data types is to save the results of a `SELECT` statement that doesn’t use all of the fields in the table (when we cannot use inline data declarations):
```
	TYPES:
	    BEGIN OF ynv_my_type,
	        carrid TYPE sflight-carrid,
	        connid TYPE sflight-connid,
	    END OF ynv_my_type.

	DATA:
	    s_my_type TYPE ynv_my_type.

	SELECT SINGLE carrid connid
	INTO s_my_type
	FROM sflight.

	WRITE: s_my_type-carrid, ' - ', s_my_type-connid.
```

From within our code we can also create a table from our own structure or from a structure defined in the ABAP Data Dictionary:
```
	TYPES:
	    BEGIN OF ynv_my_type,
	        carrid TYPE sflight-carrid,
	        connid TYPE sflight-connid,
	    END OF ynv_my_type,
    
	    ynvt_my_type TYPE STANDARD TABLE OF ynv_my_type.
```

### Field Symbols

Field symbols point to areas of memory instead of actually being loaded with the data themselves, i.e. they allow access to data in working memory without making a copy of that data (similar to pointers used in e.g. C). Additionally, changing a field symbol will change the data that it is pointing to.

Declaration example:
```
	FIELD-SYMBOLS:
		<s_sflight> TYPE sflight.
```

Assigning a value to a field symbol using the `ASSIGN` keyword makes it so that changes to a field symbol are applied to the variable as well:
```
	DATA:
	    s_flight TYPE sflight.
	FIELD-SYMBOLS:
	    <s_flight> TYPE sflight.

	ASSIGN s_flight TO <s_flight>.
	* ASSIGN s_flight TO FIELD-SYMBOL(<s_flight>). " inline data declaration

	s_flight-carrid = 'AA'.
	WRITE: 's_flight-carrid : ', s_flight-carrid, /.

	<s_flight>-carrid = ''.
	WRITE: 's_flight-carrid : ', s_flight-carrid, /.
```

After creating a field symbol, it must be assigned a value before you can access it. If you try to access it before you assign a value to it, you will see a `GETWA_NOT_ASSIGNED` exception. This is the same as a null pointer error in other programming languages. You can check if a field symbol is assigned a value in memory via `IF <field symbol> IS ASSIGNED`.

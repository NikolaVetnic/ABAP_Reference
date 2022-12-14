# Tables

Notes:
* a working memory table - an internal table
* you must always prefix your appending components with ZZ so that it doesn’t conflict with future SAP field names; yes, that’s right: two Zs, not just one like in append structure ID and program names

## Standard Table

Requires no key.

The most basic table type (akin to transparent table), typically data is selected from a database and stored in a standard table. Use the Open SQL statement `SELECT` to program database read access:
```ABAP
	SELECT <fields> FROM <table> INTO <target> WHERE <condition>.
	" <fields> which columns
	" <table> which tables
	" <target> where to?
	" <conition> which rows?
``` 

Anytime you store data from a `SELECT` statement in a table created using inline data declarations, you store the data in a standard table - this technique is called an *array fetch* and it in effect copies the selected part of the database directly into an internal table instead of doing so row-by-row:
```ABAP
	SELECT *
	INTO TABLE @DATA(results)
	FROM sflight.
```

The `SELECT` statement parts:
```ABAP
	SELECT <fields> " which columns?
	FROM <table> " which tables?
	INTO <target> " where to?
	WHERE <condition>. " which rows?
```

Creating a standard table containing the same structure of the `sflight` table:
```ABAP
	DATA: 
		t_sflight TYPE STANDARD TABLE OF sflight.
```

When using a structure based on a transparent table, that table is not created with any keys defined. Defining a key is optional, but a key is required to complete some of the commands that we will explore later in this section. A standard table can have a nonunique primary key added (no exception is thrown in case of inserting two rows with the same key). A key is defined by adding the KEY keyword followed by the fields that make up the key:
```ABAP
	DATA: 
		t_flight TYPE STANDARD TABLE OF sflight
		WITH KEY carrid connid fldate.
```

If you created a table using the LIKE keyword, you will not be able to define keys for that table. However, any keys defined in the variable being copied, would also exist in the variable defined using `LIKE` as shown below:
```ABAP
	DATA:
		t_flight TYPE STANDARD TABLE OF sflight
		WITH KEY carrid connid fldate,
	
		t_flight_copy LIKE t_flight.
```

It is also possible to create tables of custom data structures without the use of a table type using `TYPE STANDARD TABLE OF` which can then be used to store results from a `SELECT` statement:
```ABAP
	TYPES:
	    BEGIN OF ynv_my_type,
	        carrid TYPE sflight-carrid,
	        connid TYPE sflight-connid,
	    END OF ynv_my_type.

	DATA:
	    t_my_type TYPE STANDARD TABLE OF ynv_my_type.

	SELECT carrid connid
	INTO TABLE t_my_type
	FROM sflight.
```

It is also possible to use variables whose type refers to a structure in the ABAP dictionary (global structure type) or a structure type that is declared locally in the program. In this case, the variable basically matches the structure of a table row:
```ABAP
	DATA gs_flight TYPE sflight.
```

Local types are defined thusly:
```ABAP
	TYPES:
		BEGIN OF ts_flightinfo,
			carrid		TYPE s_carr_id,
			carrname	TYPE s_carrname,
			connid		TYPE s_conn_id,
			fldate		TYPE s_date,
			percentage	TYPE p LENGTH 3 DECIMALS 2,
		END OF ts_flightinfo.

	DATA
		gs_flightinfo TYPE ts_flightinfo.
```

### Data Retreival from Client-Specific Tables

A database table is classified as a client-specific table if it has a client field (data type `CLNT`) as the first key column and contains client-specific entries. If you select data from a client-specific table without specifying the client, only data records from the current client are read (a restriction to the current client is automatically added to the `WHERE` clause of the `SELECT` statement). 

If you want to read data from an explicitly specified client, specify the client name in the `WHERE` clause. However, the `CLIENT SPECIFIED` addition must be made after the `FROM` clause:
```ABAP
	SELECT * FROM spfli CLIENT SPECIFIED
		INTO ... WHERE  mandt = 402 AND carrid = 'AA'.
```

### `READ TABLE`

It is possible to read a single record from a standard table using the `READ TABLE` keyword, thus reading either `INTO` a var or `ASSIGNING` a field symbol:
```ABAP
	DATA:
	    t_flights TYPE STANDARD TABLE OF sflight,
	    s_flight TYPE sflight.

	FIELD-SYMBOLS:
	    <s_flight> TYPE sflight.

	SELECT *
	FROM sflight
	INTO TABLE t_flights.

	READ TABLE t_flights INTO s_flight INDEX 1.
	READ TABLE t_flights ASSIGNING <s_flight> INDEX 1.

	WRITE: 's_flight-carrid : ', s_flight-carrid, /.
	WRITE: 's_flight-carrid : ', <s_flight>-carrid, /.
```

Using inline data declarations (the first record’s index is 1, not 0):
```ABAP
	SELECT *
	FROM sflight
	INTO TABLE @DATA(t_flights).

	READ TABLE t_flights INTO DATA(s_flight) INDEX 1.
	READ TABLE t_flights ASSIGNING FIELD-SYMBOL(<s_flight>) INDEX 1.
```

When you use the `INTO` keyword with a variable, a copy of the record is made and stored in the variable. This means that changes to that variable will not affect the corresponding row in the table. Because you aren’t copying any data into memory, using the field symbol will lead to better.

### `LOOP AT`

Just as `READ TABLE` is used to look at a single record from within a standard table, `LOOP AT` is used to cycle through all of the records within a standard table and insert them into a variable of field symbol:
```ABAP
	DATA:
	    t_flights TYPE STANDARD TABLE OF sflight,
	    s_flight TYPE sflight.
    
	FIELD-SYMBOLS:
	    <s_flight> TYPE sflight.
    
	SELECT *
	FROM sflight
	INTO TABLE t_flights.

	LOOP AT t_flights INTO s_flight.
	    s_flight-carrid = 'AZ'.
	ENDLOOP.

	" changes every row of the standard table to have a carrid of AZ
	LOOP AT t_flights ASSIGNING <s_flight>.
	    <s_flight>-carrid = 'AZ'.
	ENDLOOP.
```

The statement `LOOP AT table_name INTO var_name. ... ENDLOOP.` is used to loop through the table rows and place them into `var_name` for access within the loop.

Inline data declarations are again a possibility:
```ABAP
	SELECT *
	FROM sflight
	INTO TABLE @DATA(t_flights).

	" limit the rows that will be looped with the WHERE
	LOOP AT t_flights INTO DATA(s_flight) WHERE fldate > '20150101'.
	    s_flight-carrid = 'AZ'.
	ENDLOOP.

	LOOP AT t_flights ASSIGNING FIELD-SYMBOL(<s_flight>) WHERE fldate > '20150101'.
	    <s_flight>-carrid = 'AZ'.
	ENDLOOP.
```

### `INSERT` and `MODIFY`

When inserting it is necessary to use a structure of the same type as table:
```ABAP
	DATA:
	    s_flight_row TYPE sflight,
	    t_flights TYPE STANDARD TABLE OF sflight.

	s_flight_row-carrid = 'NV'.
	s_flight_row-connid = '017'.

	INSERT s_flight_row INTO TABLE t_flights.
```

If there are two tables of the same structure, there’s a quick way to combine them without having to iterate through each record:
```ABAP
	DATA:
	    t_flights1 TYPE STANDARD TABLE OF sflight,
	    t_flights2 TYPE STANDARD TABLE OF sflight,
	    d_flight   TYPE sflight.

	d_flight-carrid = 'NV'.
	d_flight-connid = '019'.
	INSERT d_flight INTO TABLE t_flights1.

	d_flight-carrid = 'VV'.
	INSERT d_flight INTO TABLE t_flights1.

	INSERT LINES OF t_flights1 INTO TABLE t_flights2.
```

It is possible to change multiple rows of a standard table with `MODIFY` keyword by adding the `TRANSPORTING` and `WHERE` commands:
```ABAP
	DATA:
	    t_flights TYPE STANDARD TABLE OF sflight,
	    s_flight TYPE sflight.
    
	SELECT *
	INTO TABLE t_flights
	FROM sflight.

	s_flight-price = 500.

	MODIFY t_flights FROM s_flight
	    TRANSPORTING price WHERE carrid = 'AA'.

	" modifying the record at index 5
	MODIFY t_flights FROM s_flight INDEX 5 
		TRANSPORTING price.
```

### `DELETE`

If you already have the row that you’re using in a local data structure, you can use that structure to indicate the row that needs to be deleted. The `DELETE TABLE` command uses the primary key of the structure to find and delete the corresponding row from the standard table. If no primary key is defined when the standard table is defined, then the key is made up of the entire row, meaning the structure must match an entire row of the table:
```ABAP
	DATA:
	    t_flights TYPE STANDARD TABLE OF sflight.

	FIELD-SYMBOLS:
	    <s_flight> TYPE sflight.
    
	SELECT *
	INTO TABLE t_flights
	FROM sflight.

	READ TABLE t_flights ASSIGNING <s_flight> INDEX 2.
	DELETE TABLE t_flights FROM <s_flight>.
```

When you have a key defined in the table, you can use a structure that only has the key defined to delete the corresponding record from the standard table:
```ABAP
	DATA:
	    t_flights TYPE STANDARD TABLE OF sflight WITH KEY carrid connid fldate,
	    s_flight TYPE sflight.

	SELECT *
	INTO TABLE t_flights
	FROM sflight.

	s_flight-carrid = 'AA'.
	s_flight-connid = 17.
	s_flight-fldate = '20150107'.

	DELETE TABLE t_flights FROM s_flight.
```

You can also specify the key in the DELETE command itself:
```ABAP
	DELETE TABLE t_flights WITH TABLE KEY 
		carrid = 'AA' connid = 17 fldate = '20150107'.
```

You can also delete rows in a standard table based on a WHERE clause:
```ABAP
	DELETE t_flights WHERE carrid = ‘AA’ AND connid = 17.
```

### Join Mechanism

There is often a requirement to read data from various tables and display it on one table. In general, the technique with the best performance for this task is a table join.

An example of joining tables `SPFLI` (flight info) and `SCARR` (carrier info) - direct join implementation in the program (ABAP Join):
```ABAP
	SELECT ...
		FROM spfli INNER JOIN scarr
		ON spfli~carrid = scarr~carrid
		WHERE ...
```

## Transparent Table

A transparent table in the ABAP Dictionary is the description of the corresponding database table which contains the actual data used by the application. The fields of the transparent table form columns in the corresponding database table with identical names. Data elements, which you already know as globally defined elementary data types, are normally used for describing individual fields. Data elements refer to domains for their technical properties.

The definition of a transparent table appears to be very similar to the definition of a global structure type. Transparent tables can be used in the same way as structure types in programming. For example, transparent tables can be used to define a structured data object (structure variable), for typing an interface parameter, or as the line type of a global or a local table type. Only the list of fields is important. Other properties of the transparent table, such as the key definition or the technical properties, are irrelevant when it is being used as a data type.

In addition to the properties related to the database, there is another difference between transparent tables and structure types. A transparent table is a list of elementary fields, whereas the components of a structure type can themselves be structured again (nested structures). The components of a structure can even be typed with a table type (deep structures).

## Internal Table

An internal table is a data object in which you can keep several identically structured data records at runtime (table variable). The number of data records is restricted only by the capacity of specific system installations. The ABAP runtime system dynamically manages the size of the internal table.

Typical uses of internal table:
* storing of data from database tables or sequential files for future processing
* preparation of data for screen or printer output (for example, sort)
* preparation of data for using other services (for example, for method, function module, or subroutine calls)

Definition of internal tables with local types example:
```ABAP
	TYPES:
		BEGIN OF ts_type,
			carrid TYPE s_carr_id,
			connid TYPE s_conn_id,
			... ,
		END OF ts_type.

	DATA
		gt_itam TYPE STANDARD/SORTED/HASHED TABLE OF ts_type WITH (NON-)UNIQUE KEY...
```

Possible usage scenarios of internal tables:
* `APPEND gs TO gt_itab.` - add to the end internal table
* `INSERT gs INTO TABLE gt_itab <condition>.` - insert into table according to condition
* `READ TABLE gt_itab INTO gs <condition>.` - get from table according to condition
* `MODIFY TABLE gt_itab FROM gs <condition>.` - change table entry according to...
* `DELETE gt_itab <condition>.`

Possible scenarios of processing of sets of records:
* `LOOP AT gt_it INTO gs <condition>. ... ENDLOOP.` - processing records one by one over the entire (or a part of) the internal table
* `DELETE gt_it <condition>.` - deleting several records
* `INSERT LINES OF gt_it1 <condition> INTO gt_it2 <condition>.` - inserting several rows from another internal table
* `APPEND LINES OF gt_it1 <condition> TO gt_it2.` - appending several rows from another internal table

Syntax example - reading by index:
```ABAP
	LOOP AT gt_flightinfo INTO gs_flightinfo FROM 1 TO 5.
		WRITE: / gs_flightinfo-carrid, gs_flightinfo-connid.
	ENDLOOP.
```
```ABAP
	READ TABLE gt_flightinfo INTO gs_flightinfo INDEX 3.
	WRITE: / gs_flightinfo-carrid, gs_flightinfo-connid.
```

Syntax example - reading by key:
```ABAP
	LOOP AT gt_flightinfo INTO gs_flightinfo WHERE carrid = 'LH'.
	WRITE: / gs_flightinfo-carrid, gs_flightinfo-connid.
```
```ABAP
	READ TABLE gt_flightinfo INTO gs_flightinfo WITH TABLE KEY carrid = 'LH' ... .
	IF sy-subrc = 0.
		WRITE: / gs_flightinfo-carrid, gs_flightinfo-connid.
	ENDIF.
```

## Sorted Table

Requires either unique or non-unique keys.

A sorted table can be searched through at a quicker rate than a standard table, but will be slightly slower when inserting records than a standard table.

When defining a sorted table, using a unique key is will cause an exception to occur if a record is inserted with that uses an existing key:
```ABAP
	DATA:
	    t_sorted_flights1 TYPE SORTED TABLE OF sflight
	        WITH UNIQUE KEY carrid connid fldate,
	        WITH NON-UNIQUE KEY carrid connid.
```

We can also sort standard tables using the `SORT` keyword (can’t sort a table of the sorted type):
```ABAP
	DATA:
	    t_flights TYPE STANDARD TABLE OF sflight.
    
	SELECT *
	FROM sflight
	INTO TABLE t_flights.
	
	SORT t_flights BY carrid connid ASCENDING fldate DESCENDING.
```

An insert using a key example (using an index could cause sort order exception):
```ABAP
	DATA:
	    t_flights TYPE SORTED TABLE OF sflight
	        WITH NON-UNIQUE KEY carrid connid fldate,
	    s_flight TYPE sflight.
	    
	SELECT *
	FROM sflight
	INTO TABLE t_flights.
	
	s_flight-carrid = 'VN'.
	s_flight-connid = 19.
	s_flight-fldate = '20150404'.
	
	INSERT s_flight INTO TABLE t_flights. " uses primary key
```

Changing rows using the sorted table’s primary key will always work, and the mass change `MODIFY` is acceptable as long as the field being changed is not part of the sorted table’s key:
```ABAP
	MODIFY TABLE t_flights FROM s_flight. " uses primary key
```

There are no additional rules for deleting rows in a sorted table,
because deleting rows will preserve the sorted order.

### `BINARY SEARCH`

`BINARY SEARCH` searches through the (sorted) table much quicker than `READ TABLE` or `LOOP AT`:
```ABAP
	DATA:
	    t_flights TYPE SORTED TABLE OF sflight
	        WITH UNIQUE KEY carrid connid fldate.
	FIELD-SYMBOLS:
	    <s_flight> TYPE sflight.

	SELECT *
	INTO TABLE t_flights
	FROM sflight.

	READ TABLE t_flights ASSIGNING <s_flight>
	WITH KEY carrid = 'AA' connid = '0017' fldate = '20140423' BINARY SEARCH.

	IF sy-subrc = 0.
	    WRITE: 'Record found.'.
	ENDIF.
```

### `DELETE ADJACENT DUPLICATES FROM`

Delete records with the same primary key value that are adjacent to each other (if no key is defined, the key will be the entire record):
```ABAP
	DELETE ADJACENT DUPLICATES FROM t_flights.
```

You can use the COMPARING command to define the specific fields that you want to use to find duplicates, or COMPARING ALL FIELDS to delete only records that completely match:
```ABAP
	DELETE ADJACENT DUPLICATES FROM t_flights COMPARING ALL FIELDS.
	...
	DELETE ADJACTENT DUPLICATES FROM t_flights COMPARING connid fldate.
```

## Hashed Table

Requires unique keys.

The actual table contents are stored unsorted in memory, and a hash board is created that contains the unique keys for the data - the table can thus be sorted in any order without violating the hash board and unique key:
```ABAP
	DATA:
	    t_flights TYPE HASHED TABLE OF sflight
	        WITH UNIQUE KEY carrid connid fldate.
	
	SELECT *
	INTO TABLE t_flights
	FROM sflight.
	
	SORT t_flights BY price.
```

Hashed tables can be read at a rate much faster than standard or sorted tables (requires the entire key):
```ABAP
	DATA:
	    t_flights TYPE HASHED TABLE OF sflight
	        WITH UNIQUE KEY carrid connid fldate.
	FIELD-SYMBOLS:
	    <s_flight> TYPE sflight.

	SELECT * INTO TABLE t_flights FROM sflight.

	READ TABLE t_flights ASSIGNING <s_flight>
	WITH TABLE KEY carrid = 'AA' connid = '0017' fldate = '20140423'.
```

We can also use `READ TABLE...WITH KEY` instead of `WITH TABLE KEY` to avoid using the full primary key, but this read would ignore the hash table as if the table was a standard table. Same goes for `LOOP AT` keyword.

### Inserting, Changing and Deleting Hashed Table Rows

The only way to `INSERT` or `MODIFY` a row is by utilizing the table key:
```ABAP
	DATA:
	    t_flights TYPE HASHED TABLE OF sflight
	        WITH UNIQUE KEY carrid connid fldate,
	    s_flight TYPE sflight.

	s_flight-carrid = 'AA'.
	s_flight-connid = 17.
	s_flight-fldate = '20150101'.
	INSERT s_flight INTO TABLE t_flights.

	s_flight-fldate = '20150102'.
	INSERT s_flight INTO TABLE t_flights.

	s_flight-price = 200.
	MODIFY TABLE t_flights FROM s_flight.
```

Trying to insert a record that uses an existing key will not cause an exception, but the command will set the value of `sy-subrc` to `4`.

Deleting rows of a hashed table works just like as in standard and sorted tables.

## Performance

Summary of the performance diferences among the different table types:
|Table Type|Standard|Sorted|Hash|
|--|--|--|--|
|`INSERT`|Fastest|Middle|Slowest|
|`READ`|Slowest|Middle (with `BINARY SEARCH`)|Fastest (using table key)|
|`UPDATE`|Same|Same|Same|
|`DELETE`|Same|Same|Same|

## Copying Table Data

Using the assignment operator `=`:
```ABAP
	DATA:
	    t_table1 TYPE TABLE OF sflight,
	    t_table2 TYPE TABLE OF sflight.

	SELECT *
	INTO TABLE t_table1
	FROM sflight.

	t_table2 = t_table1.
```

The following snippet copies all of the records in table `t_sflight` into table `t_flight_price`, even though table `t_flight_price` contains only a subset of the fields in `t_sflight`:
```ABAP
	TYPES:
	    BEGIN OF ynv_flight_price,
	        carrid TYPE sflight-carrid,
	        connid TYPE sflight-connid,
	        price  TYPE sflight-price,
	    END OF ynv_flight_price.

	DATA:
	    t_sflight TYPE STANDARD TABLE OF sflight,
	    t_flight_price TYPE STANDARD TABLE OF ynv_flight_price.
    
	SELECT * INTO TABLE t_sflight FROM sflight.

	MOVE-CORRESPONDING t_sflight TO t_flight_price.
```

## Displaying Data from Working Memory

A common ABAP program is a report that will pull a selection of data from the database and display the results to the user. We can display the results to the user using an ALV grid:
```ABAP
	DATA:
	    t_flights TYPE STANDARD TABLE OF sflight,
	    gr_alv TYPE REF TO cl_salv_table.

	SELECT * INTO TABLE t_flights FROM sflight.

	cl_salv_table=>factory( importing r_salv_table = gr_alv
	                        changing t_table = t_flights ).
	gr_alv->display( ).
```

With inline data declarations:
```ABAP
	SELECT * FROM sflight INTO TABLE @DATA(t_flights).

	cl_salv_table=>factory( importing r_salv_table = DATA(gr_alv)
	                        changing t_table = t_flights ).
	gr_alv->display(  ).
```

## Obsolete Working Memory Syntax

Any time you see a syntax error suggesting that you need to add `WITH HEADER LINE` to your table definition, it typically means that you’re using an outdated ABAP keyword and should use something different and more modern.

An alternative to specifying a structure as a new table is to use the keyword `OCCURS` followed by a number. This specifies the size of the internal table that is created. Note that this is completely unnecessary for modern ABAP programs since we can use the `TYPE STANDARD TABLE` keyword instead.

Using square brackets when accessing a table is completely unnec- essary as long as you are avoiding the dreaded short form ABAP. The square brackets are meant to indicate that you’re trying to access the table data and not the table header.

Short form access is used with the `READ TABLE` and `LOOP AT` commands to read or loop at the table in to a structure of the same name instead of saving the data in another structure or field symbol. This can make the code very hard to read, because there is no difference between the table name and structure.

## Database Access Authorization Checks

Authorization check is implemented in code as follows:
```ABAP
	AUTHORITY CHECK
		OBJECT 'S_CARRID'
			ID 'CARRID' FIELD iv_carrid
			ID 'ACTVT'  FIELD '03'.		" data is checked in the master record for current user

	IF sy-subrc = 0.
		SELECT ...
	ELSE.
		< reaction to missing authorization >
	ENDIF.
```

For the authorization check in the program, you specify the authorization that is to be checked in the master record of the current user. You specify the authorization by specifying the authorization object, its fields, and the appropriate field values. Refer to the syntax shown in the figure.

In this example, the user authorization for access to the `S_CARRID` object is checked; field `CARRID` (airline) contains the airline entered by the user, and field `ACTVT` (activity) contains the value `’03’` (display).

After the `AUTHORITY-CHECK` statement, check return code `SY-SUBRC` and implement the further processing of your program accordingly.

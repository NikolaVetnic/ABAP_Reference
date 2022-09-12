## Tables

Notes:
* a working memory table - an internal table
* you must always prefix your appending components with ZZ so that it doesn’t conflict with future SAP field names; yes, that’s right: two Zs, not just one like in append structure ID and program names

### Standard Table

Requires no key.

The most basic table type (akin to transparent table), typically data is selected from a database and stored in a standard table. 

Anytime you store data from a SELECT statement in a table created using inline data declarations, you store the data in a standard table:
```
	SELECT *
	INTO TABLE @DATA(results)
	FROM sflight.
```

Creating a standard table containing the same structure of the `sflight` table:
```
	DATA: 
		t_sflight TYPE STANDARD TABLE OF sflight.
```

When using a structure based on a transparent table, that table is not created with any keys defined. Defining a key is optional, but a key is required to complete some of the commands that we will explore later in this section. A standard table can have a nonunique primary key added (no exception is thrown in case of inserting two rows with the same key). A key is defined by adding the KEY keyword followed by the fields that make up the key:
```
	DATA: 
		t_flight TYPE STANDARD TABLE OF sflight
		WITH KEY carrid connid fldate.
```

If you created a table using the LIKE keyword, you will not be able to define keys for that table. However, any keys defined in the variable being copied, would also exist in the variable defined using `LIKE` as shown below:
```
	DATA:
		t_flight TYPE STANDARD TABLE OF sflight
		WITH KEY carrid connid fldate,
	
		t_flight_copy LIKE t_flight.
```

It is also possible to create tables of custom data structures without the use of a table type using `TYPE STANDARD TABLE OF` which can then be used to store results from a `SELECT` statement:
```
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

#### `READ TABLE`

It is possible to read a single record from a standard table using the `READ TABLE` keyword, thus reading either `INTO` a var or `ASSIGNING` a field symbol:
```
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
```
	SELECT *
	FROM sflight
	INTO TABLE @DATA(t_flights).

	READ TABLE t_flights INTO DATA(s_flight) INDEX 1.
	READ TABLE t_flights ASSIGNING FIELD-SYMBOL(<s_flight>) INDEX 1.
```

When you use the `INTO` keyword with a variable, a copy of the record is made and stored in the variable. This means that changes to that variable will not affect the corresponding row in the table. Because you aren’t copying any data into memory, using the field symbol will lead to better.

#### `LOOP AT`

Just as `READ TABLE` is used to look at a single record from within a standard table, `LOOP AT` is used to cycle through all of the records within a standard table and insert them into a variable of field symbol:
```
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

Inline data declarations are again a possibility:
```
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

#### `INSERT` and `MODIFY`

When inserting it is necessary to use a structure of the same type as table:
```
	DATA:
	    s_flight_row TYPE sflight,
	    t_flights TYPE STANDARD TABLE OF sflight.

	s_flight_row-carrid = 'NV'.
	s_flight_row-connid = '017'.

	INSERT s_flight_row INTO TABLE t_flights.
```


If there are two tables of the same structure, there’s a quick way to combine them without having to iterate through each record:
```
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
```
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

#### `DELETE`

If you already have the row that you’re using in a local data structure, you can use that structure to indicate the row that needs to be deleted. The `DELETE TABLE` command uses the primary key of the structure to find and delete the corresponding row from the standard table. If no primary key is defined when the standard table is defined, then the key is made up of the entire row, meaning the structure must match an entire row of the table:
```
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
```
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
```
	DELETE TABLE t_flights WITH TABLE KEY 
		carrid = 'AA' connid = 17 fldate = '20150107'.
```

You can also delete rows in a standard table based on a WHERE clause:
```
	DELETE t_flights WHERE carrid = ‘AA’ AND connid = 17.
```

### Sorted Table

Requires either unique or non-unique keys.

A sorted table can be searched through at a quicker rate than a standard table, but will be slightly slower when inserting records than a standard table.

When defining a sorted table, using a unique key is will cause an exception to occur if a record is inserted with that uses an existing key:
```
	DATA:
	    t_sorted_flights1 TYPE SORTED TABLE OF sflight
	        WITH UNIQUE KEY carrid connid fldate,
	        WITH NON-UNIQUE KEY carrid connid.
```

We can also sort standard tables using the `SORT` keyword (can’t sort a table of the sorted type):
```
	DATA:
	    t_flights TYPE STANDARD TABLE OF sflight.
    
	SELECT *
	FROM sflight
	INTO TABLE t_flights.
	
	SORT t_flights BY carrid connid ASCENDING fldate DESCENDING.
```

An insert using a key example (using an index could cause sort order exception):
```
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
```
	MODIFY TABLE t_flights FROM s_flight. " uses primary key
```

There are no additional rules for deleting rows in a sorted table,
because deleting rows will preserve the sorted order.

#### `BINARY SEARCH`

`BINARY SEARCH` searches through the (sorted) table much quicker than `READ TABLE` or `LOOP AT`:
```
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

#### `DELETE ADJACENT DUPLICATES FROM`

Delete records with the same primary key value that are adjacent to each other (if no key is defined, the key will be the entire record):
```
	DELETE ADJACENT DUPLICATES FROM t_flights.
```

You can use the COMPARING command to define the specific fields that you want to use to find duplicates, or COMPARING ALL FIELDS to delete only records that completely match:
```
	DELETE ADJACENT DUPLICATES FROM t_flights COMPARING ALL FIELDS.
	...
	DELETE ADJACTENT DUPLICATES FROM t_flights COMPARING connid fldate.
```

### Hashed Table

Requires unique keys.

The actual table contents are stored unsorted in memory, and a hash board is created that contains the unique keys for the data - the table can thus be sorted in any order without violating the hash board and unique key:
```
	DATA:
	    t_flights TYPE HASHED TABLE OF sflight
	        WITH UNIQUE KEY carrid connid fldate.
	
	SELECT *
	INTO TABLE t_flights
	FROM sflight.
	
	SORT t_flights BY price.
```

Hashed tables can be read at a rate much faster than standard or sorted tables (requires the entire key):
```
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

#### Inserting, Changing and Deleting Hashed Table Rows

The only way to `INSERT` or `MODIFY` a row is by utilizing the table key:
```
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

### Performance

Summary of the performance diferences among the different table types:
|Table Type|Standard|Sorted|Hash|
|--|--|--|--|
|`INSERT`|Fastest|Middle|Slowest|
|`READ`|Slowest|Middle (with `BINARY SEARCH`)|Fastest (using table key)|
|`UPDATE`|Same|Same|Same|
|`DELETE`|Same|Same|Same|

### Copying Table Data

Using the assignment operator `=`:
```
	DATA:
	    t_table1 TYPE TABLE OF sflight,
	    t_table2 TYPE TABLE OF sflight.

	SELECT *
	INTO TABLE t_table1
	FROM sflight.

	t_table2 = t_table1.
```

The following snippet copies all of the records in table `t_sflight` into table `t_flight_price`, even though table `t_flight_price` contains only a subset of the fields in `t_sflight`:
```
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

### Displaying Data from Working Memory

A common ABAP program is a report that will pull a selection of data from the database and display the results to the user. We can display the results to the user using an ALV grid:
```
	DATA:
	    t_flights TYPE STANDARD TABLE OF sflight,
	    gr_alv TYPE REF TO cl_salv_table.

	SELECT * INTO TABLE t_flights FROM sflight.

	cl_salv_table=>factory( importing r_salv_table = gr_alv
	                        changing t_table = t_flights ).
	gr_alv->display( ).
```

With inline data declarations:
```
	SELECT * FROM sflight INTO TABLE @DATA(t_flights).

	cl_salv_table=>factory( importing r_salv_table = DATA(gr_alv)
	                        changing t_table = t_flights ).
	gr_alv->display(  ).
```

### Obsolete Working Memory Syntax

Any time you see a syntax error suggesting that you need to add `WITH HEADER LINE` to your table definition, it typically means that you’re using an outdated ABAP keyword and should use something different and more modern.

An alternative to specifying a structure as a new table is to use the keyword `OCCURS` followed by a number. This specifies the size of the internal table that is created. Note that this is completely unnecessary for modern ABAP programs since we can use the `TYPE STANDARD TABLE` keyword instead.

Using square brackets when accessing a table is completely unnec- essary as long as you are avoiding the dreaded short form ABAP. The square brackets are meant to indicate that you’re trying to access the table data and not the table header.

Short form access is used with the `READ TABLE` and `LOOP AT` commands to read or loop at the table in to a structure of the same name instead of saving the data in another structure or field symbol. This can make the code very hard to read, because there is no difference between the table name and structure. 

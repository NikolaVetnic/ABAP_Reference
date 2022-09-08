## Tables

Notes:
* a working memory table - an internal table
* you must always prefix your appending components with ZZ so that it doesn’t conflict with future SAP field names; yes, that’s right: two Zs, not just one like in append structure ID and program names

### Standard Table

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

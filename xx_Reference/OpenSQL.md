## Open SQL

### `SELECT` Multiple Rows

Old OpenSQL syntax (selects `*` from `s_flights` and places it into `t_results` internal table), to be run from console:
```
	SELECT *
	INTO TABLE t_results
	FROM s_flights
```

`SELECT` using inline data declarations, to be run from console:
```
	SELECT fldate, price
	INTO TABLE @DATA(t_results)
	FROM sflight
```

### `SELECT SINGLE`

Old OpenSQL syntax (**no spaces after & before parenthesis**), console:
```
	SELECT SINGLE fldate
	INTO ld_date
	FROM sflight

	SELECT SINGLE fldate, carrid, connid
	INTO (ld_date, ld_carrid, ld_connid)
	FROM sflight
```

New OpenSQL syntax, console:
```
	SELECT SINGLE fldate
	INTO @DATA(ld_inline_date)
	FROM sflight

	SELECT SINGLE fldate, carrid, connid
	FROM sflight
	INTO (@DATA(ld_date), @DATA(ld_carrid), @DATA(ld_connid))
```

Use `SELECT SINGLE *` to select every field from a single row.

### `SELECT... WHERE` Queries

OpenSQL query example, console:
```
	SELECT fldate, carrid, connid
	INTO TABLE lt_flights        " old OpenSQL
	INTO TABLE @DATA(lt_flights) " new OpenSQL
	FROM sflight
	WHERE carrid = 'AA' AND planetype = '747-400'
```

### `INSERT` Queries

Inserting a single row, ABAP code snippet:
```
	DATA:
	    d_booking type znv_book.

	d_booking-bookid = 2.
	d_booking-customid = 1.

	*INSERT znv_book FROM d_booking. " old OpenSQL
	INSERT znv_book FROM @d_booking. " new OpenSQL
```

We do not define the MANDT key value, the system will actually take care of that for us. The system will always use our logged in client for any `SELECTS`, `UPDATES`, etc. on the database.

Inserting an entire internal table, ABAP code snippet:
```
	DATA:
	    d_booking TYPE znv_book,
    	t_booking TYPE STANDARD TABLE OF znv_book.

	d_booking-bookid = 6.
	d_booking-customid = 7.
	APPEND d_booking TO t_booking.

	d_booking-bookid = 7.
	d_booking-customid = 8.
	APPEND d_booking TO t_booking.

	*INSERT znv_book FROM TABLE t_booking. " old OpenSQL
	INSERT znv_book FROM TABLE @t_booking. " new OpenSQL
```

### `MODIFY` and `UPDATE` Queries

`MODIFY` and `UPDATE` are essentially the same, except `MODIFY` will add a row if none is present. Abap code snippet:
```
	DATA:
    	s_booking TYPE znv_book.

	s_booking-bookid = 1.
	s_booking-customid = 13.

	*MODIFY znv_book FROM s_booking. " old OpenSQL
	MODIFY znv_book FROM @s_booking. " new OpenSQL
```

`UPDATE` with `SET` and `WHERE` to update fields for a selection of records, ABAP code snippet>
```
	UPDATE sflight
	SET seatsmax = 400, planetype = '747'
	WHERE planetype = '747-400'.
```

### `DELETE` Queries

ABAP code snippet:
```
	DATA:
	    s_booking TYPE znv_book.

	s_booking-bookid = 7.
	DELETE znv_book FROM s_booking.
```

Alternative (using `WHERE`):
```
	DELETE FROM znv_book
	WHERE bookid = 6.
```

### `JOIN` Queries

ABAP code snippet:
```
	DATA:
	    d_date TYPE s_date,
	    d_time TYPE s_dep_time.

	SELECT SINGLE fldate deptime  " old OpenSQL
	INTO (d_date, d_time)		  " old OpenSQL
	SELECT SINGLE fldate, deptime " new OpenSQL
	INTO (@d_date, @d_time)		  " new OpenSQL
	FROM sflight
	INNER JOIN spfli
	ON sflight~carrid = spfli~carrid 
	AND sflight~connid = spfli~connid.

	WRITE: d_date, ' - ', d_time.
```

The `INNER JOIN` matches the rows of each table based on the keys listed in the `ON` command to create a new table (which is the two tables joined together), and then the fields selected are returned.

If the field selected exists in both tables, such as the CARRID field in the preceding example, then the table that you want to select the field from must be indicated by prefixing the field with the table name and a tilde:
```
	DATA:
		d_carrid TYPE s_carr_id,
		d_date   TYPE s_date,
		d_time   TYPE s_dep_time.

	SELECT sflight~carrid fldate deptime
	INTO ( d_carrid, d_date, d_time )
	FROM sflight
	INNER JOIN spfli
	ON sflight~carrid = spfli~carrid
	AND sflight~connid = spfli~connid.
	ENDSELECT.

	WRITE: d_carrid, ' - ', d_date, ' - ', d_time.

	* using aliases
	...
	FROM sflight AS f
	INNER JOIN spfli AS s
	on f~carrid = s~carrid
	AND f~connid = s~connid.
	...
```

In an `INNER JOIN`, a record must exist in all tables included in the join, but with a `LEFT OUTER JOIN`, the record will still be displayed even if the table you’re joining doesn’t have a corresponding record. The syntax for a `LEFT OUTER JOIN` is the same as the `INNER JOIN`; only the keyword name has changed:
```
	DATA:
	    d_date TYPE s_date,
	    d_time TYPE s_dep_time.

	SELECT SINGLE fldate deptime
	INTO ( d_date, d_time )
	FROM sflight AS f
	LEFT OUTER JOIN spfli AS s
	ON f~carrid = s~carrid
	AND f~connid = s~connid.

	WRITE: d_date, ' - ', d_time.
```

With old Open SQL You cannot use a `WHERE` clause or an additional `INNER JOIN` using fields in a table that is included in a `LEFT OUTER JOIN`, but you can include these fields in a select using the new Open SQL.

### `FOR ALL ENTRIES` Queries

ABAP code snippet:
```
	DATA:
		t_book TYPE STANDARD TABLE OF znv_book,
		t_custom TYPE STANDARD TABLE OF scustom.

	SELECT *				" old OpenSQL
	INTO TABLE t_book
	FROM znv_book.

	SELECT *
	INTO TABLE t_custom		" it appears this table lacks customid field
	FROM scustom
	FOR ALL ENTRIES IN t_book
	WHERE scustom~customid = t_book-customid.
```

### With `SELECT` Options

ABAP code snippet:
```
	DATA:
	    d_carrid TYPE s_carr_id,
	    t_sflight TYPE STANDARD TABLE OF sflight.

	SELECTION-SCREEN BEGIN OF BLOCK selection.
	    SELECT-OPTIONS: so_carr for d_carrid.
	SELECTION-SCREEN END OF BLOCK selection.

	SELECT *
	INTO TABLE t_sflight
	FROM sflight
	WHERE carrid IN so_carr.
```

The selection options are actually an internal table of ranges.

### Changes in New OpenSQL

Changes:
* columns are separated with a `,` (a comma)
* any ABAP variables with a `@` symbol
* possible to specify all of the colums with `table_name~*` syntax
* when using inline data declarations, we can set the name of the different structure components using the `AS` command when selecting the database columns; if we do not use this command, the names will match the name of the column which we are selecting:
```
	SELECT SINGLE fldate AS flight_date, carrid
	FROM sflight
	INTO @DATA(s_flight).
	DATA(d_date) = s_flight-flight_date.
```
* `INTO` and `INTO TABLE` can now be located after a `WHERE` clause at the end of a statement

### Short Form OpenSQL

ABAP code snippet:
```
	TABLES: znv_book.
	SELECT * FROM znv_book.
	   WRITE: znv_book-bookid.
	ENDSELECT.
```

### Table Locks

I skipped over this section but am noting here that is available in the [book](https://www.amazon.com/ABAP-Introduction-Beginners-Guide-PRESS/dp/1493212427), if need for that information arises at later point.

## Dates, Times, Quantities and Currencies

### Dates

#### Basics

A date is a character string of eight characters, meaning that you can apply some of the string concepts from the previous chapter, such as using sub-string, to get specific year, month, and day values:
```
	DATA: my_date  TYPE d VALUE ‘20190102’,
	      year  TYPE i,
		  month TYPE i,
	      day   TYPE i.
	year = substring( val = my_date off = 0 len = 4 ).
	my_month = substring( val = my_date off = 4 len = 2 ).
	my_day = substring( val = my_date off = 6 len = 2 ).
```

Adding days to a date data type can be done by adding an integer to the date (ABAP systems can easily handle leap years when calculating dates):
```
	DATA: my_date TYPE d VALUE ‘20190102’.
	my_date = my_date + 35. " result is Feb 6 2019

	DATA: my_date TYPE d VALUE ‘20190206’.
	my_date = my_date - 35. " result is Jan 2 2019
```

If you want to add a month to your date, you can call the `RP_CALC_DATE_IN_INTERNAL` function module:
```
	DATA: my_date TYPE d VALUE ‘20190101’.
	
	CALL FUNCTION ‘RP_CALC_DATE_IN_INTERNAL’
	    EXPORTING
			date = my_date 
			days = 0 
			months = 1
			years = 0
	    IMPORTING
	        calc_date = my_date.
```

Dates also can be compared using the regular comparison operators, such as `<` or `>=`.

There is an interesting approach that exploits the fact that when a variable of type `d` contains an invalid date and is assigned to a variable of type `i` (integer), the value of the integer variable becomes `0` (this is because integer presentation of a date is the number of days since the beginning of SAP time, and for an invalid date the number of days cannot be calculated):
```
	DATA:
	  my_int  TYPE i,
	  my_date TYPE d.

	my_date = '20190230'.
	my_int = my_date.

	IF my_int = 0.
	  WRITE: 'Date is invalid.', /.
	ELSE.
	  WRITE: 'Date is valid.', /.
	ENDIF.
```

#### Factory Calendars

Use function module `DATE_CONVERT_TO_FACTORYDATE` to convert a regular date into a factory date. The factory date will be a number representing the number of working days since the factory calendar began:
```
	PARAMETERS:
	    p_date TYPE d OBLIGATORY.
	DATA:
	  simple_date  TYPE d,
	  factory_date TYPE scal-facdate.

	simple_date = p_date.
	WRITE: / 'Date entered : ', p_date.

	CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
	  EXPORTING
	    date                         = simple_date
	    factory_calendar_id          = 'US'
	  IMPORTING
	    factorydate                  = factory_date
	  EXCEPTIONS
	    calendar_buffer_not_loadable = 1
	    correction_option_invalid    = 2
	    date_after_range             = 3
	    date_before_range            = 4
	    date_invalid                 = 5
	    factory_calendar_not_found   = 6
	    OTHERS                       = 7.

	IF sy-subrc <> 0.
	  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
	  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
	ENDIF.

	ADD 10 TO factory_date.

	CALL FUNCTION 'FACTORYDATE_CONVERT_TO_DATE'
	  EXPORTING
	    factorydate                  = factory_date
	    factory_calendar_id          = 'US'
	  IMPORTING
	    date                         = simple_date
	  EXCEPTIONS
	    calendar_buffer_not_loadable = 1
	    correction_option_invalid    = 2
	    date_after_range             = 3
	    date_before_range            = 4
	    date_invalid                 = 5
	    factory_calendar_not_found   = 6
	    OTHERS                       = 7.

	IF sy-subrc <> 0.
	  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
	  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
	ENDIF.

	WRITE: / 'Result : ', simple_date.
```

#### Datum

You can use the datum date type, which will be displayed per the style of the user’s date preference:
```
	DATA: my_datum TYPE datum VALUE '20190801'.
	WRITE: 'Type DATUM:', my_datum. " prints '08/01/2019'

	DATA: my_date TYPE d VALUE '20190801'. " prints '08012019'
	WRITE: /'Type D:',  my_date.
```

#### System Date Fields

There are three system date fields (all return values related to the current date):
* `sy-datum` - returns the current system date (the system date is dependent on the system’s current time zone)
* `sy-datlo` - the date for the logged in user’s time zone
* `sy-fdayw` - factory day of week as an integer, meaning `0` for Sunday and `6` for Saturday

#### Date-Limited Records

A date-limited record is a record in a database with valid_from and/or valid_ to date fields indicating the dates between which the record is considered valid.

To select only the records that have a limited validity and are valid for today, you need to select records with a valid from date less than or equal to today and a valid to date greater than or equal to today:
```
	“New Open SQL
	SELECT * FROM USR02 INTO TABLE @DATA(results)
	WHERE GLTGV <= @sy-datum AND GLTGB >= @sy-datum.
```

### Times

#### Calculating Time

Just as with dates, you can compare time using the regular comparison operators and calculate time using regular math expressions:
```
	DATA: one_time     TYPE t,
	      another_time TYPE t.

	one_time = sy-uzeit.
	WRITE: / 'Current time:', one_time. " prints '211239'

	another_time = one_time + 5.
	WRITE: / 'Current time + 5:', another_time. " prints '211244'

	IF one_time = another_time.
	  WRITE: / 'Time is the same'.
	ELSE.
	  WRITE: / 'Time is different'.
	ENDIF.
```

#### Timestamps

A timestamp is a combination of date and time for the Coordinated Universal Time (UTC) time zone. This can be used to record the exact date and time when an action was completed by a user without having to rely on the local time zone for the user or the server. Types:
* `timestamp` - format `YYYYmmDDHHMM`,
* `timestampl` - format `YYYYmmDDHHMMSS.sssssss` (up to seven decimal places for seconds, allows for nanoseconds)

Example:
```
	DATA: my_timestamp  TYPE timestamp,
	      my_timestampl TYPE timestampl.
	GET TIME STAMP FIELD my_timestamp.
	GET TIME STAMP FIELD my_timestampl.
	GET TIME STAMP FIELD data(my_inline_timestamp).
```

The regular comparison operators can only be used between two timestamps, not between a timestamp and a date or time. You can also convert a timestamp to a local date and time variable using the `CONVERT TIME STAMP` function. When using this function, you must specify a time zone that exists in table `TTZZ`:
```
	GET TIME STAMP FIELD DATA(my_utc).
	CONVERT TIME STAMP my_UTC TIME ZONE 'PST'
	INTO DATE DATA(my_pst_date)
	     TIME DATA(my_pst_time)
	     DAYLIGHT SAVING TIME DATA(my_dst).
```

You can also perform a reverse operation and convert a local date and time into a timestamp using the `CONVERT INTO TIMESTAMP` function. A `DATE` and `TIME ZONE` are both required when using this function, but the `TIME` and `DAYLIGHT SAVING TIME` variables are optional:
```
	CONVERT DATE my_pst_date
	TIME my_pst_time
	DAYLIGHT SAVING TIME my_dst
	INTO TIME STAMP my_utc TIME ZONE ‘PST’.
```

When outputting timestamps to a string template, there are special formatting options.
|Timestamp Format|Output|
|--|--|
|`SPACE`|Format: `YYYY-mm-DD HH:MM:SS.zzzzzzz`|
|`ISO`|Format: `YYYY-mm-DDTHH:MM:SS.zzzzzzz` (uses `T` to separate date and time)|
|`USER`|Based on the user's preferences (**System > User Profile > Own Data**) under the **Defaults** tab, example: `01/01/2019 12:00:00 PM`|
|`ENVIRONMENT`|Based on the user's currently selected language environment, displays default for that country regardless of user defaults, example: `01/01/2019 12:00:00 PM`|

Formatting example:
```
	DATA:  unformatted TYPE timestampl
	  VALUE '20190101120000.0000000'.
	DATA(my_string) = |{  unformatted TIMESTAMP = ISO }|.
```

#### `SY-UZEIT` (System Time versus Local Time)

There are times based on the system time zone and times based on the user’s time zone. The system time can be found with the `sy-uzeit` system variable, whereas the user’s local time can be found with `sy-timlo`:
```
	DATA: sys_time TYPE sy-uzeit,
	      sys_timezone TYPE sy-tzone,
	      loc_time TYPE sy-timlo,
	      loc_timezone TYPE sy-zonlo.
	sys_time = sy-uzeit.
	sys_timezone = sy-tzone.
	loc_time = sy-timlo.
	loc_timezone = sy-zonlo.
```

### Quantities

#### Quantity Fields in Data Dictionary

When storing quantities in transparent tables, you need to store the quantity as a `QUAN` data type and the unit of measure as a `UNIT` data type.

#### Converting Quantities

Example:
```
	DATA:
	    my_quantity TYPE i VALUE 5.
	
	CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
	  EXPORTING
	    input                = my_quantity
	    unit_in              = 'M'
	    unit_out             = 'CM'
	  IMPORTING
	    output               = my_quantity
	  EXCEPTIONS
	    conversion_not_found = 1
	    division_by_zero     = 2
	    input_invalid        = 3
	    overflow             = 4
	    type_invalid         = 5
	    units_missing        = 7
	    unit_in_not_found    = 8
	    unit_out_not_found   = 9
	    OTHERS               = 10.

	IF sy-subrc <> 0.
	  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
	  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
	ENDIF.

	WRITE: my_quantity.
```

### Currencies

Currencies work a lot like quantities; they’re used to capture both a value (amount) and a type (currency code).

# System Fields

Some useful system fields:
|System Field|Meaning|
|--|--|
|`sy-mandt`|Logon Client|
|`sy-uname`|Logon Name of the User|
|`sy-langu`|Logon Language of the User|
|`sy-datum`|Local Date of the ABAP System|
|`sy-uzeit`|Local Time of the ABAP System|
|`sy-tcode`|Current Transaction Code|
|`sy-repid`|Name of the Current ABAP Program|
|`sy-index`|Loop Counter at `DO` and `WHILE` Loops|

In the ABAP source code, you can use several data objects without explicitly declaring them previously (for example, `sy-datum` and `sy-index`). The runtime system uses these system fields to provide the application program with information about the actual system status. The table shows several system fields.

You can find a complete list of system fields in the keyword documentation under the term System Fields.

To access system fields in your programs, use read-only access. Write access can result in the loss of important information for program parts that require this information. In addition, the runtime system might change the field content again. Therefore, SAP recommends to only read these fields:
```ABAP
	REPORT ...
	
	PARAMETERS pa_carr TYPE scarr-carrid.	
	DATA gs_scarr TYPE scarr.

	SELECT SINGLE * FROM scarr INTO gs_scarr WHERE carrid = pa_carr.
	
	IF sy-subrc = 0.
		NEW-LINE.
		WRITE: gs_scarr-carrid, gs_scarr-carrname, gs_scarr-url.
	ELSE.
		WRITE 'Sorry, no data found!'.
	ENDIF.
```

One of the most important system fields is the `sy-subrc` field. With many statements, it is supplied by the ABAP runtime system with the corresponding return code to indicate whether the statement could be executed successfully. The value zero means that the statement was executed successfully. Read the keyword documentation for the respective statements to find out if and how this return value is set in individual cases.

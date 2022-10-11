## Flow Control

Example of `IF` statement (note the **whitespaces** in the condition):

```
	IF ( d_test_0 = 1 AND d_test_1 = 2 ).
    	WRITE: 'Value of d_test is 1'.
	ELSE.
	    WRITE: 'Value of d_test something other than 1'.
	ENDIF.
```

Example of `CASE` statement:

```
	CASE d_test_0.
	    WHEN 0 OR 2.
    	    WRITE: 'd_test_0 is now 0 or 2 in CASE statement'.
	    WHEN 1.
    	    WRITE: 'd_test_0 is now 1 in CASE statement'.
	ENDCASE.
```

`DO` loop example:

```
	DO 5 Times.
    	WRITE: 'Running a loop...', /.
	ENDDO.
```

`WHILE` loop example:

```
	WHILE d_loop_counter < 5.
	    WRITE: 'Running a WHILE loop...', /.
	    d_loop_counter = d_loop_counter + 1.
	ENDWHILE.
```

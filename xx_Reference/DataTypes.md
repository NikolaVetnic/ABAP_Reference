## Data Types & Variables

### Numeric Data Types

Basic numeric data types:

-   `b*` - byte, one-byte integer
-   `s*` - short, two-byte integer
-   `i` - integer, four-byte integer
-   `p` - packed number, allows you to define the number of decimal places (up to 14) in the number (if none defined it is treated as an integer), useful for distances and money for example
-   `decfloat16` - decimal floating point with 16 places
-   `decfloat34` - ...34 places
-   `f*` - binary floating point with 17 decimal places (may produce some unpredictable rounding errors and should be avoided if possible)

Declaration:

```
	DATA:
		ld_integer TYPE i VALUE 200,
		ld_packed TYPE p DECIMALS 2 VALUE '3.115'.
```

### Character Data Types

Basic character data types:

-   `c*` - any fixed amount (up to 262,143 - if no `LENGTH` is provided it is treated as a single character) of alphanumeric characters
-   `string` - a variable length of alphanumeric characters
-   `n` - a fixed length of numeric characters (useful for IDs that have leading zeros)
-   `d` - a date in the form `yyyymmdd` (e.g. `20220905`))
-   `t` - a time in the form `hhmmss` (e.g. `120255`)

Declaration:

```
	DATA:
		d_chars TYPE c LENGTH 5 VALUE 'fiver',
		ld_single_char TYPE c VALUE 'A',
		d_string TYPE string VALUE 'Hello World'.
```

### Booleans in ABAP

When working with Booleans, it is best to use the Boolean data objects `abap_true` and `abap_false`:

```
	IF true_variable = abap_true.
	...
	DATA:
		ld_boolean TYPE boolean.
		ld_boolean = abap_true.
```

### Inline Data Declarations

The data type is inferred based on how the variable is used when it is declared:

```
	DATA(d_integer) = 10.
```

Equivalent to:

```
	DATA: d_integer TYPE integer.
	d_integer = 10.
```

# Data Types & Variables

A formal variable description is called a data type. A variable or constant that is defined concretely by data type is called a data object.

## Complete and Incomplete Data Types

**Complete data types** are the built-in ABAP standard data types that already contain a type-specific, fixed-length specification are considered complete data types, such as `d`, `t`, `i`, `int8`, `f`, `string`, `xstring`, `decfloat16`, `decfloat34`.

**Incomplete data types** are the standard types that do not contain a fixed length are considered incomplete data types. When they are used to define data objects, you need to specify the length of the variable. Examples include types such a s `c`, `n`, `x` and `p`.

## Numeric Data Types

Basic numeric data types:

-   `b*` - byte, one-byte integer
-   `s*` - short, two-byte integer
-   `i` - integer, four-byte integer
-   `p` - packed number, allows you to define the number of decimal places (up to 14) in the number (if none defined it is treated as an integer), useful for distances and money for example
-   `decfloat16` - decimal floating point with 16 places
-   `decfloat34` - ...34 places
-   `f*` - binary floating point with 17 decimal places (may produce some unpredictable rounding errors and should be avoided if possible)

Declaration:

```ABAP
	DATA:
		ld_integer TYPE i VALUE 200,
		ld_packed TYPE p DECIMALS 2 VALUE '3.115'.
```

Type `p` (*packed*, packs two digits into each byte) is one of the most important and common ABAP data types. It can be used for business calculations where the result must be accurate. A variable of this type has no decimal places - if those are needed they are to be declared with `DECIMALS` addition.

## Character Data Types

Basic character data types:

-   `c*` - any fixed amount (up to 262,143 - if no `LENGTH` is provided it is treated as a single character) of alphanumeric characters
-   `string` - a variable length of alphanumeric characters
-   `n` - a fixed length of numeric characters (useful for IDs that have leading zeros)
-   `d` - a date in the form `yyyymmdd` (e.g. `20220905`))
-   `t` - a time in the form `hhmmss` (e.g. `120255`)

Declaration:

```ABAP
	DATA:
		d_chars TYPE c LENGTH 5 VALUE 'fiver',
		ld_single_char TYPE c VALUE 'A',
		d_string TYPE string VALUE 'Hello World'.
```

Variables of type `c` or type `string` can both hold character strings. Variables of type `string` can be any length (not necessary to specify length), while those of type `c` have specific length specified with the `LENGTH` keyword.

Type `d` is used to hold dates. In ABAP, a date is always stored internally in the format `YYYYMMDD`, however a user types in the date in a default data format configured for his/her profile and then the system converts it to `YYYYMMDD`.

Variables of type `t` are used to hold times. These are stored internally in the format `HHMMTT` and are always 6 characters in length (and therefore `LENGTH` is not specified).

## Booleans in ABAP

When working with Booleans, it is best to use the Boolean data objects `abap_true` and `abap_false`:

```ABAP
	IF true_variable = abap_true.
	...
	DATA:
		ld_boolean TYPE boolean.
		ld_boolean = abap_true.
```

## Inline Data Declarations

The data type is inferred based on how the variable is used when it is declared:

```ABAP
	DATA(d_integer) = 10.
```

Equivalent to:

```ABAP
	DATA: d_integer TYPE integer.
	d_integer = 10.
```

### Literals

Literals are strings of characters without a name. Their values cannot be changed, since they are essentially hard-coded values. Numeric literals consist of continuous sequences of numbers, and text literals are character strings.

### Constants

We recommend that you avoid using literals to specify values in your source code. Instead, define constants with those values and use the constants in place of literals:
```ABAP
	CONSTANTS:
		c_hello  TYPE string VALUE 'Hello World',
		c_number TYPE i VALUE 123.
```

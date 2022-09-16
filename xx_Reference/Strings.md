## Strings

### String templates

A string literal uses the single quote character (`'`) to mark the beginning and end of a string. A string template is created by enclosing text in pipe characters (`|`). String templates really come in handy when you need to combine data from other variables in a string:
```
	DATA: my_number TYPE i VALUE 123,
	      my_string TYPE string.
	my_string = |{ my_number } gadgets ordered|.
```

You can also call a method that returns a numeric or character-based value, or include calculation expressions:
```
	my_string = |{  gadget->get_num() } gadgets ordered|.
	...
	my_string = |{  gadget->get_num() + 5 } gadgets ordered|.
```

If you need to use special characters in a string template, they need to be prefixed with a backslash character (`\`), which includes backslash(`\`), a pipe(`|`), or curly brackets(`{ }`):
```
	my_string = |we can use \|, \{ and \} if prefixed with a \\|.
```

You can also chain — that is, combine — string templates using two ampersands (`&&`). A common use of chaining string templates is to help format text through multiple lines in your IDE:
```
	my_string = | some text| &&
	            | some more text|.
```

Some additional options when including other data types in your string template are called embedded expressions, which can be quite useful. The `WIDTH`, `PAD`, and `ALIGN` expressions can be used together to impact how a string of data is presented when entered into the string template:
```
	my_string = |{ 'left' WIDTH = 10 ALIGN = LEFT PAD = '1' }|.
	WRITE: / my_string.
	my_string = |{ 'center' WIDTH = 10 ALIGN = CENTER PAD = '2' }|.
	WRITE: / my_string.
	my_string = |{ 'right' WIDTH = 10 ALIGN = RIGHT PAD = '3' }|.
	WRITE: / my_string.
```

Output of the above program:
|`Test Program`|
|--|
|`1left11111`|
|`22center22`|
|333333right`|

`CASE` is another embedded expression; it will change a given string’s case to all uppercase or all lowercase:
```
	my_string = |{ 'aBc' CASE = UPPER }|.
	my_string = |{ 'aBc' CASE = LOWER }|.
```

The SIGN embedded expression will allow you to define the format of a plus or minus sign on a given number (the example outputs `+123`):
```
	my_string = |{ 123 SIGN = LEFTPLUS }|.
```

All the possible options you can use with the `SIGN` embedded expression:
|SIGN|Description|Example|
|--|--|--|
|`LEFT`|Minus sign displayed on the left|`123`, `-123`|
|`LEFTPLUS`|Minus sign and plus sign displayed on the left|`+123`, `-123`|
|`LEFTSPACE`|Minus sign or space displayed on the left|`123`, `-123`|
|`RIGHT`|Minus sign displayed on the right|`123`, `-123`|
|`RIGHTPLUS`|Minus sign and plus sign displayed on the right|`+123`, `-123`|
|`RIGHTSPACE`|Minus sign or space displayed on the right|`123`, `-123`|

The `DECIMALS` expression allows you to define the number of decimal places to display in the string template. When dropping decimals, it will round the result:
```
	DATA: 
		my_decimal TYPE decfloat16 VALUE '1.236'.
	my_string = |{ my_decimal DECIMALS = 2 }|.
```

String templates and embedded expressions can be used together with
inline declarations:
```
	DATA(my_string1) = |{ 'left' WIDTH = 10 ALIGN = LEFT PAD = '1' }|.
```

### String Functions

Probably the most used function is `strlen`:
```
	DATA: my_string TYPE string VALUE 'Hello',
	      my_length TYPE i.
	my_length = strlen( my_string ).
```

The condense function will remove the leading, trailing, and any extra (more than one) spaces within a given string:
```
	my_string = condense( val = ' blah blah ' ).
```

The concat_lines_of function allows you to concatenate lines of a table into a single string separated by a specified character or string. A line of a table includes every column in that table:
```
	DATA: string_table TYPE TABLE OF string,
	      my_string TYPE string.
	
	APPEND 'Thing One' TO string_table.
	APPEND 'Thing Two' TO string_table.
	
	my_string = concat_lines_of( table = string_table sep = ` ` ).
```

There are a couple of different substring functions, which will return a new string from a piece of an existing string. The most basic substring function has three parameters: `val` for the value of the string; `off` for the offset, indicating where to start pulling out the new string; and `len` for the length of the string to be returned:
```
	my_string = substring( val = 'this is the whole string' off = 8 len = 3 ).
```

Following substring functions are also available:
|Type|Description|
|--|--|
|`substring_from`|Specified substring and all following characters|
|`substring_after`|All characters after specified substring|
|`substring_before`|All characters before specified substring|
|`substring_to`|Specified substring and all characters before|

## Text Symbols

Anytime you are defining text that will be displayed to the user, you should be using text symbols. Text symbols make translating programs easy.

### Creating Text Symbols

When using Eclipse, right-click anywhere in your program and select **Open Others • Text Elements**. This will open the **ABAP Text Elements** screen with the **Text Symbols** tab selected. From there, you can enter an alphanumeric symbol identifier in the first column, followed by the text that you want included in your text symbol. You can then use the text symbol in your code:
```
	DATA: 
		my_string type string.
	my_string = text-001.
	my_string = |symbol: { text-001 }|.
```

Another way to create a text symbol is to declare a string literal using single quotes (`'`) and follow it with parentheses, with the text symbol identifier inside the parentheses (when using this method to create text symbols, it’s important to run the
text symbol comparison, which will resolve any conflicts if a text symbol is defined multiple times or is not listed as a text symbol):
```
	DATA: my_string TYPE string.
	my_string = 'Some Text'(001).
```

## Translating Text Symbols

In Eclipse, right-click anywhere in your program and select **Open Others • Text Elements**. From the Display Text Symbols screen, select **Goto • Translation**. You’ll see the popup from which you can select a language to translate your application into.

## Translating Data in Tables. Translating Messages

See the book for more details.

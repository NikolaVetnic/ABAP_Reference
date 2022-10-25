## Notes

Various notes:

-   ABAP is not a case-sensitive language (hence `snake_case` is common instead of `camelCase`)
-   ABAP Development class is now Package
-   in the case of ABAP development, a project is a link to a SAP instance with a specific client, user, and language; there are two types of projects: **ABAP project** is dedicated to on-premise systems and **ABAP Cloud project** is used with the SAP Cloud Platform ABAP Environment
-   chaining statements:
```ABAP
	WRITE:  'Hello World!',
        	'Here I am!'.
```
-   `d_` preceding variable names denotes a global var, however this is a company convention and thus it may differ in j&s-soft
-   `/` - new line character
-   `sy` - the table containing all the system fields
-   whitespace **IS IMPORTANT** for ABAP compiler, e.g. `(3*3)` is considered bad while `( 3 * 3 )` is considered good practice
-   `CTRL + 7` - comment out a block of code (in Eclipse ADT)
- SAP term for a table is **transparent table** - a transparent table is defined in the data dictionary and stored in your database system
- **input help**: within SAP GUI you can choose `F4` to display possible input values (one can also call up the input help for a field using the button immediately to the right of the selected field)
- extensive information on **subroutines** may be found in **BC400_EN_Col18** book from the Learning Hub

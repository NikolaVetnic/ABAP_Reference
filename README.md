# ABAP Reference

Code snippets and cheat sheets used for learning ABAP programming.

## Notes

Various notes:

-   ABAP is not a case-sensitive language (hence `snake_case` is common instead of `camelCase`)
-   ABAP Development class is now Package
-   in the case of ABAP development, a project is a link to a SAP instance with a specific client, user, and language; there are two types of projects: **ABAP project** is dedicated to on-premise systems and **ABAP Cloud project** is used with the SAP Cloud Platform ABAP Environment
-   chaining statements:

```
	WRITE:  'Hello World!',
        	'Here I am!'.
```

-   `d_` preceding variable names denotes a global var, however this is a company convention and thus it may differ in j&s-soft
-   `/` - new line character
-   `sy` - the table containing all the system fields
-   whitespace **IS IMPORTANT** for ABAP compiler, e.g. `(3*3)` is considered bad while `( 3 * 3 )` is considered good practice
-   `CTRL + 7` - comment out a block of code (in Eclipse ADT)
- SAP term for a table is **transparent table** - a transparent table is defined in the data dictionary and stored in your database system

## Topics Covered in More Detail

Contents:

-   [Data Types](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/DataTypes.md)
-   [Operations](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Operations.md)
-   [Flow Controll](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/FlowControll.md)
-   [Screens](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Screens.md)
-   [Program Lifecycle](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/ProgramLifecycle.md)
-   [Tables](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Tables.md)
-   [OpenSQL](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/OpenSQL.md)


## Bibliography

[O'Neill Brian. Getting Started with ABAP, 1st Edition. SAP PRESS, 2015](https://drive.google.com/file/d/1LFz36ssem7Nf19USjY3ylfqtzN-RAPcY/view?usp=sharing)

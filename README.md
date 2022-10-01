# ABAP Reference

Code snippets and cheat sheets used for learning ABAP programming.

## Topics Covered in More Detail

-   [ABAP](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/ABAP.md)
-   [Data Types](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/DataTypes.md)
-   [Text Symbols](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/TextSymbols.md)
-   [Operations](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Operations.md)
-   [Flow Controll](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/FlowControll.md)
-   [Screens](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Screens.md)
-   [Program Lifecycle](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/ProgramLifecycle.md)
-   [Tables](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Tables.md)
-   [OpenSQL](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/OpenSQL.md)
-   [Data Dictionary](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/DataDictionary.md)
-   [Modularization](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Modularization.md)
-   [Error Handling](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/ErrorHandling.md)
-   [ALV](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/ALV.md)
-   [Strings](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Strings.md)
-   [Dates, Times, Quantities and Currencies](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/DatesTimesQuantitiesCurrencies.md)
-   [Working with ABAP Professionally](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/WorkingProfessionally.md)
-   [Often Used Transactions](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Transactions.md)

## Highlighted Examples

- [an example of a basic `COUNTER` class](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/Snippets_BC401_EN_Col18/bc401_01_znvcl_counter) along with a [program that uses it](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/Snippets_BC401_EN_Col18/bc401_02_znv_counter_class_usage)
- [a `COUNTER_COMP` class using two counter objects](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/Snippets_BC401_EN_Col18/bc401_03_znvcl_counter_comp) along with a [program that uses it](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/Snippets_BC401_EN_Col18/bc401_04_znv_counter_comp_usage)
- [object-oriented `EMPLOYEE` table task implementation](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/Snippets_Tasks/C_0001_ZNVCL_TASK1_EMPLOYEE) alongside [program that uses the mentioned `ZNVCL_TASK1_EMPLOYEE` class](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/Snippets_Tasks/0001_ZNV_TASK1_EMPLOYEE_MAN_ADV)

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
- **input help**: within SAP GUI you can choose `F4` to display possible input values (one can also call up the input help for a field using the button immediately to the right of the selected field)

## Bibliography

[O'Neill Brian. Getting Started with ABAP, 1st Edition. SAP PRESS, 2015](https://drive.google.com/file/d/1LFz36ssem7Nf19USjY3ylfqtzN-RAPcY/view?usp=sharing)

[O'Neill Brian. ABAP - An Introduction, 2nd Edition. SAP PRESS, 2019](https://www.amazon.com/ABAP-Introduction-Beginners-Programming-Second/dp/1493218808)

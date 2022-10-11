# Overview of Old and New ABAP Syntax

Original article by Thomas Kruegl is available [here](https://blogs.sap.com/2016/03/02/old-and-new-abap-syntax-overview-sheet/).

|If you see this|Consider using this|
|--|--|
|`DATA` and `FIELD-SYMBOLS` declarations|[Inline declarations](https://blogs.sap.com/?p=85780)|
|`MOVE CORRESPONDING`|[Constructor expression `CORRESPONDING`](https://blogs.sap.com/?p=101549), or [`CL_ABAP_CORRESPONDING`](https://blogs.sap.com/?p=133686)|
|`struc1-field1 = struc2-fielda` `struc1-field2 = struc2-fieldb` `...`|[Constructor expression `CORRESPONDING`](https://blogs.sap.com/?p=101549), or [`CL_ABAP_CORRESPONDING`](CL_ABAP_CORRESPONDING); [Constructor expression `VALUE`](https://blogs.sap.com/?p=85962)|
|`CREATE DATA` / `CREATE OBJECT`|[Constructor expression `NEW`](https://blogs.sap.com/?p=85840)|
|`GET REFERENCE OF`|[Constructor expression `REF`](https://blogs.sap.com/?p=85963)|
|variable only used once, e.g. parameter for a method call|[Constructor expression `VALUE`](https://blogs.sap.com/?p=85962); [Inline declarations](https://blogs.sap.com/?p=85780)
|helper variable for type conversion|[Constructor expression `CONV`](https://blogs.sap.com/?p=85985)|
|INCOMPLETE|TO BE CONTINUED...|

# Hungarian Vs Non-Hungarian Annotations

Hungarian is considered old coding, while Non-Hungarian is considered a new, cleaner approach. Some conventions for Hungarian annotations:
1. first prefix annotates availability or, better yet, existence: `l` - as in local, or `g` as in global
2. second prefix annotates the data type: `v` for variables, `s` for structures, `t` for internal tables, `r` for references

Using the Hungarian convention we get the following combinations: `lt` for local table, `gv` for global variable, `gr` for global reference etc.

Classes should be named thusly (exclusive to **j&s-soft** practices): `ZNV_CL_<CLASS-NAME>`

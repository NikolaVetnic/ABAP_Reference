# Overview of Old and New ABAP Syntax

|If you see this|Consider using this|
|--|--|
|`DATA` and `FIELD-SYMBOLS` declarations|[Inline declarations](https://blogs.sap.com/?p=85780)|
|`MOVE CORRESPONDING`|[Constructor expression `CORRESPONDING`](https://blogs.sap.com/?p=101549), or [`CL_ABAP_CORRESPONDING`](https://blogs.sap.com/?p=133686)|
|`struc1-field1 = struc2-fielda` `struc1-field2 = struc2-fieldb` `...`|Constructor expression `CORRESPONDING`, or `CL_ABAP_CORRESPONDING`; Constructor expression `VALUE`|
|`CREATE DATA` / `CREATE OBJECT`|Constructor expression NEW|

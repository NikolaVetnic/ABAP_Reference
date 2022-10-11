# Procedural Solution

General steps:
1. created data elements (with appropriate domains):
	* `ZNV_TASK1_EMPLOYEEID`
	* `ZNV_TASK1_FIRSTNAME`
	* `ZNV_TASK1_LASTNAME`
	* `ZNV_TASK1_DOB`
	* `ZNV_TASK1_ADDRESS`
	* `ZNV_TASK1_GENDER`
	* `ZNV_TASK1_DEPARTMENT`
	* `ZNV_TASK1_SALARY`
2. created table with the fields that use the above stated data elements
3. set `MANDT` as foreign key
4. added field `CURRENCY`, linked `SALARY` field to it via **Currency/Quantity Fields** tab
5. created message class (NB when creating class it is necessary to switch to **Messages** and then select the package in which to save the class, otherwise it won't work) and appropriate messages

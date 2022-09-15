## ALV

### Report Example Using an SALV Table

First, define the class for all the functionality in this report. In this simple example, you need only two public methods: `GET_DATA` and `PRESENT_DATA`. You also need to define internal table flights that will store the business data from database table `SFLIGHT`, and a reference variable for the ALV table object. In addition, define two private methods to add the standard functions to the toolbar and the option to save a layout.

The program body in this example is short. The only parameter is for the number of rows to be selected from the database (you’ll use this value in the `GET_DATA` method). The code in the `START-OF-SELECTION` event instantiates the class `lcl_report` and calls the methods to get the data and present it.

Declarations in old and new syntax:
```
	    " old syntax
	    DATA:
	        my_report TYPE REF TO lcl_report.
	    CREATE OBJECT my_report.

	    " new syntax
	    DATA(my_report) = NEW lcl_report( ).
```

Now create an implementation of the `lcl_report` class, starting with the `GET_DATA` method. In this example, you simply select as many records from database table `SFLIGHT` as were entered by the user in the `p_rows` parameter.

Next, add an implementation of the `PRESENT_DATA` method that calls an SALV class method to create a reference to the table object. This method also calls the local private methods that add some basic ALV functionality. In the program, all the class method code must be located within the `CLASS... ENDCLASS` keywords.

To finalize the implementation, add the last two methods. The `SET_DEFAULT_FUNCTIONS` method makes all the standard ALV functions available on the toolbar and is self-explanatory. The `SET_LAYOUT` method adds the standard layout saving functionality. To do that, the SALV method requires the so-called layout key, which includes the calling program name (available in the `sy-repid` system variable). The `set_save_restrictions` method of the SALV framework allows you to restrict the layout saving to the user’s own layouts or disallow it altogether. In this example, you want to allow saving any layouts and therefore are using the `restrict_none` constant.

The following SAP demo programs can be particularly helpful for learning more about the SALV table functionality:
* `SALV_DEMO_TABLE_LAYOUT` demonstrates how to use the layout service.
* `SALV_DEMO_TABLE_COLUMNS` is a great example of all the possible column settings that change the visual appearance of the columns. Although this program’s output can be painful to look at, it serves well when you need to show users different presentation options while designing an ALV report.
* `SALV_DEMO_TABLE_EVENTS` explains event handling. There are multiple events in SALV (e.g. double-click or hotspot click) for which you can add your own code to provide additional functionality and handle user interactions.

### Report Example Using SALV Tree

The purpose of the exercise in this section is to create a report showing a summary of total bookings by travel agency. The SALV tree functionality will allow users to drill down into the booking details for each agency.

Start by creating an ABAP report and saving it as a local object in the `$TMP` package. For this report, you only need to select a few fields from database table `SBOOK`, so define a structure type containing the needed fields. Then, add a class definition with the same two public methods and the private methods for additional SALV features. The main program body is the same as in the previous example.

The `GET_DATA` method is different. Here you select some fields from table `SBOOK` into the internal table `tree_data`. Then you sort the internal table by the `agencynum` field because this field will form the top level of the tree hierarchy. This particular code using the `SORT` command is rather typical for SAP ERP versions that are using traditional databases. With SAP HANA, it would be more efficient to sort the data by using the `ORDER BY` addition in the `SELECT` command. We’re using `SELECT * INTO CORRESPONDING` in this example as a lazy way to read data without typing in all the field names. This command finds the fields in the database table (or a view) with the names that match the target structure or internal table and then fills in those fields.

Compared to the SALV table example, a major difference in using an SALV tree is that you call the factory method using an empty table. After receiving the object reference from the method, you then build the hierarchy table in a separate method.

In this example, the tree hierarchy will have only two levels. Your business data is located in the `tree_data` internal table. To build the hierarchy table in the format required by the SALV tree framework, you need to add a “parent” node for each unique agency code (`AGENCYNUM` field) and a “child” node for each record in the table. Special methods are provided for this purpose.

To complete the class implementation, let’s add two small methods to set the report headers and to add aggregation of the booking amounts stored in the `LOCCURAM` field.

Although this is an impressive and functional report, there are several issues with the presentation in this first attempt:
* The screen title is not descriptive.
* The hierarchy column includes unnecessary leading zeroes.
* The Agency column appears twice, which is redundant and confusing.

Fortunately, all these items are easy to fix by making just a few changes in
the program. To add a title, add one line to the `SET_HEADERS` method. Then, address the leading zero issue by adding a `SHIFT` command in the `CREATE_NODE_HIERARCHY` method. Finally, to hide the duplicate Agency column, use the `SET_TECHNICAL` method of the SALV column class. “Technical” is a field attribute in SALV, which means that the field serves an internal purpose in the program and must not be visible to users.

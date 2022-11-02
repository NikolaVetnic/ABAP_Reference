# Learning Assessment

## Unit 1

1. Which of the following layers contains the user interface where each user can access a program, enter new data, and receive the results of a work process? **Answer:** presentation server layer.

2. Which of the following describes the concept of a program made up of several units, instead of a single block? **Answer:** modularity.

## Unit 2

1. Which of the following system development objects are included in the Repository? **Answer:** programs, function modules, definitions of database tables.

2. Which of the following ABAP Workbench tools is used to edit the source code? **Answer:** ABAP Editor.

3. Which of the following are package types? **Answer:** development package, main package.

4. When is the transport of development objects for a development request triggered? **Answer:** when a request is released.

5. Whenever you create or change a development object and save it, the system stores two inactive versions in the Repository. **Answer:** false.

6. Which of the following statements is correct about the ABAP programming language? **Answer:** it enables multi-language applications, it enables SQL access, it is typed.

7. ABAP Objects is the object-oriented enhancement of the ABAP programming language. **Answer:** true.

8. How do you include comment lines? **Answer:** begin the line with an asterisk * in the first column.

9. After a project employee completes the required development task, they should do the following... **Answer:** release the task within the Change request.

## Unit 3

1. Which of the following are complete ABAP standard types? **Answer:** `t`, `int8`, `d`.

2. Which of the following is the operator used in an `IF` statement to formulate a negation before logical conditions? **Answer:** `NOT`.

3. Which of the following are required in the syntax of the Message statement? **Answer:** message number, message type and message class.

4. In nested loops, which of the following contains the loop pass number of the loop in which it is located? **Answer:** `sy-index`.

5. Which of the following is the system command you can enter in the command field of a screen to start the debugger? **Answer:** `/h`.

## Unit 4

1. What are the uses of modularization? **Answer:** to provide a better overview of program layout, to encapsulate a function that is required many times within a program for multiple use, to implement the central maintainability of a function within a program, to make a function available across the system.

2. Which of the following is the name of the assignment of actual parameters to formal parameters when calling a subroutine? **Answer:** parameter passing.

3. Which of the following is the name of variables defined in the main program? **Answer:** global data objects.

4. Which of the following elements does the interface of a function module contain? **Answer:** `EXPORT` parameter, `CHANGING` parameter. 

5. Which of the following is the tab page that you can switch to implement the function module, after defining the corresponding interface? **Answer:** source code.

6. After defining the corresponding `IMPORTING` and `EXPORTING` parameters, you can switch to the **Source code** tab page to implement the functions of the function module. **Answer:** true.

7. Data in the SAP system can be accessed by means of a Business Application Programming Interface (BAPI). **Answer:** true.

8. Which of the following can be used as a visibility option for an attribute? **Answer:** `public`, `private`.

9. Static Methods are called using the `CALL CLASS METHOD` statement. **Answer:** false.

10. When you identify a method as a static method, it can be called directly without the need to generate an instance of the class first. **Answer:** true.

11. To create a static method, all you need to do is enter its name in the Constructor list. **Answer:** false.

12. Which of the following special tools maintains global classes? **Answer:** Class Builder.

13. To define an instance method as opposed to a static method in a local class, the `METHODS` statement is used instead of `CLASS-METHODS`. **Answer:** true.

## Unit 5

1. Which of the following statements copies the content of the source structure to the target structure, one component at a time? **Answer:** `MOVE-CORRESPONDING`.

2. Which of the following statements is used for defining local structure types? **Answer:** `TYPES`.

3. Which of the following specifications are required in the definition of an internal table? **Answer:** line type, primary key, table kind.

4. Which of the following is used for adding a row into an internal table? **Answer:** `APPEND`.

## Unit 6

1. Which of the following is the tool that you can use to create a transparent table? **Answer:** ABAP Dictionary.

2. When using the transparent table as a data type, other properties, such as the key definition or the technical properties, are relevant. **Answer:** false.

3. Which of the following are the types of reuse components that encapsulate database access? **Answer:** function modules, Business Application Programming Interfaces (BAPIs), methods of local classes.

4. The `SELECT` clause determines which lines are read into the target structure and processed using the statement block which you specify in the loop body. **Answer:** false.

5. Which of the following additions to a `SELECT` statement would you use to append rows to an internal table instead of overwriting the existing rows? **Answer:** `APPENDING TABLE`.

6. If you select data from client-specific tables without specifying the client, data records from the current and all other clients are read. **Answer:** false.

7. Which of the following will allow the database to search the entire table, or at least a very large part of the table for the required entries? **Answer:** sequential search.

8. Which of the following should you specify when defining a table join? **Answer:** join tables, join conditions, join columns.

9. If an ABAP program reads a buffered table, the database interface tries to get the required data from the SAP table buffer. **Answer:** true.

10. Which of the following statements are contained within Open SQL? **Answer:** `UPDATE`, `INSERT`, `SELECT`.

11. At runtime, which of the following statements can you use to check whether the actual user has the authorization required for executing the function in the user master record? **Answer:** `AUTHORITY-CHECK`.

## Unit 7

1. Which of the following can you use to adapt the default list interface to your own needs? **Answer:** Menu Painter.

2. Which of the following are standard functionality on a selection screen? **Answer:** type check, variants, value entry.

3. Which of the following statements is used for defining an input variable? **Answer:** `PARAMETERS`.

4. Which of the following are the characteristics of an event block? **Answers:** ends by beginning the next processing block, existence not absolutely necessary.

## Unit 8

1. Which of the following can you use to start screen processing? **Answer:** dialog transaction.

2. Which of the following should be assigned to a screen field to make input mandatory? **Answer:** required.

3. Which of the following statements is true regarding the implementation of data transport? **Answer:** the `TABLES` statement serves as the interface for data transport between the program and the screen.

## Unit 9

1. Which of the following is the Container Class that is used to link the EnjoySAP control with the control area on the screen? **Answer:** `CL_GUI_CUSTOM_CONTAINER`.

2. Which of the following are methods of global class `CL_GUI_ALV_GRID`? **Answer:** `CONSTRUCTOR`, `SET_TABLE_FOR_FIRST_DISPLAY`, `REFRESH_TABLE_DISPLAY`.

## Unit 10

1. What starts the screen sequence defined in the window of a Web Dynpro Component? **Answer:** web dynpro application.

2. What can be defined inside of a Web Dynpro component? **Answer:** view, component controller, window.

3. What is used to transport data between the central component controller and the individual view controllers? **Answer:** context mapping.

## Unit 11

1. Which of the following statements about the Code Inspector are true? **Answer:** you can create a check variant to define the details of what to check, standard inspection is carried out when check is performed through context menu from the Object Navigator, you can define check variants, object sets, and inspections using transaction SCI.

2. Which aspects does the Code Inspector take into consideration when examining a program? **Answer:** typical semantic errors (for example, `AUTHORITY-CHECK` statement without subsequent `SY-SUBRC` check), performance (for example, nested `SELECT` statements), security (for example, cross-client data accesses).

## Unit 12

1. Which of the following statements are true? **Answer:** ABAP Development Tools (ADTs) are an alternative to the ABAP Workbench, ADTs enable cross-platform development by integrating ABAP and non-ABAP development tools in a single powerful integrated development environment.

2. Only ABAP development tools should be used for ABAP development. **Answer:** false.

## Unit 13

1. What is a repair of an SAP repository object in a customer system called? **Answer:** modification.

2. Which of the following are the options for adjusting the SAP software to customer requirements? **Answer:** customizing, enhancements, development.

3. Which of the following are enhancement types? **Answer:** menu exit, screen exit.

# In More Detail...

## Unit 1

Presentation Server layer is the highest layer. This layer contains the user interface where each user can access the program, enter new data, and receive the results of a work process. Read more in the section, ABAP System Architecture, in the lesson, Describing the Processing of ABAP Programs, in the course BC400 (Unit 1, Lesson 1) or TAW10 Part I (Unit 7, Lesson 1).

A program is not made up of a single block, but of several units. This is known as modularization. You can use most of these modularization units in more than one program. Therefore, they are often termed as reuse units. Read more in the section, Interplay Between Server Levels and Program Flow, in the lesson, Describing the Processing of ABAP Programs, in the course BC400 (Unit 1, Lesson 1) or TAW10 Part I (Unit 7, Lesson 1).

## Unit 2

The repository consists of all ABAP development objects, such as programs, function modules, and definitions of database tables. It contains objects delivered by SAP as well as those defined by the customer. Note that the terms repository object and development object are often used interchangeably. Read more in the section, ABAP Repository, in the lesson, Introducing the ABAP Workbench, in the course BC400 (Unit 2, Lesson 1) or TAW10 Part I (Unit 8, Lesson 1).

The ABAP Editor provides a range of functions, including editing the source code, syntax checks, and options for the capitalization of ABAP keywords. Read more in the section, ABAP Workbench Tools, in the lesson, Introducing the ABAP Workbench, in the course BC400 (Unit 2, Lesson 1) or TAW10 Part I (Unit 8, Lesson 1).

There are three package types: development package (can contain Repository objects and other packages), main package (can only contain other packages), and structure package (can only contain main packages). Read more in the section, Package Attributes, in the lesson, Organizing ABAP Development Projects, in the course BC400 (Unit 2, Lesson 2) or TAW10 Part I (Unit 8, Lesson 2).

When a developer completes the development project, the developer releases the corresponding task. This release transfers the objects in the task to the transport request. Once all team members release their tasks, the development leader can release the transport request, and the transport is triggered. Read more in the section, Closing of the Project by Releasing the Request (Project Manager), in the lesson, Finalizing ABAP Development Projects, in the course BC400 (Unit 2, Lesson 4) or TAW10 Part I (Unit 8, Lesson 4).

Whenever you create or change a development object and then save it, the system stores only one inactive version in the Repository. You have an active version and an inactive version of the object. When you complete object development, you have to activate the inactive version (editing version) of the object. This version becomes the new active version of the object. Read more in the section, Activation of Programs, in the lesson, Developing ABAP Programs, in the course BC400 (Unit 2, Lesson 3) or TAW10 Part I (Unit 8, Lesson 3).

The features of the ABAP programming language are as follows: It is typed, It enables multi-language application, It enables SQL access, It is enhanced as an object-oriented language, It is platform-independent, It is upward-compatible. Read more in the section, ABAP Programming Language Features, in the lesson, Developing ABAP Programs, in the course BC400 (Unit 2, Lesson 3) or TAW10 Part I (Unit 8, Lesson 3).

ABAP Objects is the object-oriented enhancement of the ABAP programming language. Read more in the lesson, Developing ABAP Programs, Task: ABAP Programming Language Features, in the course BC400 (Unit 2, Lesson 3) or TAW10 Part I (Unit 8, Lesson 3).

Lines that begin with an asterisk (`*`) in the first column are recognized as comment lines by the ABAP runtime system and are ignored. Read more in the lesson, Developing ABAP Programs, Task: General ABAP Syntax II, in the course BC400 (Unit 2, Lesson 3) or TAW10 Part I (Unit 8, Lesson 3).

When a development object is edited or created, the relevant employee assigns this to the change request. The object is entered into the task of the employee. All Repository objects that an employee works on during a development project are collected within the employee’s task. The task will be released by the employee when all assigned work is done. Read more in the lesson, Organizing ABAP Development Projects, section: Transport of Development Objects, in the course BC400 (Unit 2, Lesson 2) or TAW10 Part I (Unit 8, Lesson 2).

## Unit 3

The built-in ABAP standard data types that already contain a type-specific, fixed-length specification are considered complete data types. They are: `D`, `T`, `I`, `INT8`, `F`, `STRING`, `XSTRING`, `DECFLOAT16`, `DECFLOAT34`. Read more in the lesson, Defining Elementary Data Objects, Task: Complete ABAP Standard Data Types, in the course BC400 (Unit 3, Lesson 1) or TAW10 Part I (Unit 9, Lesson 1).

You can formulate negations by placing the `NOT` operator before the logical expression. When negating the `IS INITIAL` query, you can use the special `IS NOT INITIAL` query. Read more in the lesson, Using Basic ABAP Statements, Task: IF Statement, in the course BC400 (Unit 3, Lesson 2) or TAW10 Part I (Unit 9, Lesson 2).

Specify the three-digit message number and the message class when using the `MESSAGE` statement. Use the message type to specify where you want the message to be displayed. Read more in the lesson, Using Basic ABAP Statement, Task: Dialog Messages, in the course BC400 (Unit 3, Lesson 2) or TAW10 Part I (Unit 9, Lesson 2).

In nested loops, sy-index always contains the loop pass number of the loop in which it is located. In the `DO` and `WHILE` loops, the sy-index system field contains the number of the current loop pass. Therefore, you should only query this system field within a loop. Read more in the lesson, Using Basic ABAP Statement, Task: Loop Constructs, in the course BC400 (Unit 3, Lesson 2) or TAW10 Part I (Unit 9, Lesson 2).

To start the debugger from a screen, enter `/h` in the command field, and press Enter. Read more in the lesson, Analyzing Programs with the ABAP Debugger, Task: Debugging Mode at Runtime, in the course BC400 (Unit 3, Lesson 3) or TAW10 Part I (Unit 9, Lesson 3).

## Unit 4

A modularization unit encapsulates a function. You can use most of the modularization units in more than one program. Therefore, they are often termed as reuse units. The improvement in transparency is a result of the program becoming more function-oriented. Modularization makes it easier to maintain programs, because you only need to make changes to the modularization unit, and not at various points in the main program. Read more in the lesson, Explaining Modularization, Task: Modularization Techniques, in the course BC400 (Unit 4, Lesson 1) or TAW10 Part I (Unit 10, Lesson 1).

The assignment of actual parameters to formal parameters when calling a subroutine is called parameter passing. Read more in the lesson, Defining and Calling Subroutines, Task: Parameter Definition for Subroutines, in the course BC400 (Unit 4, Lesson 2) or TAW10 Part I (Unit 10, Lesson 2).

Variables defined in the main program are global data objects. They are visible and can be addressed in the entire main program, as well as in every subroutine called. Read more in the lesson, Defining and Calling Subroutines, Task: Local and Global Data Objects, in the course BC400 (Unit 4, Lesson 2) or TAW10 Part I (Unit 10, Lesson 2).

Each function module has an interface with parameters for importing or exporting data. The interface of the function module consists of the import, export, changing parameters, and exceptions. Read more in the lesson, Calling Function Modules, Task: Examination of a Function Module, in the course BC400 (Unit 4, Lesson 3) or TAW10 Part I (Unit 10, Lesson 3).

After defining the corresponding `IMPORTING` and `EXPORTING` parameters, you can switch to the Source code tab page to implement the functions of the function module. Read more in the lesson, Creating Function Modules, Task: Source Code Editing, in the course BC400 (Unit 4, Lesson 4) or TAW10 Part I (Unit 10, Lesson 4).

After defining the corresponding `IMPORTING` and `EXPORTING` parameters, you can switch to the Source code tab page to implement the functions of the function module. Read more in the lesson, Creating Function Modules, Task: Source Code Editing, in the course BC400 (Unit 4, Lesson 4) or TAW10 Part I (Unit 10, Lesson 4).

The Business Object Repository (BOR) in the SAP system contains business objects types. A business object type is a program that behaves like a class. It represents an SAP table or a table hierarchy. A business object has BAPIs as methods. You can call these BAPIs to access the corresponding tables. A BAPI is a means of accessing data in the SAP system. Read more in the lesson, Describing Business Application Programming Interfaces (BAPIs), Task: Business Application Programming Interface (BAPI), in the course BC400 (Unit 4, Lesson 5) or TAW10 Part I (Unit 10, Lesson 5).

Attributes are normally encapsulated in the class by defining them `PRIVATE`, and can therefore only be read or changed using methods of the same class. Classes allow you to make specific attributes visible to users of the class, if defined `PUBLIC`. Read more in the lesson, Calling Methods of Global Classes Task Example of Access Options for a Global Class, in the course BC400 (Unit 4, Lesson 6) or TAW10 Part I (Unit 10, Lesson 6).

You use the `CALL METHOD` statement to call a method, and you then specify the method. With static methods, this specification comprises the name of the class and the method, separated by the static component selector “=>” (double-headed arrow). Read more in the lesson, Calling Methods of Global Classes, Task: Static Methods, in the course BC400 (Unit 4, Lesson 6) or TAW10 Part I (Unit 10, Lesson 6).

You don't need to generate an instance to test a static method. You can execute the static method immediately. Read more in the lesson, Calling Methods of Global Classes, Task: Global Class Documentation and Testing, in the course BC400 (Unit 4, Lesson 6) or TAW10 Part I (Unit 10, Lesson 6).

To create a static method using the Class Builder, simply enter the method name in the Method list on the Method tab page. Read more in the lesson, Creating Global Classes and Static Methods, Task: Creation of Static Methods, in the course BC400 (Unit 4, Lesson 7) or TAW10 Part I (Unit 10, Lesson 7).

Global classes are maintained with a special tool called the Class Builder. Read more in the lesson, Using Local Classes, Task: Local Classes, in the course BC400 (Unit 4, Lesson 8) or TAW10 Part I (Unit 10, Lesson 8).

To define an instance method in a local class, as opposed to the static method, the `METHODS` statement is used instead of `CLASS-METHODS`. Read more in the lesson, Using Local Classes, Task: Syntax for Static Methods, in the course BC400 (Unit 4, Lesson 8) or TAW10 Part I (Unit 10, Lesson 8).

## Unit 5

The statement, `MOVE-CORRESPONDING`, copies the content of the source structure to the target structure, one component at a time. In this case, only those components that are available under the same name in both the source structure and the target structure are considered. All other components of the structures remain unchanged. Read more in the lesson, Using Structured Data Objects, Task: Use of Structured Data Objects, in the course BC400 (Unit 5, Lesson 1) or TAW10 Part I (Unit 11, Lesson 1).

Use the `TYPES` statement to define local structure types. Read more in the lesson, Using Structured Data Objects Task Definition of Structures with Local Types, in the course BC400 (Unit 5, Lesson 1) or TAW10 Part I (Unit 11, Lesson 1).

The required specifications in the definition of an internal table are: Line type, Primary key, Table kind. Read more in the lesson, Using Internal Tables, Task: Properties of Internal Tables, in the course BC400 (Unit 5, Lesson 2) or TAW10 Part I (Unit 11, Lesson 2).

`APPEND` appends the content of a structure to an internal table, and this operation can only be used with standard tables. Read more in the lesson, Using Internal Tables, Task: Statements for Accessing Single Records of Internal Tables, in the course BC400 (Unit 5, Lesson 2) or TAW10 Part I (Unit 11, Lesson 2).

## Unit 6

The developer can use a data model as the basis for implementing appropriate table definitions (transparent tables), including their relationships with each other in the ABAP Dictionary. Read more in the lesson, Explaining Data Models, Task: Data Model Overview, in the course BC400 (Unit 6, Lesson 1) or TAW10 Part I (Unit 12, Lesson 1).

Transparent tables can be used in the same way as structure types in programming. For example, transparent tables can be used to define a structured data object. Only the list of fields is important. Other properties of the transparent table, such as the key definition or the technical properties, are irrelevant when it is being used as a data type. Read more in the lesson, Explaining Data Models, Task: Structures in the ABAP Dictionary, in the course BC400 (Unit 6, Lesson 1) or TAW10 Part I (Unit 12, Lesson 1).

Before you program direct access to database tables, look for reuse components that handle the read process, such as Logical Databases, Function Modules, BAPIs, Methods of global classes. Read more in the lesson, Retrieving Single Database Records, Task: Types of Reuse Components that Encapsulate Database Access, in the course BC400 (Unit 6, Lesson 2) or TAW10 Part I (Unit 12, Lesson 2).

The `SELECT` clause names the fields of the table that are to be read. Read more in the lesson, Retrieving Single Database Records, Task: SELECT Statement Clauses, in the course BC400 (Unit 6, Lesson 2) or TAW10 Part I (Unit 12, Lesson 2).

With array fetch, any content that might be present in the internal table is overwritten. If you want to append rows instead of overwriting, use the `APPENDING TABLE` addition. Read more in the lesson, Retrieving Multiple Database Records, Task: Data Retrieval Using an Array Fetch, in the course BC400 (Unit 6, Lesson 3) or TAW10 Part I (Unit 12, Lesson 3).

If you select data from a client-specific table without specifying the client, only data records from the current client are read (a restriction to the current client is automatically added to the `WHERE` clause of the `SELECT` statement by the Database interface). Read more in the lesson, Describing Other Aspects of Database, Task: Data Retrieval from Client-Specific Tables, in the course BC400 (Unit 6, Lesson 4) or TAW10 Part I (Unit 12, Lesson 4).

In the worst-case scenario, the entire table, or at least a very large part of the table, must be searched for the required entries. This is referred to as a sequential search, and the wait time for the database access can be very long. Read more in the lesson, Describing Other Aspects of Database Access, Task: Access Through Non-Key Fields, in the course BC400 (Unit 6, Lesson 4) or TAW10 Part I (Unit 12, Lesson 4).

Specify the following details when defining a table join: Join tables, Join conditions, Join columns. Read more in the lesson, Describing Other Aspects of Database Access, Task: Table Join Definition, in the course BC400 (Unit 6, Lesson 4) or TAW10 Part I (Unit 12, Lesson 4).

If an ABAP program reads a buffered table, the database interface first tries to get the required data from the SAP table buffer. If the required data is not yet in the SAP table buffer, the database is accessed. Afterwards the database interface also stores the read data in the SAP table buffer. Read more in the lesson, Describing Other Aspects of Database Access, Task: SAP Table Buffer, in the course BC400 (Unit 6, Lesson 4) or TAW10 Part I (Unit 12, Lesson 4).

In addition to the `SELECT` statement, Open SQL contains the `UPDATE`, `INSERT`, `DELETE`, and `MODIFY` statements. Read more in the lesson, Describing Other Aspects of Database Access, Task: Write Access to the Database, in the course BC400 (Unit 6, Lesson 4) or TAW10 Part I (Unit 12, Lesson 4).

At runtime, use the `AUTHORITY-CHECK` statement to check whether the user has the authorization required to execute the function being called in the user master record. Read more in the lesson, Implementing Authorization Checks, Task: Authorization Check (Principle), in the course BC400 (Unit 6, Lesson 5) or TAW10 Part I (Unit 12, Lesson 5).

## Unit 7

You can use the Menu Painter to adapt the default list interface to your own needs. Read more in the lesson, Implementing ABAP Lists, Task: Standard List Functions, in the course BC400 (Unit 7, Lesson 1) or TAW10 Part I (Unit 13, Lesson 1).

The selection screen has the following standard functions: Texts in several languages, Automatic type checking, Complex selections for intervals, comparative conditions, and patterns, Field documentation for input fields, Search help associated with the Dictionary type, Saving of selection screens as variants. Read more in the lesson, Implementing Selection Screens Task Standard Functions of the Selection Screen, in the course BC400 (Unit 7, Lesson 2) or TAW10 Part I (Unit 13, Lesson 2).

An input variable is defined with a `PARAMETERS` statement instead of a `DATA` statement. The definition of such an input variable creates a variable in the system and generates a selection screen with a corresponding input option. Read more in the lesson, Implementing Selection Screens, Task: Single-Value Input, in the course BC400 (Unit 7, Lesson 2) or TAW10 Part I (Unit 13, Lesson 2).

Characteristics of an Event Block are: Introduced with an event keyword. It ends by the beginning of the next processing block, Cannot be nested, Existence is not absolutely necessary, Sequence of event blocks is unimportant. Read more in the lesson, Implementing Events of ABAP Reports, Task: Event Block Characteristics, in the course BC400 (Unit 7, Lesson 3) or TAW10 Part I (Unit 13, Lesson 3).

## Unit 8

Use a dialog transaction to trigger screen processing. Read more in the lesson, Creating Screens, Task: Runtime Architecture of the Screen Flow Control, in the course BC400.

To make input mandatory, assign the Required attribute to a screen field. Read more in the lesson, Creating Input and Output Fields, Task Maintenance of the Attributes of a Screen Element, in the course BC400.

You use the TABLES statement to define a structure variable of the same type and name with reference to the specified Dictionary structure within the program. This structure variable then serves as the interface between the program and the screen. Read more in the lesson, Implementing Data Transport, Task: The TABLES Structure as an Interface to the Screen, in the course BC400.

## Unit 9

An SAP Container Control (container), such as `CL_GUI_CUSTOM_CONTAINER`, is used to link the EnjoySAP control and the control area on the screen. The EnjoySAP control is embedded in the container, which in turn is integrated in the control area. Read more in the lesson, Using the SAP List Viewer, Task: Runtime Architecture of the ALV Grid Control, in the course BC400.

The `CL_GUI_ALV_GRID` global class has numerous methods that can be called for the corresponding grid control functions. The methods `CONSTRUCTOR`, `SET_TABLE_FOR_FIRST_DISPLAY` and `REFRESH_TABLE_DISPLAY` are some of them. Read more in the lesson, Using the SAP List Viewer, Task: Important Methods for the Grid Control Class , in the course BC400.

## Unit 10

The screen sequence defined in the window is started by way of the Web Dynpro application. Read more in the lesson, Describing Web Dynpro ABAP, Task: Application Example, in the course BC400.

A component contains one or several windows and has its own controller, the `COMPONENTCONTROLLER`. Windows embed one or more views and define navigation options between them (view sequences). Read more in the lesson, Describing Web Dynpro ABAP, Task: Web Dynpro Component, in the course BC400.

Data is transported between the central component controller and the individual view controllers using context mapping. Read more in the lesson, Implementing Data Transport in Web Dynpro, Task: Data Transport, in the course BC400.

## Unit 11

When you initiate a check from inside the ABAP Editor, the Code Inspector uses a default check variant with a predefined set of checks to be performed. You can overwrite this default check variant by creating a new check variant. Calling the Code Inspector from the context menu of the object list will make use of the `DEFAULT` variant. To define individual checks (check variants, object sets, inspections), start the Code Inspector with transaction code SCI. Read more in the lesson, Improving the Quality of ABAP Code with the Code Inspector, Task: Definition of Check Variants, Object Sets, and Inspections, in the course BC400 (Unit 11, Lesson 1) or TAW10 Part I (Unit 14, Lesson 1).

The most important check categories are as follows: General Checks, Performance Checks, Security Checks, Syntax Check and Generation, Programming Conventions, Search Functions. Read more in the lesson, Improving the Quality of ABAP Code with the Code Inspector, Task: Check Categories, in the course BC400 (Unit 11, Lesson 1) or TAW10 Part I (Unit 14, Lesson 1).

## Unit 12

ABAP Development Tools (ADTs) are an alternative to the ABAP Workbench. ADTs also enable cross-platform development by integrating ABAP and non- ABAP development tools in a single powerful integrated development environment. In addition, ADTs are tightly integrated with SAP HANA studio, SAPUI5 tools, and JAVA. Read more in the lesson, Describing ABAP Development Tools for SAP NetWeaver, Task: Benefits of Eclipse, in the course BC400.

As an ABAP programmer, you can develop ABAP programs with Eclipse- based ABAP development tools as an alternative to using the ABAP Workbench. Read more in the lesson, Describing ABAP Development Tools for SAP NetWeaver, Task: Business Example, in the course BC400.

## Unit 13

Repairing an SAP repository in a customer system is also called a modification. Read more in the lesson, Adjusting the SAP Standard Software, Task: Comparison of Terms, in the course BC400.

Customizing, Enhancements and Customer developments are options for adjusting the SAP software. Read more in the lesson, Adjusting the SAP Standard Software, Task: Adjustment Options for SAP Standard Software to Meet Customer Requirements, in the course BC400.

In addition to program exits, the following enhancement types exist: Menu Exits and Screen Exits. Read more in the lesson, Adjusting the SAP Standard Software, Task: Other Enhancement Types, in the course BC400.

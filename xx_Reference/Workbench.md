# ABAP Workbench

## ABAP Repository

Consists of all system development objects, such as programs, function modules, definitions of database tables, etc. The Repository is in the database and is independent of the client - this means that a Repository object can be accessed from any client and looks the same in each client in the system. The database also contains application and customizing data, which is normally client- dependent.

The system subdivides the Repository according to application components. Whenever a Repository object is created, it must be assigned to a package.

The **Repository Information System** is suitable for random (that is, not application-specific) searches of Repository objects. To access it, use `SE84` **Object Navigator** transaction and double-click on an object type - a selection screen appears allowing you to limit your search.

Usage example: expand **ABAP Dictionary** from the Objects view to the left, double-click on **Database Tables** and type in a name of the table (in my case `ZNV_TASK1_EMPL`), push the **Execute** button (to the right from the search box) to display the table, then double-click on the table name to display it.

ABAP Workbench Tools:
* `SE38` the ABAP Editor - for editing the source code
* `SE11` the ABAP Dictionary - for editing database table definitions, data types, and other entities
* `SE12` the ABAP Dictionary (display only)
* `SE51` the Screen Painter - for configuring screens together with functions for user dialogs
* `SE41` the Menu Painter - for designing user interface components: menu bar, standard toolbar, application toolbar, and function key settings
* `SE37` the Function Builder - for maintaining function modules
* `SE24` the Class Builder - for maintaining global classes and interfaces

## Creation of Packages

The attributes of a package have the following meaning:
* **application component**: determine the location of the package within the application hierarchy by specifying the corresponding application component
* **software component**: for customer developments, enter `HOME` for the software component
* **transport layer**: the transport layer determines if the objects of this package are to be transported to a subsequent system and, if so, to which system. For customer developments you have to specify the transport layer that your system administrator set up for this purpose
* **direct superpackage**: the immediate superpackage (or synonym "surrounding package") refers to the package that contains the current package as a direct subpackage based on the package hierarchy. The package hierarchy orders all packages in the form of several trees: Each package has (potentially) a number of subpackages and can, in turn, be the subpackage of a surrounding package
* **package type**: choose between the three package types: development package (can contain Repository objects and other packages), main package (can only contain other packages), and structure package (can only contain main packages)
* **flag for package encapsulation**: when this package property is activated, the package is encapsulated. The consequence of this is that only the development elements exposed in package interfaces of the package are visible to the outside. Possible client packages need use accesses to those package interfaces that contain the development elements used

Example of a package creation dialogue:
```
	Package:				ZBC400_00 " comply with customer namespace
	Short description:		Exercises for group 00
	Application Component:	CA " to which app component does the package belong?
	Software Component:		HOME
	Transport Layer:		ZDEV " where do you want to transport the dev objects
	Superpackage:			
	Package Type:			Development Package
	Package Encapsulated:	[ ] " checkbox not ticked
```

Transport route specification examples:
* `DEV` : development system
* `QAS` : test and translation system
* `PRD` : production system

To create a package:
1. navigate to the `SE80` Object Navigator
2. in the navigation area, choose the **Package** object type and enter the name of the package in the input field below, ensuring that you comply with the customer namespace conventions. Press Enter. If the specified package does not already exist, the system will branch off to a dialog to create a package. Alternatively, you can choose Edit Object on the initial screen of the Object Navigator. In the dialog box, search for the option of specifying a package and enter the name of the package. Press `F5` to create the object.
3. set the attributes of the package to be created
4. assign the package to a change request

## Creation of Transactions

To create a transaction:
1. in the Object Navigator, display the object list for the program
2. in the navigation area, select your program and choose **Create → Transaction** from the context menu
3. enter the required transaction code - make sure you comply with the customer namespace conventions; assign a short text and select the **Program and Selection Screen (Report Transaction)** radio button
4. on the next screen, enter the name of the program and choose **Professional User Transaction**; under **GUI enabled**, set the **SAP GUI for Windows** indicator
5. save the transaction
6. assign the transaction to a package and to a change request on the following screens

To add transaction to personal favorites:
1. navigate to the initial screen, the **SAP Easy Access** screen
2. in the Favorites context menu, choose **Insert Transaction**
3. in the dialog box, enter the required transaction code; the short text of the transaction now appears under the Favorites context menu; you can start the corresponding program by double-clicking it

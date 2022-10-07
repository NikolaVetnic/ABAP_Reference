## ABAP Development Tools (for Eclipse)

When you use ADT, you log on to an SAP back-end system and work directly with its repository objects - the development process is exactly the same as when you use the *ABAP Workbench*.

An ABAP project serves as a container for the development objects that are stored in a particular ABAP back-end system, and contains the logon parameters for the system logon — System, Client, User, and language. To make it easier to manage objects you can set up favorite packages for each project.

Types of editors:
* native Eclipse editors
	* Eclipse features
	* Eclipse-like look and feel
	* available for some Repository Objects (e.g. Programs, Classes, Web Dynpro, etc.)
* integrated SAP GUI editors
	* no new features
	* SAP GUI look and feel
	* used where no native editor is available (e.g. Transactions, Dictionary Objects, Screens, etc.)

### Overview of Refactoring Options

In Eclipse, **Quick Fixes** are shortcuts to complete tasks or resolve errors that are proposed automatically by the editor (the key combination `CTRL + 1` shows you available quick fixes in a dialog box):
* add missing implementation (for entire class or individual method)
* generate method (for example instance constructor or setter/getter method for an attribute)
* change visibility (for example, turn local variable into attribute, make private component public)
* rename component ( attribute, method, variable, interface)

Method extraction involves moving a code block to a new method and replacing the block with a call to that new method. To perform method extraction, you must be editing a global or local ABAP class in a class pool, a program, or another type of ABAP program. Your code must be free of syntax errors before you begin. Note that method extraction is not supported in code outside of classes and that you can only extract a method within the start and end area of another method.

Another key refactoring technique is the deletion of the unused variables that can accumulate in code over time.

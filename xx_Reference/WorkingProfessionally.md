## Working with ABAP Professionally

### ABAP System Architecture Basics

ABAP-based systems follow a client/server architecture - ABAP code executes centrally on the ABAP server that the user accesses through one of the clients. Server works with a DB, most often SAP S/4HANA.

Customers will have at least one each of development (DEV), quality assurance (QA), and production (PRD) environments - these are referred to as a *landscape*. Each environment is identified by a three-character system ID (client number, unique within each SAP customer’s landscape).

Client in ABAP systems **is not the same** as client in client/server architecture - in former, it means a separate area in the DB within the same system, while in latter it is the application used to access an SAP ERP system. `MANDT` DB field is tied to an ABAP client and is thus part of a key in client-dependent tables (there are also client-independent ones). ABAP code base is, unlike business data, always client-independent and is the same within one environment (DEV, QAS or PRD).

For example, a company could have one client (`MANDT`) for its North American business unit and another client for its European business unit. The ABAP programs would be the same in both clients, but users in each business unit would only be able to see data in their own client that they chose while logging in, almost as if there were two separate systems running the same programs using separate data. 

An ABAP system must have at least one client. Each client can have different settings to allow or disallow certain activities, such as changing configuration or ABAP code.

The different environments, or systems, are connected via the SAP Trans- port Management System (SAP TMS). System configuration or ABAP pro- gram changes are moved across the landscape using transports, collections of changes associated with the user who made them.

### Transport Management

Changes in development objects and most configuration changes are moved across the landscape using transports, standard SAP features that keep track of changes being made and which, after release, contain a specific set of changed objects to be moved, or transported, to the next system in the transport route. You can view your system’s SAP TMS configuration by entering Transac- tion STMS and clicking the Transport Routes button.

The transport path for a specific ABAP object is determined by which **package** it’s assigned to. The package is the highest level in the development object hierarchy (can be compared to a root folder). Packages are used to organize ABAP development and data dictionary objects and to assign them a transport route. Assigning the `$TMP` package is the equivalent of saving an object locally. This special package exists in every ABAP system and it’s used for objects that must not be transported anywhere else.

#### Creating a package

In Eclipse, a new package can be created by using the menu option **New • ABAP Package**; after entering the package name, the SAP GUI screen will open to complete the rest of the upcoming steps.

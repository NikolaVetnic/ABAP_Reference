# Persistent Objects and OO Transactions

In principle, ABAP programs work with data and objects that are only valid at runtimei (transient, data in memory is temporary and disappears when the program ends). To store the data permanently and independent of the program, you must store it in a database. Theoretically, you can also use files at the operating system level.

Тhe ABAP developer can use these services and write the current attribute values of objects as persistent to the associated transparent tables. To read the values from the tables, persistent services can be used to import the values into an object previously defined as persistent. When doing this, the persistence services use the ABAP Open SQL interface.

You will use the persistence services with object-oriented programming techniques. In this way, you can simulate an object-oriented database management system using a relational database management system and ABAP Objects.

## Persistent Classes

To use the persistence service for objects, you must create their types as persistent classes in Class Builder. The term persistent class indicates that the persistence service manages the instances of the class and their state.

Global classes are generally persistent. The question is whether or not their instances are also persistent.

To ensure that the instances of persistent classes are unique, they must contain key attributes. You can type these either as worldwide unique identification numbers (object GUID) or semantic keys.

When you create a persistent class `ZCL_<name>`, Class Builder automatically generates methods for getting and setting its attributes. In addition, the system also generates other repository objects, including the class actor (also known as the class agent) `ZCA_<name>`. Programs must call the methods of this class to manage the instances of persistent classes, that is, persistent objects. The class actor also performs the actual database access. It automatically inherits the required methods from the basis class `ZCB_<name>`. You can redefine these methods, if necessary. For example, extend database accesses. The class actor is a singleton instance and has a friendship relationship with the persistent class.

## Creation of Persistent Objects

In the program, you need to define a reference variable with the type of the class actor and fill it with the reference from the static attribute `AGENT`. You can then create a new instance of the persistent class using its method `CREATE_PERSISTENT`. The ABAP developer creates its interface from the definition of the key attributes of the persistent class. If a persistent object of the same persistence class with the same key attributes already exists in the program, the class-based exception `CX_OS_OBJECT_EXISTING` is triggered. By default, the data is saved by an asynchronous update. You must start this process by using the `COMMIT WORK` statement. Thus, the instances of the persistent class do not become persistent objects until after that point:
```ABAP
	DATA:
		go_carrier TYPE REF TO zcl_carrier,
		go_agent   TYPE REF TO zca_carrier.
	...
	go_agent = zca_carrier=>agent. " singleton instance of class ZCA_CARRIER is friend of ZCL_CARRIER

	TRY.
			go_carrier = go_agent->create_persistent(
				i_carrid = 'LH'
				i_carrname = 'Lufthansa'
			).
		CATCH cx_os_object_existing.
			...
	ENDTRY.
	
	go_carrier-> ...

	COMMIT WORK.
```

## Access to Persistent Objects

You can load a single persistent object back into a program using the `GET_PERSISTENT` method. When doing so, you must pass the key values to the interface parameters so that you can uniquely restore the object. If the program does not find the object, it raises the class-based exception `CX_OS_OBJECT_NOT_FOUND`:
```ABAP
	DATA:
		go_carrier  TYPE REF TO zcl_carrier,
		go_agent    TYPE REF TO zca_carrier,
		gv_carrname TYPE s_carrname.
	...
	go_agent = zca_carrier=>agent.
	
	TRY.
			go_carrier = go_agent->get_persistent( i_carrid = 'LH' ).
			gv_carrname = go_carrier->get_carrname( ).
			WRITE: 'LH : ', gv_carrname.
		CATCH cx_os_object_not_found.
			...
	ENDTRY.
```

You can retrieve a larger set of persistent objects by using the Query Service. It is possible to provide a selection, much like the `WHERE`-clause of a `SELECT` statement, as well as the ordering criteria.

## Create a Persistent Classes

Checklist:
1. model the class for your persistent objects
2. find or create a suitable transparent table that has a field for each attribute of the class. If necessary, adapt the attributes of the class to match the technical field attributes of the transparent table (there must be a primary key field in the table for each key attribute of the class or there must be a key attribute of the class for each primary key field in the table)
3. in the Class Builder, create a persistent class as described in the SAP Library; define the persistence mapping to the table

## OO Transaction

In transaction maintenance, you can create transaction code as an OO transaction. This means that you link the transaction code either to the transaction service of the ABAP Objects Services for persistent objects or a public method in a global or local class of a program. When calling this type of OO transaction, which is linked to an instance method, the system automatically creates an instance of the class in its own internal session and then executes this instance method.

If the OO Transaction Model flag is set, the system links the transaction code to the transaction service of the ABAP Objects Services. Conversely, if it is not set, you can call any method in a global or local class.

Users, therefore, now have the opportunity to call methods themselves in the usual way (using menu options, command field entries, and so on).

Breaking these rules will not cause a syntax error or message from the Extended Syntax Check. Instead, you will either cause a runtime error or there will be no display

Therefore, you may have to resort to using methods in local program classes. Naturally, you can then use all the conventional dialog programming techniques.

You can create an OO transaction by using the following steps:
1. in the Create Transaction dialog box, enter a description in the Short Text field
2. choose the Method of a Class (OO Transaction) option as an Initial Object
3. if you want to link the transaction code to a normal instance method, leave the OO Transaction Model checkbox unselected
4. enter the Class Name and Method. If you are using a local class, select the Local in Program checkbox and enter the program name
5. save the transaction code

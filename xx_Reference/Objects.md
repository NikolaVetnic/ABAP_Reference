## ABAP Objects

ABAP Objects is not a new language, but a systematic extension of ABAP. You can use ABAP Objects statements in procedural ABAP programs. ABAP Objects syntax example:
```
	REPORT ... .

	DATA:
		gv_counter TYPE i,
		gs_kna1 TYPE kna1.
	...

	CLASS lcl_car DEFINITION.
		...
	ENDCLASS.

	* =-=-=-= main program =-=-=-=

	gv_counter = gv_counter + 1.

	CREATE OBJECT ...

	MOVE gs_kna1 TO ...
```

Type checks in the object-oriented contexts of ABAP Objects are stricter than those in the procedural contexts. For a list of obsolete language elements, refer to the ABAP keyword documentation. 

### Client/Server Relationship

Objects behave like client/server systems. When one object sends a message to another object to ask it to behave in a certain way, the first object is defined as a client and the second object is defined as a server.

### Key Characteristics

Key characteristics of OO model of ABAP Objects:
* objects are a direct abstraction of the real world
* objects are units made up of data and functions
* processes can be implemented in a better way than in procedural programming

Advantages of the OO approach in regards to procedural programming model:
* improved software structure and consistency in the development process
* reduced maintenance effort and less susceptibility to errors
* better integration of the customer or user into the analysis, design, and maintenance process
* simpler and more secure extension of the software

## Topics Covered in More Detail
* [Classes](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Objects/Classes.md)
* [Inheritance](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Objects/Inheritance.md)
* [Interfaces](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Objects/Interfaces.md)
* [Events](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Objects/Events.md)
* [Global Classes](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Objects/GlobalClasses.md)
* [Business Add-Ins](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Objects/BAdI.md)
* [ABAP Development Tools (for Eclipse)](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Objects/ADT.md)
* [Class-Based Exceptions](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/xx_Reference/Objects/ClassBasedExceptions.md)


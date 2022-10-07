## Interfaces

Interfaces can be seen as superclasses that cannot be instantiated, do not contain implementations, and have only public components. You can simulate multiple inheritances using interfaces.

In ABAP Objects, interfaces primarily serve to define uniform interface protocols for services. Various classes can implement these services in different ways, but you need to keep the same semantics.

Generalization and specialization relationships using interfaces:
```
	INTERFACE lif_partner.
		METHODS display_partner.
	ENDINTERFACE.
	" =-=-=-=
	CLASS lcl_rental DEFINITION.
		PUBLIC SECTION.
			INTERFACES lif_partner.
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_rental IMPLEMENTATION.
		METHOD lif_partner~display_partner.
			...
		ENDMETHOD.
	ENDCLASS.
```

Access to interface components:
```
	DATA go_rental TYPE REF TO lcl_rental.
```

Simplifying access to interface components with alias names:
```
	INTERFACE lif_partner.
		METHODS display_partner.
		DATA gv_partner_id TYPE n LENGTH 10.
	ENDINTERFACE.
	" =-=-=-=
	CLASS lcl_rental DEFINITION.
		PUBLIC SECTION.
			INTERFACES lif_partner.
			ALIASES display_partner1
				FOR lif_partner~display_partner.
		PRIVATE SECTION.
			ALIASES gv_partner_id FOR lif_partner~gv_partner_id.
	ENDCLASS.
	" =-=-=-=
	DATA go_rental TYPE REF TO lcl_Rental.
	...
	CREATE OBJECT go_rental ... .
	...
	" go_rental->lif_partner~display_partner( ).
	go_rental->display_partner1( ).
```

An interface reference can only refer to instances of classes that have implemented the interface because interfaces themselves cannot be instantiated (a typical area of use for upcast assignments is preparation for generic access):
```
	DATA:
		go_rental TYPE REF TO lcl_rental,
		go_partner TYPE REF TO lif_partner.
	CREATE OBJECT go_rental.
	go_partner = go_rental. " up-cast
```

To assign an interface reference to a class reference where the class has implemented the interface, you must use the down-cast assignment operator `MOVE ... ?TO ...` or its short form `?=`:
```
	METHOD book_flight.
		DATA:
			lo_carrier TYPE REF TO lcl_carrier,
			lo_partner TYPE REF TO lif_partner.
		LOOP AT mt_partners INTO lo_partner.
			TRY.
				lo_carrier ?= lo_partner.
				" call method of lcl_carrier to book flight
				CATCH cx_sy_move_cast_error.
					" react to cast error
			ENDTRY.
		ENDLOOP.
```

Interfaces like regular superclasses can include other interfaces. As with regular inheritance, the interface hierarchies can be of any depth. The including interface is a specialization of the included interface. The including interface is known as a compound interface, represents an extension of the included interface:
```
	INTERFACE lif_partner.
		METHODS display_partner.
	ENDINTERFACE.
	" =-=-=-=
	INTERFACE lif_lodging.
		INTERFACES lif_partner.
		METHODS book_room.
	ENDINTERFACE.
	" =-=-=-=
	CLASS lcl_hotel DEFINITION.
		PUBLIC SECTION.
			INTERFACES lif_lodging.
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_hotel IMPLEMENTATION.
		METHOD lif_partner~display_partner.
			...
		ENDMETHOD.
		METHOD lif_lodging~book_room.
			...
		ENDMETHOD.
	ENDCLASS.
```

As with regular inheritance, the implementing class only needs to list the compound interface in order to integrate all components. Nevertheless, the components of the component interfaces keep their original names:
```
	component_interface_name~component_name
```

Addressing components in compound interfaces syntax:
```
	DATA:
		go_hotel TYPE REF TO lcl_hotel,
		go_partner TYPE REF TO lif_partner,
		go_lodging TYPE REF TO lif_lodging.

	go_hotel->lif_partner~display_partner( ).
	go_hotel->lif_lodging~bok_room( ).

	" up-casts for generic access
	go_lodging = go_hotel.
	go_lodging->lif_partner~display_partner( ).
	go_lodging->book_room( ).

	go_partner = go_hotel.
	go_partner->display_partner( ).

	" down-casts for specific access again:
	go_lodging ?= go_partner.
	" or:
	go_hotel ?= go_partner.
```

If there is no suitable way to link classes in terms of inheritance creating generalization/specialization relationships using interfaces can have the following advantages:
* separation of the protocol interface - often defined by user and the service implementing class
* safe, generic method of access
* ability to simulate multiple inheritance

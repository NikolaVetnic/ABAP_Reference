## Events

Instance events can be triggered by the instances of the class, but static events can be triggered by the class itself. Events can also be defined as interface components.

Given the right circumstances, handler methods react to the triggering of this event. This means that the runtime system may call these handler methods after the event has been triggered. In other words, the client usually does not call the handler method directly.

This results in a completely different modeling concept. While you are developing the class that triggers the event, you do not need to know anything about the class that is handling it. The triggering class sends a specific message to all classes and, if required, their instances. At the time of development, type of handlers and the number of handlers, which may be used are not known.

An example of event emitting and handling can be viewed in these files:
* [`znvcl_ev_vehicle`](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/Snippets_BC401_EN_Col18/bc401_05_znvcl_ev_vehicle)
* [`znvcl_ev_rental`](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/Snippets_BC401_EN_Col18/bc401_06_znvcl_ev_rental)
* [`znv_ev_main`](https://github.com/NikolaVetnic/ABAP_Reference/blob/master/Snippets_BC401_EN_Col18/bc401_07_znv_ev_main)

The visibility of an event defines where the event can be handled: `PUBLIC` - can be handled anywhere, `PROTECTED` - can only be handled in its own class or its subclasses, `PRIVATE` - can only be handled in its own class.

The visibility of an event handler defines where the handler method can be registered: `PUBLIC` - can be registered anywhere in the program, `PROTECTED` - can be registered within its own class or its subclasses, `PRIVATE` - can only be registered within its own class.

Events can be defined as interface components - triggering and handling events are done the same way as in classes: **1)** define event in an interface, **2** trigger the interface event in implementing classes, **3)** handle interface event in handler class (client class), **4)** register event handling.

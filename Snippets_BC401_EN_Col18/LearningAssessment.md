# Learning Assessment

## Unit 1

1. Data and functions are kept separate in the procedural programming model. **Answer:** true.

2. What does multiple instantiation mean? **Answer:** creating and managing runtime instances.

3. Which of the following is a simplification of complex relationships in the real world? **Answer:** abstraction.

4. Which kind of UML diagram pays particular attention to the sequence in which objects relate to each other? **Answer:** behavior.

5. You can group all characteristics and behaviors of similar objects into one central class. **Answer:** true.

### Unit 2

1. The CLASS statement can be nested, that is, you can define a class within a class.**Answer:** false.

2. Which one of the following syntax elements is used to define static attributes? **Answer:** `CLASS-DATA`.

3. Which of the following options is used to create an object? **Answer:** `CREATE OBJECT ref_name`.

4. You must create and address objects using reference variables. **Answer:** true.

5. During program runtime, you create discrete objects (instances) in memory for an existing class. This process is called instantiation. **Answer:** true.

6. When calling a static method from within the class, you can omit the class name. **Answer:** true.

7. In which of the following expressions can functional methods be called directly? **Answer:** `IF`, `COMPUTE`, `MOVE`, `WHILE`.

8. You can describe methods that have a `_________` parameter as functional methods. **Answer:** `RETURNING`.

9. You have to define RETURNING parameters using the VALUE addition, that is they must be pass by value. **Answer:** true.

10. The `_____________` is a special instance method in a class. **Answer:** constructor.

11. The instance constructor is automatically called at runtime with the CREATE OBJECT statement. **Answer:** true.

12. Which of the following points are true about the static constructor? **Answer:** Each class has not more than one static constructor.

13. The instance constructor’s signature can have importing parameters or exceptions.**Answer:** true.

## Unit 3

1. Which of the following are the characteristics of inheritance? **Answer:** Common components can only exist once in the superclass, subclasses contain extensions or changes.

2. A superclass is a generalization of its subclasses. **Answer:** true.

3. Suppose that you have a class `X` that inherits from class `Y`. After an up-cast a reference variable that is statically typed `TYPE REF TO Y` points to an instance of class `X`. Which of the following components of class `X` can you access with this reference variable? **Answer:** components inherited from class `Y`, components redefined in class `X`.

4. When objects from different classes react differently to the same method calls, this is known as `__________`. **Answer:** polymorphism.

5. A typical use for `________` assignments is to prepare for generic access. **Answer:** upcast.

6. Which of the following is used to assign a superclass reference to a subclass reference? **Answer:** narrowing cast.

7. Which of the following is the downcast assignment operator? **Answer:** `?=`.

8. Suppose that you have the same class `X` that inherits from class `Y`. After a down-cast, a reference variable that is statically typed `TYPE REF TO X` points to an instance of class `X`. Which of the following components of class `X` can you access with this reference variable? **Answer:** components defined in class `X`, components inherited from class `Y`, components redefined in class `X`, components defined in class `X` and redefined in its subclasses.

9. Which of the following are advantages of correctly using class hierarchies? **Answer:** centralized maintenance, safe and generic method of access.

## Unit 4

1. You can access interface components only by using an object reference. **Answer:** false.

2. Which of the following is the main strength of interfaces? **Answer:** polymorphism.

3. `?=` is the down-cast assignment operator. **Answer:** true.

4. Suppose a reference variable that is typed to an interface contains an instance reference of a class that implements this interface and you copy this to a reference variable that is typed to the class (down-cast). Which of the following components can you access with this reference variable? **Answer:** the components of the interface, the components from the class that are not defined on the interface, all components of the class, the components of the interface for which alias names have been defined.

5. Which of the following strongly resembles inheritance? **Answer:** interface.

6. Interface can include other interfaces. **Answer:** true.

##  Unit 5

1. Which of the following cannot be defined as interface components? **Answer:** Classes.

2. Which of the following statements is used to trigger events? **Answer:** `RAISE EVENT`.

3. You can trigger static events only in static methods. **Answer:** false.

4. Which of the following is specified by the definition of the handler method? **Answer:** Which method will react to which event of which class.

5. Handler methods are registered using the `SET HANDLER` statement. Registration is only active at program runtime. **Answer:** true.

6. Which visibility section would an event need to be defined in, to ensure that it can only be handled in the class itself and its subclasses? **Answer:** `PROTECTED`.

## Unit 6

1. You can only use local classes or interfaces within the same program in which they are defined and implemented. **Answer:** true.

2. The naming convention for SAP Standard Global Interfaces is `ZIF_` or `YIF_`. **Answer:** false.

3. Which of the following buttons in Class Builder is used to override an inherited method? **Answer:** Redefine.

4. Local types of the global class are encapsulated and cannot be accessed from outside. **Answer:** true.

## Unit 7

1. The ABAP List Viewer (ALV) Grid Control is a tool that you can use to display nonhierarchical lists in a standardized form. **Answer:** true.

2. Container controls provide the technical connection between the screen and the application control. **Answer:** true.

3. To create a handler object for an event, we must first define a `____________`. **Answer:** class.

4. A handler method can be either a class method (static method) or an instance method of an object. **Answer:** true.

5. With a Business Add-In (BAdI), an SAP application program provides the enhancement option through an interface method. **Answer:** true.

6. An object of the BAdI adapter class is instantiated by the call of the static method `GET_INSTANCE` of the class `CL_EXITHANDLER`. **Answer:** true.

7. To implement a Business Add-In (BAdI), the BADI definition name must be determined. **Answer:** true.

8. The code of a BAdI implementation is stored in a `__________` of an automatically generated customer class. **Answer:** method.

## Unit 8

1. When you work with ADT, you log onto the ABAP back-end system and remain logged on for your entire session. **Answer:** true.

2. ADT contains native Eclipse editors for which kinds of repository objects? **Answer:** ABAP programs, ABAP classes.

3. You are creating an ABAP class in ADT and have just written the definition of a method in the public section of the class definition. You press `CTRL + 1` to see the available quick fixes. Which of the following quick fixes are available? **Answer:** add the implementation of the method, move the method definition to the protected or private section.

## Unit 9

1. Which of the following statements is used to raise class-based exceptions? **Answer:** `RAISE EXCEPTION`.

2. Which of the following blocks is used to catch and handle exceptions? **Answer:** `TRY... CATCH... ENDTRY`.

3. A class-based exception can only be handled if the statement that raised it is enclosed in a TRY-ENDTRY control structure. **Answer:** true.

4. `TRY-ENDTRY` structures can be nested to any depth. **Answer:** true.

5. You can specify only two exception classes to the CATCH statement. **Answer:** false.

6. If an exception is raised, the name of the exception class is displayed in the `__________` field in debugging mode. **Answer:** exception raised.

7. Which of the following syntax additions is used to propagate an exception from a procedure? **Answer:** `RAISING`.

8. For subclasses of `____________` the corresponding exceptions cannot be propagated explicitly using the `RAISING` addition. **Answer:** `CX_NO_CHECK`.

9. Which of the following are ways of handling an exception? **Answer:** continue program, remove the cause of error, do not propagate an exception.

10. Which of the following is used to jump back to the TRY statement? **Answer:** `RETRY`.

11. Which of the following is a prerequisite for using the RESUME statement? **Answer:** the exception has to be caught with addition `BEFORE UNWIND`, the exception has to be propagated with addition `RESUMABLE( )`, the exception has to be raised with addition `RESUMABLE`.

## Unit 10

1. In ABAP Unit, the SETUP method is called before each individual test method. **Answer:** true.

2. Which of the following statements about ABAP Unit tests are true? **Answer:** test classes are not generated in a production system, each test method is executed independently of all of the others.

## Unit 11

1. Static `_____________` cannot be abstract because they cannot be redefined. **Answer:** methods.

2. You can prevent a class from being inherited from by defining it as final. **Answer:** true.

3. An association means that at runtime an instance of one class stores references to objects of another class. **Answer:** true.

4. If the visibility of the instance constructor is changed from private to protected, the visibility is extended to all of its subclasses. **Answer:** true.

5. Which of the following are the advantages of a factory method? **Answer:** a factory method can have coding that is executed before the actual instantiation, a class can have more than one factory method with different implementations and signatures.

6. Which of the following concepts is used to provide a class access to the private components of the other class? **Answer:** friendship.

# In More Detail...

## Unit 1

The procedural programming model allows separation of data and functions. In an ABAP, procedural program, global variables contain data, while subroutines contain functions. Read more in the task, The Procedural Programming Model, in the lesson, Introduction to Object-Oriented Programming, in course BC401.

You are correct! Multiple Instantiation is the possibility to create several runtime instances using the same program context. It is one of the key characteristics of object-oriented programming. Read more in the task, Multiple Instantiation in Object-Oriented Programming, in the lesson, Introduction to Object-Oriented Programming, in course BC401.

Unlike in procedural programming, using multiple instantiation in object- oriented programming allows you to create a direct abstraction of a real object. Objects are a direct abstraction of the real world. Read more in the task, Object-Oriented ABAP, in the lesson, Introduction to Object-Oriented Programming, in the course, BC401.

The different diagram types in UML represent different views of a system. Behavior diagrams show the sequence in which the objects relate to each other. Read more in the task, Class Diagrams, in the lesson, Analyzing and Designing with Unified Modeling Language (UML), in the course, BC401.

Object-oriented programming views the real world as a collection of objects. Some of these objects are similar and can be described in the same way if they use the same characteristics and exhibit the same behavior. You can group all the characteristics and behaviors of these similar objects into one central class. Read more in the task, Classification of Objects, in the lesson, Analyzing and Designing with Unified Modeling Language (UML), in the course, BC401.

## Unit 2

One of the characteristics of a class in object-oriented programming is that a class statement cannot be nested; that is, you cannot define a class within a class. However, you can define local auxiliary classes for global classes. Read more in this lesson, Creating Local Classes, Task Characteristics of a Class, in the course, BC401.

**Static attributes** exist once for each class and are visible for all runtime instances in that class. You define static attributes with the syntax element, CLASS-DATA. Read more in this lesson, Creating Local Classes, Task Static Attributes and Instance Attributes, in the course, BC401.

The `CREATE OBJECT` statement creates an object in the memory. The object attribute values are initial or assigned according to the `TYPE` specification. Read more in the task, Creating Objects, in the lesson, Creating Objects, in the course, BC401.

The reference variable can point to the instance of a class. The initial value of a reference variable is an empty reference, that is, the reference points to nothing at all. Read more in the task, Definition of Reference Variables, in the lesson, Creating Objects, in the course, BC401.

A class contains the generic description of an object and describes the characteristics that all objects in that class have in common. During the program runtime, you use the class to create discrete objects (instances) in the memory. This process is called instantiation. Read more in the task, Objects as Instances of Classes, in the lesson, Creating Objects, in the course BC401.

Static methods are addressed with their class name, since they do not need instances. When you call a static method from the class, you can omit the class name. Read more in the task, Static Method Calls, in the lesson, Accessing Methods and Attributes, in the course, BC401.

You can call functional methods within the following expressions: `MOVE`, `LOOP`, Logical expressions: `IF`, `ELSEIF`, `WHILE`, `CHECK`, `WAIT`, arithmetic expressions and bit expressions: `COMPUTE`, case conditions: `CASE`, `WHEN`. Read more in the task, Functional Method Calls, in the lesson, Accessing Methods and Attributes, in the course, BC401.

Methods that have a `RETURNING` parameter are described as functional methods. Read more in the task, Functional Method Calls, in the lesson, Accessing Methods and Attributes, in the course, BC401.

The `RETURNING` parameter must always be passed by value - `RETURNING VALUE(...)` - and not by reference. Read more in the task, Functional Method Calls, in the lesson, Accessing Methods and Attributes, in the course, BC401.

The instance constructor is a special instance method in a class and is always named `CONSTRUCTOR`. Read more in the task, Instance Constructor, in the lesson, Implementing Constructors in Local Classes, in the course, BC401.

The constructor is automatically called at runtime with the `CREATE OBJECT` statement. Read more in the task, Instance Constructor, in the lesson, Implementing Constructors in Local Classes, in the course, BC401.

When you define static constructors, always consider the following points: 
* each class does not have more than one static constructor
* the static constructor must be defined in the public section
* the static constructor does not have any parameters or exceptions
* the static constructor cannot be called explicitly. 
Read more in the task, Static Constructor of the lesson, Implementing Class Constructors in Local Classes, in course BC401.

The instance constructor signature can only include importing parameters and exceptions. Read more in the task, Static Constructor of the lesson, Implementing Class Constructors in Local Classes, in course BC401.

## Unit 3

The common components of the subclasses only need to be defined and implemented in the superclass and they are inherited by all the subclasses. The subclasses are in turn different specializations of their superclasses. Therefore, additions (extensions) or changes are permitted in the subclasses. Read more in the task, Characteristics of Generalization and Specialization, in the lesson, Implementing Inheritance, in the course, BC401.

Inheritance must be used to implement generalization and specialization relationships. A superclass is a generalization of its subclasses. Read more in task, Implementation of Inheritance, in the lesson, Implementing Inheritance, of course BC401.

If you assign a subclass reference to a superclass reference, this subclass ensures that all components that you can access syntactically after the cast assignment are actually available in the instance. The subclass always contains at least the same components as the superclass always the name and the signature of redefined methods are identical. Read more in the task, Upcasts, in the lesson, Implementing Up-casts using Inheritance, of course BC401.

When objects of different classes react differently to the same method calls, this is known as polymorphism. A client can handle different classes uniformly, irrespective of their implementation. The runtime system searches for the right implementation of a method on behalf of the client. Read more in the lessons, Implementing Up-casts using Inheritance and Implementing Polymorphism Using Inheritance, of the course, BC401.

A typical use for up-cast assignments is to prepare for generic access. A user who is not interested in the finer points of the instances of the subclasses but simply needs to address the shared components can use a superclass reference for this access. Read more in the task, Generic Access to Objects, in the lesson, Implementing Polymorphism Using Inheritance, of the course, BC401.

To assign a superclass reference to a subclass reference, you must use the downcast assignment, also called narrowing cast. Read more in the task, Downcasts, in the lesson, Implementing Downcast Using Inheritance, of course BC401.

The downcast assignment operator is `MOVE ... ?TO ...` or its short form `?=`. Read more in the task, Downcasts, in the lesson, Implementing Downcast Using Inheritance, of course BC401.

After assigning a superclass type of reference to a subclass reference, clients are no longer limited to inherited components. Read more in the task, Downcasts, in the lesson, Implementing Downcast Using Inheritance, of course BC401.

Commonly used elements only need to be stored once in a central location (in the superclass). These elements are then automatically available to all subclasses. Changes made at a later stage have an immediate effect on the subclasses. Implementing Polyphormism using inheritance provides the generic access – one call, multiple implementations. Read more in the task, Usage of Class Hierarchies, in the lesson, Implementing Downcast Using Inheritance, of course BC401.

## Unit 4

You can access instance interface components only by using an object reference whose class implements the interface. But you can access static components by using the interface name. Read more in the task, Access to Interface Components, in the lesson, Defining and Implementing Local Interfaces, in course BC401.

The option of performing polymorphism is one of the main strengths of interfaces. A client can handle different classes uniformly, regardless of their implementation. Read more in the task, Generic Access Using the Interface Reference, in the lesson, Implementing Polymorphism Using Interfaces, in course BC401.

To assign an interface reference to a class reference where the class has implemented the interface, you must use the down-cast assignment operator MOVE ... ? TO ... or its short form ?=. Read more in the task, Downcasts with Interfaces, in the lesson, Integrating Class Models Using Interfaces, in course BC401.

The down-casting view is widened or at least unchanged. This is a switch from a view of a few components to a view of more components. The term Narrowing Cast is also used. Read more in the task, Downcasts with Interfaces, in the lesson, Integrating Class Models Using Interfaces, in course BC401.

An interface implementation strongly resembles regular inheritance. With regular inheritance, you can define large hierarchies of classes. When we use interfaces instead of classes we call that interface hierarchies. Read more in the task, Implementation of Interface Hierarchies, in the lesson, Integrating Class Models Using Interfaces, in course BC401.

In ABAP Objects, interfaces like regular superclasses can include other interfaces. As with regular inheritance, the interface hierarchies can be of any depth. Read more in the Compound Interface in Unified Modeling Language (UML) Notation, in the lesson, Integrating Class Models Using Interfaces, in course BC401.

## Unit 5

Attributes and methods can be defined as interface components. Events can also be defined as interface components. Read more in the task, Event-Controlled Method Calls, in the lesson, Implementing Events in Local Classes, in course BC401.

A class or instance can trigger an event at runtime using the `RAISE EVENT` statement. Both instance events and static events can be triggered in instance methods. In static methods, you can only trigger static events. Read more in the task, Event Triggering and Handling, in the lesson, Implementing Events in Local Classes, in course BC401.

You can trigger static events not only in static methods, but as well in instance methods. Read more in the task, Event Triggering and Handling, in the lesson, Implementing Events in Local Classes, in course BC401.

Instance or static methods can be defined within a class to handle events. To do so, you must specify in the definition of the handler the event using the `FOR EVENT` statement, and the class or interface in which the event was defined using the OF statement. Read more in the task, Handling Events, in the lesson, Implementing Events in Local Classes, in course BC401.

Events are registered using the `SET HANDLER` statement. Registration is only active at program runtime. Read more in the task, Registering Event Handling, in the lesson, Implementing Events in Local Classes, in course BC401.

The visibility of an event defines where the event can be handled: `PUBLIC` - Event can be handled anywhere, `PROTECTED` - Event can only be handled within its own class or its subclasses, `PRIVATE` - Event can only be handled within its own class. Read more in the task, Visibility Sections of Events, in the lesson, Implementing Events in Local Classes, in course BC401.

## Unit 6

As with subroutines, use local classes or local interfaces only within the program in which they are defined and implemented. The CLASS statement is a local declarative statement in the program. Read more in the task, Definition of Global Classes, in the lesson, Creating Global Classes, in course BC401.

The naming convention is IF_ for SAP global interfaces and ZIF_ or YIF_ for user-defined global interfaces. Read more in the task, Definition of Global Interfaces, in the lesson, Defining and Implementing Global Interfaces, in course BC401.

To redefine an inherited method, select the relevant method in the list and choose the Redefine button. Alternatively, you can use the context menu in the navigation area. Read more in the task, Redefinition of a Method in Global Class, in the lesson, Implementing Inheritance in Global Classes, in course BC401.

You can define local types in global classes. All components of the global class have access to these local types, but they are encapsulated if you try to access them from outside. The same applies for local interfaces in global classes. Read more in the task, Local Types in Global Classes, in the lesson, mplementing Inheritance in Global Classes, in course BC401.

## Unit 7

The ALV Grid Control is a tool that you can use to display non-hierarchical lists in a standardized form. Read more in the task, The ALV Grid Control, in the lesson, Using the ABAP List Viewer (ALV), in course BC401.

Container Controls provide the technical connection between the screen and application controls. The application controls ALV Grid Control, Tree Control, and Picture Control are always embedded in the Container Control, which is connected to the screen. Read more in the task, Including an ALV Grid Control Instance in a Dialog Program, in the lesson, Using the ABAP List Viewer (ALV), in course BC401.

To create a handler object for an event, define a class. Read more in the task, ALV Grid Control – Reacting to a Double-Click, in the lesson, Using the ABAP List Viewer (ALV), in course BC401.

A handler method is a class method (static method) or an instance method of an object. If a class method is defined as a handler method, no object of the handled class needs to be instantiated to use the method. Read more in the task, ALV Grid Control – Reacting to a Double-Click, in the lesson, Using the ABAP List Viewer (ALV), in course BC401.

With a BAdI, an SAP application provides the enhancement option through an interface and an adapter class implementing that interface. The interface can be implemented by several users in the delivery chain, thus, multiple BAdI implementations are possible. Read more in this lesson, Describing Business Add-Ins (BAdIs), Describing Business Add-Ins (BAdIs), Task BAdI – Basics, of the course, BC401.

An object of the adapter class is instantiated by the call of the `GET_INSTANCE` static method of the `CL_EXITHANDLER` class. Read more in this lesson, Describing Business Add-Ins (BAdIs), Task The BAdI Calling Program, of the course, BC401.

The name of the BAdl must be investigated before you can implement the BAdI. When the name of the BAdI is determined, the BAdI can be implemented. Read more in this lesson, Describing Business Add-Ins (BAdIs), Task BAdI Implementation, of the course, BC401.

The code of a BAdI implementation is stored in the method of an automatically generated customer class. Read more in this lesson, Describing Business Add-Ins (BAdIs), Task BAdI Implementation, of the course, BC401.

## Unit 8

When you use ADT, you log on to an SAP back-end system and work directly with its repository objects. Therefore, the development process is the same as when you use the ABAP Workbench, there is no check-out and check-in of the objects. Read more in the task, Creation of an ABAP Project, in the lesson, Developing Eclipse- Based ABAP Programs, in course BC401.

There are two types of editors in the ABAP development tools: Editors for which there is a native Eclipse implementation for example, ABAP Editor, an Eclipse editor, to edit ABAP reports or ABAP classes Editors that are displayed in an in-place SAP graphical user interface (GUI), for example, ABAP transaction editor with the classic SAP GUI visualization appearing within the Eclipse environment. Read more in the task, Task Types of Editors, in the lesson, Developing Eclipse-Based ABAP Programs, in course BC401.

Quick Fixes (for example, add missing method implementations), Method Extraction, Deleting Unused Variables, Renaming (for example, rename an attribute, a method, a variable, an interface), Changing Visibility (for example, turning local variables into attributes, making private components public). Read more in the task, Quick Fixes, in the lesson, Developing Eclipse-Based ABAP Programs, in course BC401.

## Unit 9

Class-based exceptions are raised either by the `RAISE EXCEPTION` statement or by the runtime environment. Read more in the task, Use of the `RAISE EXCEPTION` Statement, in the lesson, Explaining Class-Based Exceptions, in course BC401.

You handle the exception using the `CATCH` statement in the `TRY-ENDTRY` structure. Read more in the task, Class-Based Exception Handling, in the lesson, Explaining Class-Based Exceptions, in course BC401.

Read more in the task, Downcasts with Interfaces, in the lesson, Integrating Class Models Using Interfaces, in course BC401.

Like all control structures in ABAP Objects, you can nest `TRY-ENDTRY` structures to any depth. In particular, the `TRY` block, the `CATCH` block, and the `CLEANUP` block can contain complete `TRY-ENDTRY` structures themselves. Read more in the task, Class-Based Exception Handling, in the lesson, Explaining Class-Based Exceptions, in course BC401.

Specify any number of exception classes in the `CATCH` statement. In this way, you define an exception handler for all these exception classes and their subclasses. Read more in the task, Class-Based Exception Handling, in the lesson, Explaining Class-Based Exceptions, in course BC401.

If an exception is raised, the system displays name of the exception class in the Exception Raised field in debugging mode. Read more in the task, Class-Based Exceptions in the Debugger, in the lesson, Explaining Class-Based Exceptions, in course BC401.

To propagate an exception from a procedure, you generally use the `RAISING` addition when you define the procedure interface. Read more in the task, Exception Propagation, in the lesson, Defining and Raising Exceptions, in course BC401.

For subclasses of `CX_NO_CHECK`, you cannot propagate the corresponding exceptions explicitly using the RAISING addition. If you do not handle these exceptions in the processing block where they occur, they are automatically propagated. Read more in the task, The Hierarchy of Predefined Exception Classes, in the lesson, Defining and Raising Exceptions, in course BC401.

Techniques to Handle an Exception Caught in a `CATCH` Statement: Continue the program behind an `ENDTRY` statement, Remove the cause of the error and start again, Raise and propagate one exception. Read more in the task, Exception Handling, in the lesson, efining and Raising Exceptions, in course BC401.

When you handle an exception in a `CATCH` block, use the `RETRY` statement to go back to the `TRY` statement of the respective `TRY-ENDTRY` structure, for example, if the cause for the exception was removed during the handling. Read more in the task, The `RETRY` Statement, in the lesson, Implementing Advanced Exception Handling Techniques, in course BC401.

The exception must be caught with `CATCH` statement using the addition `BEFORE UNWIND`. This ensures that the context of the exception is kept alive for a possible `RESUME`. If the exception is propagated, you must mark it as resumable on all hierarchy levels by using the `RAISING RESUMABLE ( ... )` addition with the name of the exception class inside the brackets. The exception must be raised with the `RAISE RESUMABLE ...` variant of the `RAISE EXCEPTION` statement. Read more in the task, Implementation of Resumable Exceptions, in the lesson, Implementing Advanced Exception Handling Techniques, in course BC401.

## Unit 10

The `SETUP` method is an Instance method, which is executed before every single test method of the class. Read more in the task, Text Fixtures, in the lesson, Unit Testing with ABAP Unit, in course BC401.

Typically, test coding is generated only in development and test systems. In production systems, test classes are excluded from the load versions to minimize the size of the loads. Each test method is executed independently. Read more in the tasks, Unit Testing and Generation of Test Code, in the lesson, Unit Testing with ABAP Unit, in course BC401.

## Unit 11

In an abstract class, you can define abstract methods among other things. This means that the abstract method cannot be implemented in that class. Instead, it is implemented in a subclass of the class. If the subclass of that class is not abstract, the abstract methods must be redefined and implemented in the subclass Static methods cannot be abstract because they cannot be redefined. Read more in the task, Abstract Classes, in the lesson, Implementing Advanced Object-Oriented Techniques, in course BC401.

Prevent a class from being inherited by using the `FINAL` addition with the class statement. Read more in the task, Final Classes, in the lesson, Implementing Advanced Object-Oriented Techniques, in course BC401.

Associations like aggregations and compositions are an important design principle of object-oriented programming. An association means that at runtime, an instance of one class stores references to objects of another class. Read more in the task, Navigation Methods and Chain Method Calls, in the lesson, Implementing Advanced Object-Oriented Techniques, in course BC401.

If the instance constructor is protected, the visibility is extended to all of its subclasses, that is, the subclasses can also create instances of the class. Read more in the task, Visibility of the Instance Constructor, in the lesson, Implementing Advanced Object-Oriented Techniques, in course BC401.

A factory method can have coding that executes before the actual instantiation. The method can perform checks, read data from the database, or set locks, before actually creating the instance. Classes can have more than one factory method. Each of the methods can have a different implementation and signature as well. The different factory method makes it possible to create instances of the same class based on different information. Read more in the task, Advantages of factory methods, in the lesson, Implementing the Singleton Pattern, in course BC401.

In some cases, classes have to work together so closely that one class needs access to the other classes protected and private components. Or, one class might need to create instances of the other class regardless of the visibility of the constructor. In this case, use the concept of class friendship. Read more in the task, Definition of Friendship Relationships, in the lesson, Implementing Factory Classes Using Friendship, of course BC401.

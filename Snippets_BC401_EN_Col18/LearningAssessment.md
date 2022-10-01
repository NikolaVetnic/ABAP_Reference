# Learning Assessment

## Unit 1

1. Data and functions are kept separate in the procedural programming model. **Answer:** true.

2. What does multiple instantiation mean? **Answer:** creating and managing runtime instances.

3. Which of the following is a simplification of complex relationships in the real world? **Answer:** abstraction.

4. Which kind of UML diagram pays particular attention to the sequence in which objects relate to each other? **Answer:** behavior.

5. You can group all characteristics and behaviors of similar objects into one central class. **Answer:** true.

## Unit 2

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

### In More Detail...

#### Unit 1

The procedural programming model allows separation of data and functions. In an ABAP, procedural program, global variables contain data, while subroutines contain functions. Read more in the task, The Procedural Programming Model, in the lesson, Introduction to Object-Oriented Programming, in course BC401.

You are correct! Multiple Instantiation is the possibility to create several runtime instances using the same program context. It is one of the key characteristics of object-oriented programming. Read more in the task, Multiple Instantiation in Object-Oriented Programming, in the lesson, Introduction to Object-Oriented Programming, in course BC401.

Unlike in procedural programming, using multiple instantiation in object- oriented programming allows you to create a direct abstraction of a real object. Objects are a direct abstraction of the real world. Read more in the task, Object-Oriented ABAP, in the lesson, Introduction to Object-Oriented Programming, in the course, BC401.

The different diagram types in UML represent different views of a system. Behavior diagrams show the sequence in which the objects relate to each other. Read more in the task, Class Diagrams, in the lesson, Analyzing and Designing with Unified Modeling Language (UML), in the course, BC401.

Object-oriented programming views the real world as a collection of objects. Some of these objects are similar and can be described in the same way if they use the same characteristics and exhibit the same behavior. You can group all the characteristics and behaviors of these similar objects into one central class. Read more in the task, Classification of Objects, in the lesson, Analyzing and Designing with Unified Modeling Language (UML), in the course, BC401.

#### Unit 2

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

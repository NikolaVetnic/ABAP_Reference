## Unit Testing with ABAP Unit

Unit tests are written as local classes within the main program of a global class (class pool). They act as users of the class, and as such, they may only access its public components. Each test is implemented as a method of the class, the aim being to perform remove operations on the class under test that lead to a particular verifiable state of the class. Each test method is executed independently.

**ABAP Unit** is a unit-testing framework that is integrated into the ABAP language. ABAP Unit tests are transported with the ABAP repository objects that they test. 

Advantages of unit testing:
* define your tests in ABAP - no need to learn a test script language
* easy testing while you develop - test tool integrated in ABAP Workbench and ADT
* mass testing - unit test integrated in Code Inspector and ABAP Test Cockpit
* easy execution and evaluation - anybody can run a test without ABAP knowledge
* integrated test coverage - evaluate which percentage of your code is covered by your tests
* no resource consumption in productive systems - decide in which systems test coding is generated and loaded
* test classification with regard for time consumption and potential damage - identify time consuming and critical tests

### Definition of a Unit Test Class

ABAP Unit tests are realized as local test classes with the required test methods in your application programs. You define local test classes by adding the `FOR TESTING` addition in the class definition. Further additions are used to classify the tests according to their time consumption and the impact on the data integrity.

Definition of ABAP Unit Test classes:
```ABAP
	CLASS lcl_mytests DEFINITION
					FOR TESTING " test class definition
					DURATION ... " time consumption (LOW, MEDIUM or HIGH)
					RISK LEVEL ... " risk for sys data (HARMLESS, CRITICAL, DANGEROUS)

		PRIVATE SECTION.
			METHODS my_test_1 FOR TESTING.  " test method definition
			METHODS my_test_2 FOR TESTING.  " - instance method
			METHODS my_test_3 FOR TESTING.  " - no parameters
			... (additional helper methods) " - preferably private
	ENDCLASS.
```

Test methods are defined as instance methods without parameters (preferably private) in your local test classes. Like a test class, their definition also requires the `FOR TESTING` addition.

A test method contains program code to be executed for testing, followed by a check of the results derived from the test code. The content of the variables to be tested is compared against expected values by calling standard check methods of service class `CL_ABAP_UNIT_ASSERT` (static methods `ASSERT_...`). If the respective values do not correspond to the expectation, the check method can enter a note, a warning, or even an error in the test log.

Implementation of ABAP Unit Test methods:
```ABAP
	CLASS lcl_mytests DEFINITION FOR TESTING ...
		PRIVATE SECTION.
			METHODS my_test_1 FOR TESTING.
			METHODS my_test_2 FOR TESTING.
	ENDCLASS.
	" =-=-=-=
	CLASS lcl_mytests IMPLEMENTATION.
		METHOD my_test_1.
			* use a function of the productive code
			...
			* assert correctness of the result
			cl_abap_unit_assert=>assert_...( ... ).
		ENDMETHOD.
		...
	ENDCLASS.
```

Assistance class (performsm checks, writes protocol of failed checks):
|`CL_ABAP_UNIT_ASSERT`|
|--|
|`assert_intial()`|
|`assert_not_initial()`|
|`assert_bound()`|
|`assert_not_bound()`|
|`assert_equals()`|
|`assert_differs()`|
|`assert_subrc()`|

Assertion method import parameters:
|Parameter|Description|
|--|--|
|`act`|variable to be checked|
|`exp`|expected value (methods `assert_equals`, `assert_differs`)|
|`msg`|message to be displayed in the log if the value does not match the expectation|
|`lev`|gravity of error to be logged (class constants `TOLERABLE`, `CRITICAL`, `FATAL`)|
|`quit`|what is terminated if test fails (class constants `NO`, `METHOD`, `CLASS`, `PROGRAM`|

Check methods also have a return parameter, `assertion_failed` (value = `’X’` /`SPACE`). This can be used by the test method to find out the check results.

### ABAP Unit Test Execution

In ABAP Workbench:
* for a Program, on the menu bar choose *Program → Execute → Unit Tests*
* or a Class, on the menu bar choose *Class → Run → Unit Tests*
* in the Object Name screen area, right-click the item name and choose *Execute → Unit Tests*

In ADT:
* on the Project Explorer menu bar, choose *Run → Run As → ABAP Unit Tests*

Tools for mass testing:
* in the ABAP Test Cockpit (ATC), use check profile with checks `BC_AUNIT`, `BC_AUNIT_M`, or `BC_AUNIT_L`
* in the Code Inspector (SCI), use check variant with check List of *Checks → Dynamic Tests → ABAP Unit*

### Test Fixtures

A fixture is a test configuration that is created before a test method is called, and ensures a unique test behavior. A fixture consists of test data, test objects, resources, and connections.

To create a fixture, implement the following private and parameterless methods in a test class:
|Methods|Description|
|--|--|
|`SETUP`|instance method, which is executed before every single test method of the class|
|`TEARDOWN`|instance method, which is executed after every single test method of the class|
|`CLASS_SETUP`|static method, which is executed once before the first test method of the class|
|`CLASS_TEARDOWN`|static method, which is executed once after the last test meth- od of the class|

Phase model of unit test and fixture methods:
|1|start internal session for test class|
|2|class construction of test class|
|3|setup of class fixture (method `CLASS_SETUP`)|
|4a|for each test method: create test class instance|
|4b|for each test method: setup of fixture (method `SETUP`)|
|4c|for each test method: execution of test method|
|4d|for each test method: teardown of fixture (method `TEARDOWN`)|
|5|teardown of class fixture (method `CLASS_TEARDOWN`)|

### Test Coverage

To run ABAP Unit tests with code coverage measurement, choose *Execute → Unit Tests With → Coverage* from the development object’s context menu in the navigation area (ABAP Workbench) or *Coverage As → ABAP Unit Test* from the context menu in the Project Explorer (ADT).

ABAP Development Tools display the coverage in the source code editor as follows:
* statement coverage: every executable line is highlighted either green or red. Green indicates that the statement was exceeded. Red indicates that the line was not executed
* branch coverage: branches in your code are highlighted red if they were not executed. Branches are highlighted green if they were run and resolved to both true and false. Branches are highlighted yellow if they were run but resolved only to one condition, true or false
* procedure coverage: procedure entry points are highlighted green or red to indicate whether they were called or not

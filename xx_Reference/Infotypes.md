# Infotypes

[An infotype](https://www.techopedia.com/definition/28728/infotype-sap#:~:text=Techopedia%20Explains%20Infotype-,What%20Does%20Infotype%20Mean%3F,capable%20of%20maintaining%20employee%20data.) is an information unit used to maintain master data related to SAP human resource management systems (HRMS). An infotypes contains a four-digit code with a related name and is capable of maintaining employee data.

## Use

Recording employee data for administrative, time recording, and payroll purposes is of primary importance for master data administration in HR. In the SAP System, the information units used to enter master data are called infotypes .

Infotypes are used to group related data fields together. They provide information with a structure, facilitate data entry, and enable you to store data for specific periods.

## Structure

Infotypes are characterized by the following:
* infotype structure
* data entry
* time-dependent storage of infotype data

### Infotype Structure

To the user, infotypes appear as data entry screens. They contain whole series of information (for example, last name, first name, date of birth) that you enter in data fields. Data fields concerning the same or similar subject matter are combined into data groups or information units.

In database terms, infotypes represent a **data structure** or **set of related data records**. When you update an infotype, old data is not lost but is instead stored in the system for historical evaluation purposes.

### Data Entry

You perform data entry as follows:
* your entries are automatically checked for accuracy and against table entries
* predefined default values help you to enter and maintain data
* checks and default values depend on the employee’s organizational assignment; the organizational assignment determines the relevant information used from time recording, wage type and pay scale structure

### Time-Dependent Storage of Infotype Data

When you update an infotype, the old data may not be lost. Instead, it must be retained so that past data can be evaluated. When you update an employee’s personal data, the old data is automatically time-delimited. The system creates a validity period for each infotype record. As a result, each employee infotype has several data records that differ from each other by their validity periods.

You must also define how the various data records of one infotype interact with each other with respect to their validity periods. This time-based reaction is determined by the infotype [Time Constraint](https://help.sap.com/docs/HR_RENEWAL/4946a4f5c2d7427c96d89242e1ff2d9a/cd88dd53c88c424de10000000a174cb4.html).

### Assigning Infotypes to Countries

You can define the permissibility of infotypes for each country, which will restrict the selection of infotypes available when you edit master data.

E.g. infotype **Fiscal Data D** (`0012`) should only be displayed for employees assigned to country grouping Germany.

## Displaying a Complete List of Infotype Records

Steps to take in order to display a complete list of infotype records:
1. from the SAP Easy Access screen, select **Human Resources /  Personnel Management / Administration / HR Master Data / Display**.
2. this takes you to the initial display screen for HR master data
3. enter the employee’s personnel number
4. select either the infotype you want from the infotype selection list or directly enter the name/number of the infotype whose available records you want to display in the field **Infotype**
5. if you do not know the exact name or number of the infotype records you want to display, and if it does not appear in the infotype selection list, please read the [Selecting HR Data](https://help.sap.com/docs/SAP_S4HANA_ON-PREMISE/c6c3ffd90792427a9fee1a19df5b0925/5137e153a217424de10000000a174cb4.html) section
6. Select **Goto / Maintain overview**

## Select Human Resource Data

Features:
* a large amount of data in the Human Resource Management System is stored in [infotypes](https://help.sap.com/docs/SAP_S4HANA_ON-PREMISE/c6c3ffd90792427a9fee1a19df5b0925/dd72dd538636424de10000000a174cb4.html). To access data on an employee, you must enter the employee’s personnel number and the infotype whose data records you want to process
* you can find personnel numbers, even if you do not know them exactly, by using the [Search Help for Personnel Numbers](https://help.sap.com/docs/SAP_S4HANA_ON-PREMISE/c6c3ffd90792427a9fee1a19df5b0925/5437e153a217424de10000000a174cb4.html)
* after you have found the employee you require, you can access this employee’s infotype records as follows:
	* selecting a known infotype - each infotype has both a name and a number, if you know either of these, you can enter it directly
	* selecting an infotype using functional areas - the Human Resource Management System contains functional areas that group infotypes together by subject matter (e.g. personal data, payroll data), from the functional area, you can select the infotype you want to process
	* selecting an infotype using the text search - if you enter a search term in the Infotype field, the system displays a selection menu listing all of the infotypes whose name includes the search term
	* selecting an infotype using the personnel file - the Personnel File is a reference tool that you can use to view all of the information stored in the system for a particular employee

## Creating an Infotype

**Create Infotype** tcode is `PM01`. To create an infotype follow the steps as stated [here](https://wiki.scn.sap.com/wiki/display/TechTSG/Steps+to+Customize+infotypes):
1. go to `PM01`
2. enter the custom infotype (IT for short) number which you want to create - it should be a 4 digit number and start with `9` (for my task I chose `9900`)
3. select the `Employee Infotype` radio button
4. select the `PS Structure Infotype` radio button
5. click on **Create** - a separate table maintenance window appears
6. create a PS structure with all the fields you want on the IT
7. save and activate the PS structure
8. go back to the initial screen of `PM01`
9. click on **All** push button - it takes a few moments (*this step I have skipped as I couldn't find the **All** button*)
10. click on **Technical Characteristics** button - IT list screen appears
11. click on **Edit** button (upper right)
12. select the newly created IT and click on **Details** button (the one with the magnifying glass)
13. type in `SUBTY` as subtype field, `T591A` as subtype table and `T591S` as subtype text tab - save and return to `PM01` screen (**Back** button in upper left)
14. click on **Infotype Characteristics** button - IT list screen appears
15. click on **Edit** button
16. click on **New Entries** button, enter IT number and short text
17. here we have to set different IT characteristics as per the requirement (it's best to open another session with some standard Infotype's infotype characteristics screen and use as the reference to fill yours), how I filled it out can be seen [here](http://nikolapacekvetnic.rs/wp-content/uploads/2022/10/Screenshot-2022-10-21-at-09.43.14.jpg)
18. save all entries - now the IT is created and ready to use

If you want to change the layout of the IT as per your requirement:
1. go to `PM01` and on initial screen select **Screen** radio button and type in `2000` as the screen name, and then click on **Edit**
2. select `Layout Editor` and click **Change** - the default screen layout is now displayed (example of mine can be seen [here](http://nikolapacekvetnic.rs/wp-content/uploads/2022/10/Screenshot-2022-10-21-at-11.16.29.jpg)) and can be modified as ones sees fit

It's important not to forget to save and activate at every level.

ITs cannot be created locally so they have to be in one package or other. The way I found them in Eclipse was to explore different subobjects from the initial screen of `PM01` and learn their names, then within Eclipse I did RC on **Favorite Objects**, then **Add Object** and then I would search for the names found in `PM01` transaction. For example, IT `9900` yielded following objects:
* `MP990000`, `MP990010`, `MP990020`, `MP990030`, `MP990040`, `MP990050`
* `ZHCMT_BSPA_PA_XX_R9900` - screen structure for infotype `9900`
* `ZCL_HRPA_INFOTYPE_9900` - check class for infotype `9900`


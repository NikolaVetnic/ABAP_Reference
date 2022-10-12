```ABAP
CLASS znv_cl_task1_employee_dao DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          ip_emplid TYPE znv_task1_employeeid
          ip_fname  TYPE znv_task1_firstname
          ip_lname  TYPE znv_task1_lastname
          ip_dob    TYPE znv_task1_dob
          ip_addr   TYPE znv_task1_address
          ip_gender TYPE znv_task1_gender
          ip_dept   TYPE znv_task1_department
          ip_salary TYPE znv_task1_salary,

      get_employee_as_row RETURNING VALUE(r_result) TYPE znv_task1_empl,

      get_d_emplid RETURNING VALUE(r_result) TYPE znv_task1_employeeid,
      get_d_fname RETURNING VALUE(r_result) TYPE znv_task1_firstname,
      set_d_fname IMPORTING i_d_fname TYPE znv_task1_firstname,
      get_d_lname RETURNING VALUE(r_result) TYPE znv_task1_lastname,
      set_d_lname IMPORTING i_d_lname TYPE znv_task1_lastname,
      get_d_dob RETURNING VALUE(r_result) TYPE znv_task1_dob,
      set_d_dob IMPORTING i_d_dob TYPE znv_task1_dob,
      get_d_addr RETURNING VALUE(r_result) TYPE znv_task1_address,
      set_d_addr IMPORTING i_d_addr TYPE znv_task1_address,
      get_d_gender RETURNING VALUE(r_result) TYPE znv_task1_gender,
      set_d_gender IMPORTING i_d_gender TYPE znv_task1_gender,
      get_d_dept RETURNING VALUE(r_result) TYPE znv_task1_department,
      set_d_dept IMPORTING i_d_dept TYPE znv_task1_department,
      get_d_salary RETURNING VALUE(r_result) TYPE znv_task1_salary,
      set_d_salary IMPORTING i_d_salary TYPE znv_task1_salary.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      d_emplid TYPE znv_task1_employeeid,
      d_fname  TYPE znv_task1_firstname,
      d_lname  TYPE znv_task1_lastname,
      d_dob    TYPE znv_task1_dob,
      d_addr   TYPE znv_task1_address,
      d_gender TYPE znv_task1_gender,
      d_dept   TYPE znv_task1_department,
      d_salary TYPE znv_task1_salary.
ENDCLASS.



CLASS znv_cl_task1_employee_dao IMPLEMENTATION.

  METHOD constructor.
    d_emplid = ip_emplid.
    d_fname = ip_fname.
    d_lname = ip_lname.
    d_dob = ip_dob.
    d_addr = ip_addr.
    d_gender = ip_gender.
    d_dept = ip_dept.
    d_salary = ip_salary.
  ENDMETHOD.

  METHOD get_employee_as_row.
    r_result-employeeid = d_emplid.
    r_result-firstname = d_fname.
    r_result-lastname = d_lname.
    r_result-dob = d_dob.
    r_result-address = d_addr.
    r_result-gender = d_gender.
    r_result-department = d_dept.
    r_result-salary = d_salary.
  ENDMETHOD.

  METHOD get_d_emplid.
    r_result = me->d_emplid.
  ENDMETHOD.

  METHOD get_d_fname.
    r_result = me->d_fname.
  ENDMETHOD.

  METHOD set_d_fname.
    me->d_fname = i_d_fname.
  ENDMETHOD.

  METHOD get_d_lname.
    r_result = me->d_lname.
  ENDMETHOD.

  METHOD set_d_lname.
    me->d_lname = i_d_lname.
  ENDMETHOD.

  METHOD get_d_dob.
    r_result = me->d_dob.
  ENDMETHOD.

  METHOD set_d_dob.
    me->d_dob = i_d_dob.
  ENDMETHOD.

  METHOD get_d_addr.
    r_result = me->d_addr.
  ENDMETHOD.

  METHOD set_d_addr.
    me->d_addr = i_d_addr.
  ENDMETHOD.

  METHOD get_d_gender.
    r_result = me->d_gender.
  ENDMETHOD.

  METHOD set_d_gender.
    me->d_gender = i_d_gender.
  ENDMETHOD.

  METHOD get_d_dept.
    r_result = me->d_dept.
  ENDMETHOD.

  METHOD set_d_dept.
    me->d_dept = i_d_dept.
  ENDMETHOD.

  METHOD get_d_salary.
    r_result = me->d_salary.
  ENDMETHOD.

  METHOD set_d_salary.
    me->d_salary = i_d_salary.
  ENDMETHOD.

ENDCLASS.
```

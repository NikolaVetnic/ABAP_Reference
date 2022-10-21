```ABAP
CLASS znvcl_task1_employee DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      db_index,
      db_get
        IMPORTING ip_emplid      TYPE znv_task1_employeeid
        RETURNING VALUE(rt_empl) TYPE znv_task1_empl,
      db_show
        IMPORTING ip_emplid TYPE znv_task1_employeeid,
      db_destroy
        IMPORTING ip_emplid      TYPE znv_task1_employeeid
        RETURNING VALUE(rt_empl) TYPE znv_task1_empl,
      db_exists_record
        IMPORTING ip_emplid        TYPE znv_task1_employeeid
        RETURNING VALUE(rt_exists) TYPE abap_bool.

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

      get_empl
        RETURNING VALUE(rt_empl) TYPE znv_task1_empl,

      db_update,

      get_emplid
        RETURNING VALUE(rt_emplid) TYPE znv_task1_employeeid,
      get_fname
        RETURNING VALUE(rt_fname) TYPE znv_task1_firstname,
      get_lname
        RETURNING VALUE(rt_lname) TYPE znv_task1_lastname,
      get_dob
        RETURNING VALUE(rt_dob) TYPE znv_task1_dob,
      get_addr
        RETURNING VALUE(rt_addr) TYPE znv_task1_address,
      get_gender
        RETURNING VALUE(rt_gender) TYPE znv_task1_gender,
      get_dept
        RETURNING VALUE(rt_dept) TYPE znv_task1_department,
      get_salary
        RETURNING VALUE(rt_salary) TYPE znv_task1_salary,

      set_emplid
        IMPORTING ip_emplid TYPE znv_task1_employeeid,
      set_fname
        IMPORTING ip_fname TYPE znv_task1_firstname,
      set_lname
        IMPORTING ip_lname TYPE znv_task1_lastname,
      set_dob
        IMPORTING ip_dob TYPE znv_task1_dob,
      set_addr
        IMPORTING ip_addr TYPE znv_task1_address,
      set_gender
        IMPORTING ip_gender TYPE znv_task1_gender,
      set_dept
        IMPORTING ip_dept TYPE znv_task1_department,
      set_salary
        IMPORTING ip_salary TYPE znv_task1_salary.

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



CLASS znvcl_task1_employee IMPLEMENTATION.
  METHOD db_index.
    " =-=-=-= get all db entries
    SELECT employeeid, firstname, lastname, dob, address, gender, department, salary, currency
    FROM znv_task1_empl
    INTO TABLE @DATA(employees).

    cl_salv_table=>factory(
      IMPORTING r_salv_table = DATA(alv_table)
      CHANGING t_table = employees ).
    alv_table->display( ).
  ENDMETHOD.

  METHOD db_get.
    " =-=-=-= get db entry by id
    SELECT employeeid, firstname, lastname, dob, address, gender, department, salary, currency
    FROM znv_task1_empl
    INTO CORRESPONDING FIELDS OF @rt_empl
    WHERE employeeid = @ip_emplid.
    ENDSELECT.

    IF sy-subrc <> 0.
      MESSAGE e004(znv_task1_employee).
    ENDIF.
  ENDMETHOD.

  METHOD db_show.
    " =-=-=-= display db entry by id as alv
    SELECT employeeid, firstname, lastname, dob, address, gender, department, salary, currency
    FROM znv_task1_empl
    INTO TABLE @DATA(employee_sel)
    WHERE employeeid = @ip_emplid.

    IF sy-subrc <> 0.
      MESSAGE i005(znv_task1_employee) WITH ip_emplid.
    ELSE.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = DATA(alv_table)
        CHANGING t_table = employee_sel ).
      alv_table->display( ).
    ENDIF.
  ENDMETHOD.

  METHOD db_destroy.
    " =-=-=-= remove db entry by id and return it
    rt_empl = db_get( ip_emplid ).
    DELETE znv_task1_empl FROM rt_empl.

    IF sy-subrc = 0.
      MESSAGE i002(znv_task1_employee).
    ELSE.
      MESSAGE e004(znv_task1_employee).
    ENDIF.
  ENDMETHOD.

  METHOD db_exists_record.
    SELECT COUNT( * )
    FROM znv_task1_empl
    INTO @DATA(count)
    UP TO 1 ROWS
    WHERE employeeid = @ip_emplid.

    IF count = 1.
      rt_exists = abap_true.
    ELSE.
      rt_exists = abap_false.
    ENDIF.
  ENDMETHOD.



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



  METHOD get_empl.
    rt_empl-employeeid = d_emplid.
    rt_empl-firstname = d_fname.
    rt_empl-lastname = d_lname.
    rt_empl-dob = d_dob.
    rt_empl-address = d_addr.
    rt_empl-gender = d_gender.
    rt_empl-department = d_dept.
    rt_empl-salary = d_salary.
  ENDMETHOD.



  METHOD db_update.
    " =-=-=-= create or update a db entry
    DATA(tmp) = get_empl( ).
    MODIFY znv_task1_empl FROM tmp.

    IF sy-subrc = 0.
      MESSAGE i001(znv_task1_employee).
    ELSE.
      MESSAGE e004(znv_task1_employee).
    ENDIF.
  ENDMETHOD.



  METHOD get_emplid.
    rt_emplid = d_emplid.
  ENDMETHOD.

  METHOD get_fname.
    rt_fname = d_fname.
  ENDMETHOD.

  METHOD get_lname.
    rt_lname = d_lname.
  ENDMETHOD.

  METHOD get_dob.
    rt_dob = d_dob.
  ENDMETHOD.

  METHOD get_addr.
    rt_addr = d_addr.
  ENDMETHOD.

  METHOD get_gender.
    rt_gender = d_gender.
  ENDMETHOD.

  METHOD get_dept.
    rt_dept = d_dept.
  ENDMETHOD.

  METHOD get_salary.
    rt_salary = d_salary.
  ENDMETHOD.



  METHOD set_emplid.
    d_emplid = ip_emplid.
  ENDMETHOD.

  METHOD set_fname.
    d_fname = ip_fname.
  ENDMETHOD.

  METHOD set_lname.
    d_lname = ip_lname.
  ENDMETHOD.

  METHOD set_dob.
    d_dob = ip_dob.
  ENDMETHOD.

  METHOD set_addr.
    d_addr = ip_addr.
  ENDMETHOD.

  METHOD set_gender.
    d_gender = ip_gender.
  ENDMETHOD.

  METHOD set_dept.
    d_dept = ip_dept.
  ENDMETHOD.

  METHOD set_salary.
    d_salary = ip_salary.
  ENDMETHOD.
ENDCLASS.
```

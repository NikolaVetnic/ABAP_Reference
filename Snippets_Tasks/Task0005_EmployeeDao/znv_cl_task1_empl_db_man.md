```ABAP
CLASS znv_cl_task1_empl_db_man DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      index,
      get
        IMPORTING ip_emplid      TYPE znv_task1_employeeid
        RETURNING VALUE(rt_empl) TYPE znv_task1_empl,
      show
        IMPORTING ip_emplid TYPE znv_task1_employeeid,
      update
        IMPORTING ip_empl TYPE znv_task1_empl,
      destroy
        IMPORTING ip_emplid      TYPE znv_task1_employeeid
        RETURNING VALUE(rt_empl) TYPE znv_task1_empl,
      exists_record
        IMPORTING ip_emplid        TYPE znv_task1_employeeid
        RETURNING VALUE(rt_exists) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS znv_cl_task1_empl_db_man IMPLEMENTATION.
  METHOD index.
    " =-=-=-= get all db entries
    SELECT employeeid, firstname, lastname, dob, address, gender, department, salary, currency
    FROM znv_task1_empl
    INTO TABLE @DATA(employees).

    cl_salv_table=>factory(
      IMPORTING r_salv_table = DATA(alv_table)
      CHANGING t_table = employees ).
    alv_table->display( ).
  ENDMETHOD.

  METHOD get.
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

  METHOD show.
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

  METHOD update.
    " =-=-=-= create or update a db entry
    MODIFY znv_task1_empl FROM ip_empl.

    IF sy-subrc = 0.
      MESSAGE i001(znv_task1_employee).
    ELSE.
      MESSAGE e004(znv_task1_employee).
    ENDIF.
  ENDMETHOD.

  METHOD destroy.
    " =-=-=-= remove db entry by id and return it
    rt_empl = get( ip_emplid ).
    DELETE znv_task1_empl FROM rt_empl.

    IF sy-subrc = 0.
      MESSAGE i002(znv_task1_employee).
    ELSE.
      MESSAGE e004(znv_task1_employee).
    ENDIF.
  ENDMETHOD.

  METHOD exists_record.
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

ENDCLASS.
```

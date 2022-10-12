```ABAP
REPORT znv_employee_man_w_lcl_dao.

SELECTION-SCREEN BEGIN OF BLOCK action WITH FRAME TITLE TEXT-002.
  PARAMETERS:
    p_dis TYPE abap_bool RADIOBUTTON GROUP act DEFAULT 'X' USER-COMMAND dis,
    p_shw TYPE abap_bool RADIOBUTTON GROUP act,
    p_upd TYPE abap_bool RADIOBUTTON GROUP act,
    p_rem TYPE abap_bool RADIOBUTTON GROUP act.
SELECTION-SCREEN END OF BLOCK action.

SELECTION-SCREEN BEGIN OF BLOCK employee WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    p_emplid TYPE znv_task1_employeeid MODIF ID eid,
    p_fname  TYPE znv_task1_firstname LOWER CASE MODIF ID fnm,
    p_lname  TYPE znv_task1_lastname LOWER CASE MODIF ID lnm,
    p_dob    TYPE znv_task1_dob MODIF ID dob,
    p_addr   TYPE znv_task1_address LOWER CASE MODIF ID adr,
    p_gender TYPE znv_task1_gender MODIF ID gnd,
    p_dept   TYPE znv_task1_department MODIF ID dpt,
    p_salary TYPE znv_task1_salary MODIF ID sal.
SELECTION-SCREEN END OF BLOCK employee.



AT SELECTION-SCREEN OUTPUT.
  IF p_dis = abap_true.
    LOOP AT SCREEN.
      IF screen-group1 = 'EID' OR
         screen-group1 = 'FNM' OR
         screen-group1 = 'LNM' OR
         screen-group1 = 'DOB' OR
         screen-group1 = 'ADR' OR
         screen-group1 = 'GND' OR
         screen-group1 = 'DPT' OR
         screen-group1 = 'SAL'.
        screen-input = '0'.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSEIF p_shw = abap_true OR p_rem = abap_true.
    LOOP AT SCREEN.
      IF screen-group1 = 'FNM' OR
         screen-group1 = 'LNM' OR
         screen-group1 = 'DOB' OR
         screen-group1 = 'ADR' OR
         screen-group1 = 'GND' OR
         screen-group1 = 'DPT' OR
         screen-group1 = 'SAL'.
        screen-input = '0'.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.



CLASS lcl_empl_dao DEFINITION.
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
      get_employee_as_row
        RETURNING VALUE(r_result) TYPE znv_task1_empl.
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

CLASS lcl_empl_dao IMPLEMENTATION.
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
ENDCLASS.



START-OF-SELECTION.


IF p_shw = abap_true.
  znv_cl_task1_empl_db_man=>show( ip_emplid = p_emplid ).
ELSEIF p_upd = abap_true.
  znv_cl_task1_empl_db_man=>update(
    ip_empl = NEW lcl_empl_dao(
      ip_emplid = p_emplid
      ip_fname  = p_fname
      ip_lname  = p_lname
      ip_dob    = p_dob
      ip_addr   = p_addr
      ip_gender = p_gender
      ip_dept   = p_dept
      ip_salary = p_salary
    )->get_employee_as_row( )
  ).
ELSEIF p_rem = abap_true.
  znv_cl_task1_empl_db_man=>destroy( ip_emplid = p_emplid ).
ELSEIF p_dis = abap_true.
  znv_cl_task1_empl_db_man=>index( ).
ENDIF.
```

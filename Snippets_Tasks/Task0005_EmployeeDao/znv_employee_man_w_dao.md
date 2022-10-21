```ABAP
*&---------------------------------------------------------------------*
*& Report znv_employee_man_w_dao
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT znv_employee_man_w_dao.

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



START-OF-SELECTION.

  IF p_shw = abap_true.
    znv_cl_task1_empl_db_man=>show( ip_emplid = p_emplid ).
  ELSEIF p_upd = abap_true.
    znv_cl_task1_empl_db_man=>update(
      ip_empl = NEW znv_cl_task1_employee_dao(
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

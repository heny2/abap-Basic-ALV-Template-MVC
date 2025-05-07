*&---------------------------------------------------------------------*
*& Report ZBCD_TOOLS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbcd_tools.

*data:
*  begin of out occurs 0,
*    line type char200,
*  end of out.
*
*
*selection-screen begin of block b4 with frame title text-t06.
*  parameters:
*  p_prog  type trdir-name.
*selection-screen end of block b4.
*
**&---------------------------------------------------------------------*
**& Form frm_process_prog
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
*form frm_process_prog.
*
*  check p_prog is not initial.
*
*  read report p_prog into out.
*  editor-call for out.
*  insert report p_prog from out.
*
*endform.

SELECTION-SCREEN BEGIN OF BLOCK bl01 WITH FRAME TITLE TEXT-pnn.

  PARAMETERS: p_prog TYPE progname.
  PARAMETERS: p_1 TYPE char50.        " methods

SELECTION-SCREEN END OF BLOCK bl01.


TYPES: BEGIN OF ty_alv,
         a TYPE char40,
         b TYPE char120,
       END OF ty_alv.

DATA:lt_data TYPE STANDARD TABLE OF ty_alv,
     ls_data TYPE ty_alv.

REFRESH: lt_data.

DATA: BEGIN OF out OCCURS 0,
        line TYPE text200,
      END OF out.

START-OF-SELECTION.

  DATA:lv_str TYPE char100.
  lv_str = p_prog && '%'.

  SELECT *
    INTO TABLE @DATA(lt_cx)
    FROM trdir
   WHERE name LIKE @lv_str.

  LOOP AT lt_cx INTO DATA(ls_cx).
    CLEAR out[].
    TRY.
        READ REPORT ls_cx-name INTO out[].
      CATCH cx_sy_read_src_line_too_long.
        CONTINUE.
    ENDTRY.

    READ TABLE out INDEX 1.
    IF p_1 IS NOT INITIAL .
      ls_data-a = ls_cx-name.
      ls_data-b = out.
      IF ls_data-b CS p_1.
        APPEND ls_data TO lt_data.
      ENDIF.
    ELSE.
      ls_data-a = ls_cx-name.
      ls_data-b = out.
      APPEND ls_data TO lt_data.
    ENDIF.
  ENDLOOP.
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
  DATA lt_fieldcat TYPE slis_t_fieldcat_alv.
  DATA ls_fieldcat TYPE slis_fieldcat_alv.

  DATA lw_layout   TYPE slis_layout_alv.

  DEFINE add_fieldcat.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname  = &1.
    ls_fieldcat-seltext_l  = &2.
    APPEND ls_fieldcat TO lt_fieldcat.
    CLEAR ls_fieldcat.
  END-OF-DEFINITION.

  add_fieldcat 'A'      '程序名'.
  add_fieldcat 'B'      '代码'.

  lw_layout-colwidth_optimize = abap_on.
  DATA: w_repid LIKE sy-repid.
  w_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = w_repid "定义回执程序
      is_layout               = lw_layout
      it_fieldcat             = lt_fieldcat
      i_callback_user_command = 'ALV_USER_COMMAND' "定义执行事件
    TABLES
      t_outtab                = lt_data
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

FORM alv_user_command USING r_ucomm     LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.

  DATA: BEGIN OF out OCCURS 0,
          line TYPE text200,
        END OF out.
  CASE r_ucomm.
    WHEN '&IC1'.
      READ TABLE lt_data INTO ls_data INDEX rs_selfield-tabindex.
      IF sy-subrc = 0 .
        CHECK ls_data-a IS NOT INITIAL.
        READ REPORT ls_data-a INTO out.
        EDITOR-CALL FOR out.
        INSERT REPORT ls_data-a FROM out.
      ENDIF.
    WHEN OTHERS .
  ENDCASE .
ENDFORM.

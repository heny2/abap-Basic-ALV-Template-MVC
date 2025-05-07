*&---------------------------------------------------------------------*
*& Report ZDBMT
*&---------------------------------------------------------------------*
REPORT zbcd_table_sm30 MESSAGE-ID db.

*&---------------------------------------------------------------------*
*& Report ZSRM_MAINTAIN_TABLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

TYPE-POOLS: icon.

TABLES: sscrfields.
CONSTANTS: c_count TYPE i VALUE 1000.

DATA: go_dock TYPE REF TO cl_gui_docking_container,
      go_cont TYPE REF TO cl_gui_container,
      go_grid TYPE REF TO cl_gui_alv_grid,
      gs_layo TYPE lvc_s_layo,
      gs_fcat TYPE lvc_s_fcat,
      gt_fcat TYPE lvc_t_fcat.

DATA: g_table TYPE dd02l-tabname,
      g_zshow TYPE c,
      g_lines TYPE i.

FIELD-SYMBOLS: <table> TYPE STANDARD TABLE. " Dynamic TABLE
*&---------------------------------------------------------------------*
*&       CLASS cl_alv_events DEFINITION
*&---------------------------------------------------------------------*
*        text
*----------------------------------------------------------------------*
CLASS cl_alv_events DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_alv_sycom FOR EVENT before_user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    " cl_alv_events

*&---------------------------------------------------------------------*
*&       CLASS cl_alv_events IMPLEMENTATION
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_alv_events IMPLEMENTATION.

  METHOD handle_alv_sycom.
    CASE e_ucomm.
*      WHEN '&XXL'.
*** 触发自己的功能
*        PERFORM frm_xls_output.

** 屏蔽原功能
*        go_grid->set_user_command( 'DO_NOTHING' ).
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "handle_alv_sycom

ENDCLASS.                    " cl_alv_events IMPLEMENTATION

DATA: gr_grid TYPE REF TO cl_alv_events.

SELECTION-SCREEN: PUSHBUTTON 01(10) p_renew USER-COMMAND onli.

SELECTION-SCREEN: FUNCTION KEY 1,
FUNCTION KEY 2.

INITIALIZATION.
  PERFORM frm_set_pbo.
  PERFORM frm_get_tabname.
  PERFORM frm_alv_layout.
  PERFORM frm_get_tabdata.
  PERFORM frm_alv_object.

AT SELECTION-SCREEN.

  PERFORM frm_set_pai.
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_PBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_set_pbo .

  p_renew = '@BB@换表'.

  sscrfields-functxt_01 = '@11@全表删除'.
  sscrfields-functxt_02 = '@2L@保存'.

  DATA: BEGIN OF lt_excl OCCURS 0,
          fcode LIKE sy-ucomm,
        END OF lt_excl.

**Exclude User Commands
  lt_excl-fcode = 'ONLI'.
  APPEND lt_excl.
  lt_excl-fcode = 'SJOB'.
  APPEND lt_excl.
  lt_excl-fcode = 'PRIN'.
  APPEND lt_excl.
  lt_excl-fcode = 'SPOS'.
  APPEND lt_excl.

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = lt_excl.

ENDFORM.                    " FRM_SET_PBO
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_TABNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_get_tabname .

  DATA: ls_sval TYPE sval,
        lt_sval TYPE TABLE OF sval,
        l_title TYPE text40,
        l_reply TYPE c.

  CLEAR: g_table,g_zshow.

  ls_sval-tabname   = 'DD02L'.
  ls_sval-fieldname = 'TABNAME'.
  ls_sval-field_obl = 'X'.
  APPEND ls_sval TO lt_sval.

  l_title = '输入表名'.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = l_title
    IMPORTING
      returncode      = l_reply
    TABLES
      fields          = lt_sval
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF sy-subrc <> 0 OR l_reply = 'A'.
    MESSAGE s000 WITH '操作取消！' DISPLAY LIKE 'W'.
    LEAVE PROGRAM.
  ELSE.
    READ TABLE lt_sval INTO ls_sval WITH KEY fieldname = 'TABNAME'.
    IF sy-subrc = 0.
      g_table = ls_sval-value.
      PERFORM frm_check_if_valid.
    ELSE.
      PERFORM frm_get_tabname.
    ENDIF.
  ENDIF.

ENDFORM.                    " FRM_GET_TABNAME
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_IF_VALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

*“标准表也行
*FORM frm_check_if_valid .
*
*  DATA: l_table TYPE tabname.
*
*  IF g_table(1) NE 'Z' AND g_table(1) NE 'Y' AND 1 <> 1.
*    MESSAGE s000 WITH '闈炶嚜瀹氫箟琛ㄤ笉鑳界淮鎶わ紒' DISPLAY LIKE 'E'.
*    PERFORM frm_get_tabname.
*  ELSE.
*    SELECT SINGLE tabname INTO l_table FROM dd02l
*     WHERE tabname = g_table
*       AND tabclass = 'TRANSP'.
*    IF l_table IS INITIAL.
*      MESSAGE s000 WITH '閫忔槑琛#' g_table '涓嶅瓨鍦#锛#' DISPLAY LIKE 'W'.
*      PERFORM frm_get_tabname.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.

FORM frm_check_if_valid .

  DATA: l_table TYPE tabname.

  IF g_table(1) NE 'Z' AND g_table(1) NE 'Y'.
    MESSAGE s000 WITH '非自定义表不能维护！' DISPLAY LIKE 'E'.
    PERFORM frm_get_tabname.
  ELSE.
    SELECT SINGLE tabname INTO l_table FROM dd02l
    WHERE tabname = g_table
    AND tabclass = 'TRANSP'.
    IF l_table IS INITIAL.
      MESSAGE s000 WITH '透明表' g_table '不存在！' DISPLAY LIKE 'W'.
      PERFORM frm_get_tabname.
    ENDIF.
  ENDIF.

ENDFORM.                    " FRM_CHECK_IF_VALID
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_alv_layout .

  CHECK g_table NE space.

  DATA: dy_data TYPE REF TO data. " Table

* Build fieldcat
*  DATA(gt_fcat)
  gt_fcat = CORRESPONDING lvc_t_fcat(
  CAST cl_abap_structdescr(
  cl_abap_structdescr=>describe_by_name( g_table ) )->get_ddic_field_list( )
  MAPPING
  scrtext_m = fieldtext
  key = keyflag
  ref_table = tabname
  ref_field = fieldname
  dd_outlen = headlen
  col_pos = position
*   EXCEPT
*    rollname
  ).

*  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_buffer_active        = space
*      i_structure_name       = g_table
*      i_client_never_display = 'X'
*      i_bypassing_buffer     = space
*    CHANGING
*      ct_fieldcat            = gt_fcat
*    EXCEPTIONS
*      inconsistent_interface = 1
*      program_error          = 2
*      OTHERS                 = 3.

  IF g_zshow IS INITIAL.
    LOOP AT gt_fcat INTO gs_fcat.
      gs_fcat-edit = 'X'.
*      gs_fcat-no_zero = 'X'.
      IF gs_fcat-key = 'X'.
        gs_fcat-emphasize = 'X'.
      ENDIF.
*      gs_fcat-ref_field = ''.
*      gs_fcat-ref_table = ''.
      MODIFY gt_fcat FROM gs_fcat.
    ENDLOOP.
  ENDIF.

* Create a new Table
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = gt_fcat
    IMPORTING
      ep_table        = dy_data.

  ASSIGN dy_data->* TO <table>.

ENDFORM.                    " FRM_ALV_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_OBJECT
*&---------------------------------------------------------------------*
*       text  创建选择屏幕上的对象
*----------------------------------------------------------------------*
FORM frm_alv_object .

  CHECK <table> IS ASSIGNED.

  IMPORT data = <table> FROM MEMORY ID sy-cprog.

  FREE MEMORY ID sy-cprog.

  CHECK go_dock IS INITIAL.

  CREATE OBJECT go_dock
    EXPORTING
      repid = sy-cprog
      dynnr = sy-dynnr
      ratio = 90
      side  = cl_gui_docking_container=>dock_at_bottom
      name  = 'DOCK_CONT'.

  IF sy-subrc <> 0.
    MESSAGE e000 WITH 'Error in the Docking control'.
  ENDIF.

  go_cont ?= go_dock.

  CREATE OBJECT go_grid
    EXPORTING
      i_parent = go_cont.

* Set output layout
  gs_layo-cwidth_opt = 'X'.
  gs_layo-zebra      = 'X'.

* ALV Display
  CALL METHOD go_grid->set_table_for_first_display
    EXPORTING
      i_save                        = 'A'
      is_layout                     = gs_layo
    CHANGING
      it_outtab                     = <table>[]
      it_fieldcatalog               = gt_fcat[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

** Set edit enable
  CALL METHOD go_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

  CALL METHOD cl_gui_control=>set_focus
    EXPORTING
      control = go_grid.

* Set events
  CALL METHOD go_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CREATE OBJECT gr_grid.

  SET HANDLER gr_grid->handle_alv_sycom FOR go_grid.

ENDFORM.                    " FRM_ALV_OBJECT
*&---------------------------------------------------------------------*
*&       Form frm_alv_refresh
*&---------------------------------------------------------------------*
*        text
*----------------------------------------------------------------------*
FORM frm_alv_refresh.

  DATA: ls_stbl TYPE lvc_s_stbl VALUE 'XX'.

  CALL METHOD go_grid->refresh_table_display
    EXPORTING
      is_stable = ls_stbl
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

ENDFORM.                    " frm_alv_refresh
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_TABDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_get_tabdata .
* Get data

  CHECK <table> IS ASSIGNED.

  DATA: l_lines TYPE char8.

  CLEAR <table>[].
  "加上CORRESPONDING FIELDS OF  (CHANGE BY WULB02 20170331)
  SELECT * FROM (g_table) INTO CORRESPONDING FIELDS OF TABLE <table>.

  l_lines = g_lines = lines( <table> ).

  CONCATENATE g_table '-数据条目数-' l_lines  INTO sy-title.

  CONDENSE sy-title NO-GAPS.

ENDFORM.                    " FRM_GET_TABDATA
*&---------------------------------------------------------------------*
*&      Form  FRM_SET_PAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_set_pai .
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
*      PERFORM frm_delete_all.
    WHEN 'FC02'.
      PERFORM frm_save_to_db.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " FRM_SET_PAI
*&---------------------------------------------------------------------*
*&      Form  frm_save_to_db
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_save_to_db.

  CHECK <table> IS ASSIGNED.

  CALL METHOD go_grid->check_changed_data.

  IF <table>[] IS NOT INITIAL.

    DELETE FROM (g_table) WHERE mandt EQ sy-mandt.

    MODIFY (g_table) FROM TABLE <table>.

    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      MESSAGE  '保存成功！' TYPE 'S'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE  '保存失败！' TYPE 'S'.
    ENDIF.

    PERFORM frm_alv_refresh.

  ELSE.
    MESSAGE '无可保存数据！' TYPE 'S' DISPLAY LIKE 'W'.
  ENDIF.

ENDFORM.                    " frm_save_to_db
*&---------------------------------------------------------------------*
*&      Form  FRM_DELETE_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_delete_all .

  CHECK <table> IS ASSIGNED.

  IF <table> IS NOT INITIAL.

    DATA: l_reply TYPE c,
          l_iconm TYPE iconname VALUE 'ICON_MESSAGE_WARNING'.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = '请确认！'
        text_question  = '确认要删除该表的所有数据?!'
        text_button_1  = '是'
        icon_button_1  = 'Y'
        text_button_2  = '否'
        icon_button_2  = 'N'
        default_button = '2'
        popup_type     = l_iconm
      IMPORTING
        answer         = l_reply.

    CHECK l_reply = '1'. " Means YES

    CLEAR l_reply.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = '请确认！'
        text_question  = '请再次确认删除操作!'
        text_button_1  = '是'
        icon_button_1  = 'Y'
        text_button_2  = '否'
        icon_button_2  = 'N'
        default_button = '2'
        popup_type     = l_iconm
      IMPORTING
        answer         = l_reply.

    CHECK l_reply = '1'. " Means YES

    DELETE FROM (g_table) WHERE mandt EQ sy-mandt.

    CHECK sy-subrc = 0.

    MESSAGE '删除成功！' TYPE 'S'.

    REFRESH <table>.

    PERFORM frm_alv_refresh.

  ELSE.
    MESSAGE s000 WITH '没有可以删除的数据！' DISPLAY LIKE 'W'.
  ENDIF.

ENDFORM.                    " FRM_DELETE_ALL
*&---------------------------------------------------------------------*
*&      Form  FRM_XLS_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_xls_output .

  IF <table>[] IS INITIAL.
    MESSAGE s000 DISPLAY LIKE 'W'.
  ELSEIF lines( <table> ) > c_count.
    MESSAGE '数据量太大，下载成文件！' TYPE 'S' DISPLAY LIKE 'W'.
    PERFORM frm_data_to_file.
  ELSE.
    CALL FUNCTION 'ZEXCEL_OLE2_EXPORT'
      TABLES
        it_list = <table>
        it_fcat = gt_fcat.
  ENDIF.
ENDFORM.                    " FRM_XLS_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  FRM_DATA_TO_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_data_to_file .

  DATA: l_p_path TYPE string,
        l_f_path TYPE string,
        l_length TYPE i,
        l_dfname TYPE string,
        l_flname TYPE string,
        l_filter TYPE string,
        l_action TYPE i.

  DATA: BEGIN OF ls_head,
          fname TYPE char30,
        END OF ls_head.

  DATA: lt_head LIKE TABLE OF ls_head.

  l_filter = 'Excel_OLD (*.xls)|*.xls|All Files (*.*)|*.*|'.

  CONCATENATE g_table '-' sy-datum '-' sy-uzeit INTO l_dfname.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_file_name = l_dfname
      file_filter       = l_filter
    CHANGING
      filename          = l_flname
      path              = l_p_path
      fullpath          = l_f_path
      user_action       = l_action.

  IF l_action EQ 0.
**
    LOOP AT gt_fcat INTO gs_fcat.
      ls_head-fname = gs_fcat-scrtext_m.
      APPEND ls_head TO lt_head.
    ENDLOOP.

**
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        bin_filesize = l_length
        filename     = l_flname
        filetype     = 'DAT'
        fieldnames   = lt_head[]
      CHANGING
        data_tab     = <table>
      EXCEPTIONS
        OTHERS       = 1.

    MESSAGE s999 WITH '下载成功：' l_flname.
  ELSE.
    MESSAGE w999 WITH '操作取消！'.
  ENDIF.
ENDFORM.                    " FRM_DATA_TO_FILE

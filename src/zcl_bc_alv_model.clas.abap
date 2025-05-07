class ZCL_BC_ALV_MODEL definition
  public
  create public .

public section.

  data MY_ALV_DATA type ref to DATA .
  data MY_APPLICATION type ref to ZCL_BC_ALV_APPLICATION .

  methods CONSTRUCTOR
    importing
      !IM_TITLEBAR type CHAR20 .
  methods EXCLUDING_FUNCTIONS
    changing
      !CH_EXCLUDING type MMPUR_PF_EXCLUDING_TAB
      !CH_PFSTATUS type C optional
      !CH_PFPROGRAM type SYREPID optional .
  methods TITLEBAR
    changing
      !CH_TITLE type STYP_TITLEBAR_MM .
  methods HANDLE_ON_DATA_CHANGED
    for event DATA_CHANGED of CL_GUI_ALV_GRID
    importing
      !ER_DATA_CHANGED
      !E_ONF4
      !E_ONF4_BEFORE
      !E_ONF4_AFTER
      !E_UCOMM .
  methods HANDLE_HOTSPOT_CLICK
    for event HOTSPOT_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW_ID
      !E_COLUMN_ID
      !ES_ROW_NO .
  methods HANDLE_TOOLBAR_CREATION
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE .
  methods HANDLE_USER_COMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods HANDLE_F4
    for event ONF4 of CL_GUI_ALV_GRID
    importing
      !E_FIELDNAME
      !E_FIELDVALUE
      !ES_ROW_NO
      !ER_EVENT_DATA
      !ET_BAD_CELLS
      !E_DISPLAY .
  methods HANDLE_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW
      !E_COLUMN
      !ES_ROW_NO .
  methods SET_FIELDCATALOG
    returning
      value(RT_FIELDCATALOG) type LVC_T_FCAT .
  methods SET_LAYOUT
    returning
      value(RS_LAYOUT) type LVC_S_LAYO .
  methods SET_VARIANT
    returning
      value(RS_VARIANT) type DISVARIANT .
  methods SET_FIELDS_FOR_F4
    returning
      value(RE_T_F4) type LVC_T_F4 .
  methods SET_DROPDOWN
    returning
      value(RT_DROPDOWN) type LVC_T_DRAL .
  methods GET_DATA .
  methods DYNPRO_USER_COMMAND
    importing
      !IM_UCOMM type SY-UCOMM .
protected section.

  data MY_SEL_DATA type ref to DATA .
  data MY_TITLEBAR type CHAR20 .

  methods TOOLBAR_INIT
    changing
      !CH_BUTTONS type TTB_BUTTON
      !CH_MENUES type TTB_BTNMNU .
  methods NOTIFY_ROW_CHANGED
    importing
      !IM_MODI type LVC_S_MODI
    changing
      !CH_DATA_CHANGED type ref to CL_ALV_CHANGED_DATA_PROTOCOL .
  methods EXECUTE_BEFORE_TRANSPORT
    importing
      !IM_FCODE type SY-UCOMM .
  methods GET_TABLE_LINES
    returning
      value(RE_LINES) type I .
  methods GET_SELECTION .
  methods CHECK_AUTHORITY
    returning
      value(RE_SUBRC) type SY-SUBRC .
  methods SELECT_ALL
    importing
      !IM_SEL type ABAP_BOOL .
  methods RETRIVE_DATA .
  methods PROCESS_DATA .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BC_ALV_MODEL IMPLEMENTATION.


  METHOD check_authority.
  ENDMETHOD.


  METHOD constructor.
    my_titlebar = im_titlebar.
  ENDMETHOD.


  METHOD dynpro_user_command.
  ENDMETHOD.


  METHOD excluding_functions.
  ENDMETHOD.


  METHOD execute_before_transport.
  ENDMETHOD.


  METHOD get_data.
    retrive_data( ).
    process_data( ).
  ENDMETHOD.


  METHOD get_selection.
    DATA lt_rows TYPE lvc_t_row.
    DATA l_line  TYPE i.
    FIELD-SYMBOLS:<fs_table>  TYPE table,
                  <fs_select> TYPE table.

    CHECK my_alv_data IS NOT INITIAL AND my_application IS NOT INITIAL.

    ASSIGN my_alv_data->*  TO <fs_table>.

    IF my_sel_data IS NOT INITIAL.
      ASSIGN my_sel_data->* TO <fs_select>.
      CLEAR:<fs_select>.
    ENDIF.

    my_application->my_grid->get_selected_rows( IMPORTING et_index_rows = lt_rows ).
    cl_gui_cfw=>flush( ).

    LOOP AT lt_rows INTO DATA(ls_row).
      l_line = ls_row-index.
      READ TABLE <fs_table> ASSIGNING FIELD-SYMBOL(<fs_line>) INDEX l_line.
      IF sy-subrc = 0.
        APPEND <fs_line> TO <fs_select>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_table_lines.
    FIELD-SYMBOLS:<fs_table> TYPE table.
    ASSIGN my_alv_data->* TO <fs_table>.
    re_lines = lines( <fs_table> ).
  ENDMETHOD.


  METHOD handle_f4.

  ENDMETHOD.


  METHOD handle_hotspot_click.

  ENDMETHOD.


  METHOD handle_on_data_changed.
    DATA ls_modified_cell  TYPE lvc_s_modi.
    DATA lt_modified_cells TYPE lvc_t_modi.
    DATA l_last_row_id     LIKE ls_modified_cell-row_id.

    IF er_data_changed IS INITIAL.
      RETURN.
    ENDIF.

    lt_modified_cells = er_data_changed->mt_mod_cells.
    SORT lt_modified_cells BY row_id.

    LOOP AT lt_modified_cells INTO ls_modified_cell.
      IF ls_modified_cell-row_id <> l_last_row_id.
        notify_row_changed( EXPORTING im_modi         = ls_modified_cell
                            CHANGING  ch_data_changed = er_data_changed ).
        l_last_row_id = ls_modified_cell-row_id.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD handle_toolbar_creation.
    toolbar_init( CHANGING ch_buttons = e_object->mt_toolbar
                           ch_menues  = e_object->mt_btnmnu ).
  ENDMETHOD.


  METHOD handle_user_command.
    DATA l_fcode TYPE sy-ucomm.

    l_fcode = e_ucomm.

    execute_before_transport( EXPORTING im_fcode = l_fcode ).
  ENDMETHOD.


  METHOD notify_row_changed.
  ENDMETHOD.


  METHOD process_data.
  ENDMETHOD.


  METHOD retrive_data.
    CHECK check_authority( ) = 0.
  ENDMETHOD.


  METHOD select_all.
    FIELD-SYMBOLS <lt_data> TYPE ANY TABLE.
    " 检查LAYOUT中是否存在BOX复选框
    DATA(ls_layout) = set_layout( ).
    CHECK ls_layout-box_fname IS NOT INITIAL.
    " 当传入 X 时，只修改 BOX 为空的数据
    " 当传入 空时，只修改 BOX 为X 的数据
    DATA(lv_where) = xsdbool( im_sel = abap_false ).
    ASSIGN my_alv_data->* TO <lt_data>.
    " 修改内表
    LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
      ASSIGN COMPONENT ls_layout-box_fname
          OF STRUCTURE <ls_data>
          TO FIELD-SYMBOL(<lv_box>).
      <lv_box> = COND abap_boolean( WHEN <lv_box> = lv_where THEN im_sel
                                    ELSE <lv_box> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD set_dropdown.
  ENDMETHOD.


  METHOD set_fieldcatalog.
  ENDMETHOD.


  METHOD set_fields_for_f4.
  ENDMETHOD.


  METHOD set_layout.
  ENDMETHOD.


  METHOD set_variant.
  ENDMETHOD.


  METHOD titlebar.
    ch_title-title = my_titlebar.
  ENDMETHOD.


  METHOD toolbar_init.
  ENDMETHOD.


  METHOD handle_double_click.
    CHECK e_row-rowtype IS INITIAL.
  ENDMETHOD.
ENDCLASS.

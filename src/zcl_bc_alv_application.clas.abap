class ZCL_BC_ALV_APPLICATION definition
  public
  create public .

public section.

  constants MC_OPERATION_ONLI type SY-UCOMM value 'ONLI' ##NO_TEXT.
  constants MC_CUSTOM_CONTAINER type ZEBC_CONTYPE value 'C' ##NO_TEXT.
  constants MC_DOCKING_CONTAINER type ZEBC_CONTYPE value 'D' ##NO_TEXT.
  data MY_MODEL type ref to ZCL_BC_ALV_MODEL .
  data MY_GRID type ref to CL_GUI_ALV_GRID .
  data MY_SET_CURSOR_ROW type LVC_S_ROW .
  data MY_SET_CURSOR_COL type LVC_S_COL .
  data MT_TABLE type ref to DATA .

  methods CONSTRUCTOR
    importing
      !IM_MODEL type ref to ZCL_BC_ALV_MODEL
      !IM_SCREEN type SY-DYNNR
      !IM_PF_STATUS type CHAR20
      !IM_RATIO type I optional
      !IM_CONTAINER_NAME type STRING40 optional
      !IM_PROG type SY-CPROG optional .
  methods PBO .
  methods PAI
    importing
      !IM_UCOMM type SY-UCOMM .
  methods UPDATE_GRID_DISPLAY .
  methods START
    exceptions
      PROG_NOT_FOUND
      FORM_NOT_FOUND .
  methods HAS_ERROR
    returning
      value(RE_ERROR) type ABAP_BOOLEAN .
  methods GET_ERROR_MESSAGE
    returning
      value(RE_MESSAGE) type BAPIRET2 .
  methods SET_MESSAGE
    importing
      !IM_MESSAGE type BAPIRET2 .
  methods INIT_MESSAGE .
  methods GET_MY_PROG
    returning
      value(RE_PROG) type SY-REPID .
protected section.

  data MT_FIELDCATALOG type LVC_T_FCAT .
  data MT_SORT type LVC_T_SORT .
  data MT_MESSAGE type BAPIRET2_T .
  data MS_LAYOUT type LVC_S_LAYO .
private section.

  data MY_PF_STATUS type CHAR20 .
  data MY_CONTAINER_NAME type STRING40 .
  data MY_TITLE type STYP_TITLEBAR_MM .
  data MY_SCREEN type SY-DYNNR .
  data MY_CONTAINER type ref to CL_GUI_CONTAINER .
  data MY_DOCKING_RATIO type I .
  data MY_PROG type SY-REPID .

  methods REMOVE_GRID .
  methods CREATE_CONTAINER .
  methods CREATE_GRID .
  methods GET_BASIC_DATA .
  methods GET_VARIANT
    returning
      value(RE_VARIANT) type DISVARIANT .
  methods GET_EXCLUDING_FUNCTIONS
    returning
      value(RE_EXCLUDE) type UI_FUNCTIONS .
  methods GET_DROPDOWN
    returning
      value(RE_DROPDOWN) type LVC_T_DRAL .
  methods GET_FIELDS_FOR_F4
    returning
      value(RE_T_F4) type LVC_T_F4 .
ENDCLASS.



CLASS ZCL_BC_ALV_APPLICATION IMPLEMENTATION.


  method CONSTRUCTOR.
    " 初始化消息处理器
*    my_message_handler = NEW #( ).
    my_screen         = im_screen.
    my_model          = im_model.
    my_pf_status      = im_pf_status.
    my_docking_ratio  = im_ratio.
    my_container_name = im_container_name.
    my_prog           = COND #( WHEN im_prog IS INITIAL OR im_prog IS NOT SUPPLIED THEN sy-cprog
                                ELSE im_prog ).

    IF im_ratio IS NOT SUPPLIED AND im_container_name IS NOT SUPPLIED.
      " 未指定Docking或container，默认Docking，ratio = 70
      my_docking_ratio = 70.
    ENDIF.

    get_basic_data( ).
  endmethod.


  METHOD create_container.
    DATA l_docking TYPE REF TO cl_gui_docking_container.
    DATA l_custom  TYPE REF TO cl_gui_custom_container.

    CHECK my_container IS INITIAL.
    IF my_container_name IS INITIAL.
      l_docking = NEW #( repid     = my_prog
                         dynnr     = my_screen
                         side      = cl_gui_docking_container=>dock_at_bottom
                         ratio     = my_docking_ratio
                         extension = 220 ).

      my_container = l_docking.
    ELSE.
      l_custom = NEW #( container_name = my_container_name ).
*               repid = l_win_prog
*               dynnr = l_win_dynnr
*               NO_AUTODEF_PROGID_DYNNR = 'X'.
      my_container = l_custom.
    ENDIF.
  ENDMETHOD.


  method CREATE_GRID.
    IF my_container IS INITIAL.
      create_container( ).
    ENDIF.

    CREATE OBJECT my_grid
      EXPORTING
        i_parent          = my_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4.

    IF sy-subrc <> 0.
      " raise error.
    ENDIF.

    SET HANDLER my_model->handle_on_data_changed  FOR my_grid.
    SET HANDLER my_model->handle_hotspot_click    FOR my_grid.
    SET HANDLER my_model->handle_toolbar_creation FOR my_grid.
    SET HANDLER my_model->handle_user_command     FOR my_grid.
    SET HANDLER my_model->handle_f4               FOR my_grid.
    SET HANDLER my_model->handle_double_click     FOR my_grid.
    "ENTER事件
    my_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    "修改事件
    my_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
  endmethod.


  METHOD get_basic_data.
    FIELD-SYMBOLS <fs_table> TYPE table.

    ASSIGN my_model->my_alv_data->* TO <fs_table>.

    GET REFERENCE OF <fs_table> INTO mt_table.
    mt_fieldcatalog = my_model->set_fieldcatalog( ).
    ms_layout       = my_model->set_layout( ).
  ENDMETHOD.


  METHOD get_dropdown.
    re_dropdown = my_model->set_dropdown( ).
  ENDMETHOD.


  METHOD get_excluding_functions.
    DATA ls_exclude TYPE ui_func.

    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
    INSERT ls_exclude INTO TABLE re_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
    INSERT ls_exclude INTO TABLE re_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
    INSERT ls_exclude INTO TABLE re_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    INSERT ls_exclude INTO TABLE re_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    INSERT ls_exclude INTO TABLE re_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
    INSERT ls_exclude INTO TABLE re_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    INSERT ls_exclude INTO TABLE re_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    INSERT ls_exclude INTO TABLE re_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_check.
    INSERT ls_exclude INTO TABLE re_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
    INSERT ls_exclude INTO TABLE re_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
    INSERT ls_exclude INTO TABLE re_exclude.
  ENDMETHOD.


  METHOD get_fields_for_f4.
    re_t_f4 = my_model->set_fields_for_f4( ).
  ENDMETHOD.


  METHOD get_variant.
    re_variant = my_model->set_variant( ).
  ENDMETHOD.


  METHOD pai.
    CASE im_ucomm.
      WHEN 'ONLI'.
        my_model->get_data( ).
      WHEN OTHERS.
        my_model->dynpro_user_command( im_ucomm ).
    ENDCASE.
  ENDMETHOD.


  METHOD pbo.
    DATA lt_excluding TYPE mmpur_pf_excluding_tab.

    my_model->titlebar( CHANGING ch_title = my_title ).
    SET PF-STATUS my_pf_status OF PROGRAM my_prog EXCLUDING lt_excluding.
    IF my_title-title IS NOT INITIAL.
      SET TITLEBAR my_title-title OF PROGRAM my_prog WITH my_title-par1 my_title-par2 my_title-par3
                                                          my_title-par4 my_title-par5 my_title-par6
                                                          my_title-par7 my_title-par8 my_title-par9.
    ENDIF.
    update_grid_display( ).
  ENDMETHOD.


  METHOD remove_grid.
    IF my_grid IS NOT INITIAL.
      my_grid->free( ).

      IF my_container IS NOT INITIAL.
        my_container->free( ).
      ENDIF.

      CLEAR: my_grid,
             my_container.
    ENDIF.
  ENDMETHOD.


  METHOD start.
    " 后台处理不调用屏幕
    CHECK sy-batch IS INITIAL.

    TRY .
      PERFORM frm_call_screen IN PROGRAM (my_prog) USING my_screen.
    CATCH cx_sy_program_not_found.
      RAISE prog_not_found.
    CATCH cx_sy_dyn_call_illegal_form.
      RAISE form_not_found.
    ENDTRY.
  ENDMETHOD.


  METHOD update_grid_display.
    DATA ls_stable   TYPE lvc_s_stbl.
    DATA ls_variant  TYPE disvariant.
    DATA lt_exclude  TYPE ui_functions.
    DATA lt_f4       TYPE lvc_t_f4.
    DATA lt_dropdown TYPE lvc_t_dral.
    FIELD-SYMBOLS <table> TYPE table.

    IF    mt_table        IS INITIAL
       OR mt_fieldcatalog IS INITIAL.
      RETURN.
    ENDIF.

    IF my_grid IS NOT INITIAL.
      ls_stable-row = 'X'.
      ls_stable-col = 'X'.
      " 自适应列宽
      ms_layout-cwidth_opt = abap_true.
      my_grid->set_frontend_layout( ms_layout ).
      " 刷新ALV
      my_grid->refresh_table_display( is_stable = ls_stable ).
    ELSE.
      " 创建ALV Grid
      create_grid( ).

      IF mt_table IS INITIAL.
        RETURN.
      ENDIF.

      ASSIGN mt_table->* TO <table>.

      " 下拉框
      my_grid->set_drop_down_table( it_drop_down_alias = get_dropdown( ) ).

      my_grid->register_f4_for_fields( get_fields_for_f4( ) ).
      " 显示ALV
      my_grid->set_table_for_first_display( EXPORTING
*                                                      IT_LIST_COMMENTARY   = GT_HEADER[]
                                                      i_save               = 'A'
                                                      is_variant           = get_variant( )
                                                      is_layout            = ms_layout
                                                      it_toolbar_excluding = get_excluding_functions( )
                                            CHANGING  it_sort              = mt_sort
                                                      it_outtab            = <table>
                                                      it_fieldcatalog      = mt_fieldcatalog ).
    ENDIF.

    "设置焦点
    IF     my_grid           IS NOT INITIAL
       AND my_set_cursor_row IS NOT INITIAL
       AND my_set_cursor_col IS NOT INITIAL.

      my_grid->set_current_cell_via_id( is_row_id    = my_set_cursor_row
                                        is_column_id = my_set_cursor_col ).

      CLEAR my_set_cursor_col.

      my_grid->set_scroll_info_via_id( is_row_info = my_set_cursor_row
                                       is_col_info = my_set_cursor_col ).

      CLEAR: my_set_cursor_row,
             my_set_cursor_col.

      my_grid->set_focus( my_grid ).
    ENDIF.
  ENDMETHOD.


  METHOD get_error_message.
    READ TABLE mt_message INTO re_message WITH KEY type = 'E'.
  ENDMETHOD.


  METHOD has_error.
    READ TABLE mt_message
      TRANSPORTING NO FIELDS
        WITH KEY type = 'E'.
    IF sy-subrc = 0.
      re_error = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD set_message.
    CHECK im_message IS NOT INITIAL.
    APPEND im_message TO mt_message.
  ENDMETHOD.


  METHOD init_message.
    REFRESH mt_message.
  ENDMETHOD.


  METHOD get_my_prog.
    re_prog = my_prog.
  ENDMETHOD.
ENDCLASS.

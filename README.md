# abap ALV Template 用法
## 1.定义本地类并继承 zcl_bc_alv_model
``` abap
CLASS lcl_model DEFINITION INHERITING FROM zcl_bc_alv_model.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS titlebar             REDEFINITION.
    METHODS set_fieldcatalog     REDEFINITION.
    METHODS set_layout           REDEFINITION.

  PROTECTED SECTION.
    METHODS retrive_data REDEFINITION.
    METHODS process_data REDEFINITION.

  PRIVATE SECTION.
    DATA mt_data TYPE TABLE OF sflight.
ENDCLASS.
```
``` abap
CLASS lcl_model IMPLEMENTATION.
  METHOD constructor.
    " 默认标题
    super->constructor( im_titlebar = '9000' ).
    " 获取数据引用
    GET REFERENCE OF mt_data INTO my_alv_data.
  ENDMETHOD.

  METHOD titlebar.
    "
    super->titlebar( CHANGING ch_title = ch_title ).
    " 标题
    ch_title-par1 = |测试程序|.
    ch_title-par2 = |【条目数:{ CONV string( get_table_lines( ) ) }】|.
  ENDMETHOD.

  METHOD set_fieldcatalog.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'SFLIGHT'
      CHANGING
        ct_fieldcat      = rt_fieldcatalog.
  ENDMETHOD.

  METHOD set_layout.
    rs_layout-zebra      = abap_true.
    rs_layout-cwidth_opt = abap_true.
    rs_layout-sel_mode   = 'D'.
  ENDMETHOD.

  METHOD retrive_data.
    " 调用父类方法进行权限校验
    super->retrive_data( ).
    " 主数据获取
    SELECT *
      FROM sflight UP TO 100 ROWS
      INTO CORRESPONDING FIELDS OF TABLE @mt_data.
  ENDMETHOD.

  METHOD process_data.
    " 处理数据
  ENDMETHOD.
ENDCLASS.
```

## 2.实例化 zcl_bc_alv_application 并将 model 传入
``` abap
START-OF-SELECTION.
  " 实例化 ZCL_BC_ALV_APPLICATION
  PERFORM frm_execute_fcode.

FORM frm_execute_fcode .
  DATA lo_application   TYPE REF TO zcl_bc_alv_application.
  DATA ls_container_set TYPE zsbc_container_setting.

  DATA(lo_model) = NEW lcl_model( ).
  ls_container_set-container_type  = zcl_bc_alv_application=>mc_custom_container."'C'.
  ls_container_set-with_gui_status = abap_false.

  CALL FUNCTION 'ZFM_BC_ALV_APPSTART'
    EXPORTING  i_model              = lo_model
               i_container_type     = ls_container_set
    IMPORTING  i_application        = lo_application
    EXCEPTIONS no_container_type    = 1
               wrong_container_type = 2.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  " 执行数据查询（ 若为Docking容器则注释该段，并将9000屏幕查询按钮UCOMM设置为ONLI）
  lo_application->pai( zcl_bc_alv_application=>mc_operation_onli ).

  IF lo_application->has_error( ) = abap_true.
    DATA(ls_message) = lo_application->get_error_message( ).
    MESSAGE ID ls_message-id TYPE 'S' NUMBER ls_message-number
            WITH ls_message-message_v1
                 ls_message-message_v2
                 ls_message-message_v3
                 ls_message-message_v4 DISPLAY LIKE ls_message-type.
    LEAVE LIST-PROCESSING.
  ENDIF.
  " 调用屏幕
  lo_application->start( EXCEPTIONS prog_not_found = 1
                                    form_not_found = 2 ).

  DATA(lv_prog) = lo_application->get_my_prog( ).

  PERFORM frm_check_subrc IN PROGRAM (lv_prog) USING sy-subrc.
ENDFORM.
```

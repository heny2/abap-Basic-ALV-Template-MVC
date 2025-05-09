# abap ALV Template 用法
## 1.定义本地类并继承 ZCL_BC_ALV_MODEL
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
ENDCLASS.

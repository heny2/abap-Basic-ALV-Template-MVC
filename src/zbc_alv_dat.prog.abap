*&---------------------------------------------------------------------*
*& Include ZBC_ALV_DAT
*&---------------------------------------------------------------------*
* 字段目录
DATA:ls_fieldcatalog TYPE lvc_s_fcat.
DEFINE set_field_catalog.
  CLEAR: ls_fieldcatalog.
  ls_fieldcatalog-fix_column    = &2.
  ls_fieldcatalog-tabname       = &3.
  ls_fieldcatalog-fieldname     = &4.
  ls_fieldcatalog-outputlen     = &5.
  ls_fieldcatalog-coltext       = &6.
  ls_fieldcatalog-scrtext_l     = &6.
  ls_fieldcatalog-scrtext_m     = &6.
  ls_fieldcatalog-scrtext_s     = &6.
  ls_fieldcatalog-checkbox      = &7.
*  ls_fieldcat-no_zero       = &7.
  ls_fieldcatalog-ref_table     = &8.
  ls_fieldcatalog-ref_field     = &9.
  APPEND ls_fieldcatalog TO &1.
END-OF-DEFINITION.

" 屏幕OK-CODE
DATA ok-code  TYPE sy-ucomm.

" GUI 动态文本
DATA gv_but01 TYPE smp_dyntxt.
DATA gv_but02 TYPE smp_dyntxt.
DATA gv_but03 TYPE smp_dyntxt.
DATA gv_but04 TYPE smp_dyntxt.
DATA gv_but05 TYPE smp_dyntxt.
DATA gv_but06 TYPE smp_dyntxt.
DATA gv_but07 TYPE smp_dyntxt.

" ALV应用程序
DATA go_application TYPE REF TO zcl_bc_alv_application.

*&---------------------------------------------------------------------*
*& 包含               LZFG_BC_ALVMCR
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

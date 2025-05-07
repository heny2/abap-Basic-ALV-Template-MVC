*&---------------------------------------------------------------------*
*& 包含               LZFG_BC_ALVDAT
*&---------------------------------------------------------------------*
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

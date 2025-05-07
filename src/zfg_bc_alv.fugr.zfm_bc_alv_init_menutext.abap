FUNCTION zfm_bc_alv_init_menutext.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(IT_DYNTXT) TYPE  WSFW_T_SMP_DYNTXT OPTIONAL
*"----------------------------------------------------------------------
  DATA lv_fieldname TYPE string.
  DATA lv_button(2) TYPE n.

  CLEAR:gv_but01,gv_but02,gv_but03,
        gv_but04,gv_but05,gv_but06,
        gv_but07.

  LOOP AT it_dyntxt INTO DATA(ls_dyntxt).
    lv_button    = sy-tabix.
    lv_fieldname = |GV_BUT{ lv_button }|.
    ASSIGN (lv_fieldname) TO FIELD-SYMBOL(<fs_dyntxt>).
    <fs_dyntxt>  = ls_dyntxt.

    CLEAR lv_fieldname.
  ENDLOOP.

ENDFUNCTION.

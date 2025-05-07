FUNCTION ZFM_BC_ALV_APPSTART.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(I_MODEL) TYPE REF TO  ZCL_BC_ALV_MODEL
*"     REFERENCE(I_CONTAINER_TYPE) TYPE  ZSBC_CONTAINER_SETTING
*"       OPTIONAL
*"  EXPORTING
*"     REFERENCE(I_APPLICATION) TYPE REF TO  ZCL_BC_ALV_APPLICATION
*"  EXCEPTIONS
*"      NO_CONTAINER_TYPE
*"      WRONG_CONTAINER_TYPE
*"----------------------------------------------------------------------
  DATA lv_screen TYPE sy-dynnr.

  IF i_container_type IS NOT SUPPLIED OR i_container_type IS INITIAL.
    RAISE no_container_type.
  ENDIF.

  lv_screen = COND #( WHEN i_container_type-with_gui_status = abap_true THEN '9000'
                      ELSE '9001' ).

  CASE i_container_type-container_type.
    WHEN 'C'."c_container.
      go_application = NEW #( im_model     = i_model
                              im_screen    = lv_screen
                              im_pf_status = '9000'
*                              im_ratio     = 95
                              im_container_name = 'CC_9000'
                              im_prog      = sy-repid
                              ).
    WHEN 'D'."c_docking.
      go_application = NEW #( im_model     = i_model
                              im_screen    = lv_screen
                              im_pf_status = '9000'
                              im_ratio     = 95
*                              im_container_name = 'CC_9000'
                              im_prog      = sy-repid
                              ).
    WHEN OTHERS.
      RAISE wrong_container_type.
  ENDCASE.

  i_model->my_application = go_application.
  i_application           = go_application.

ENDFUNCTION.

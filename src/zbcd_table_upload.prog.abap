*&---------------------------------------------------------------------*
*& Report ZABAP_TOOL_3
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT zbcd_table_upload.

DATA: gt_table TYPE REF TO data,
      gs_table TYPE REF TO data,
      gt_data  TYPE REF TO data,
      gs_data  TYPE REF TO data.

FIELD-SYMBOLS: <gt_table> TYPE table,
               <gs_table> TYPE any,
               <gt_data>  TYPE table,
               <gs_data>  TYPE any.

DATA: gv_lines TYPE sy-tabix,
      gv_save  TYPE c,
      gv_cnt   TYPE p,
      gv_cnts  TYPE p.

DATA: gv_tabname LIKE dd02l-tabname.

SELECTION-SCREEN BEGIN OF BLOCK blk_b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file  TYPE localfile OBLIGATORY,
              p_bcol  TYPE i OBLIGATORY DEFAULT 1,
              p_brow  TYPE i OBLIGATORY DEFAULT 2,
              p_ecol  TYPE i OBLIGATORY,
              p_erow  TYPE i OBLIGATORY,
              p_sname TYPE dd02l-tabname OBLIGATORY,
              p_tname LIKE dd02l-tabname OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk_b1.

SELECTION-SCREEN BEGIN OF BLOCK blk_b2 WITH FRAME TITLE TEXT-002.
  SELECTION-SCREEN BEGIN OF LINE .
    SELECTION-SCREEN  POSITION 2 .
    SELECTION-SCREEN COMMENT 2(50) txt_01.
  SELECTION-SCREEN END OF LINE .

  SELECTION-SCREEN BEGIN OF LINE .
    SELECTION-SCREEN  POSITION 2 .
    SELECTION-SCREEN COMMENT 2(50) txt_02.
  SELECTION-SCREEN END OF LINE .

  SELECTION-SCREEN BEGIN OF LINE .
    SELECTION-SCREEN  POSITION 2 .
    SELECTION-SCREEN COMMENT 2(50) txt_03.
  SELECTION-SCREEN END OF LINE .

  SELECTION-SCREEN BEGIN OF LINE .
    SELECTION-SCREEN  POSITION 2 .
    SELECTION-SCREEN COMMENT 2(50) txt_04.
  SELECTION-SCREEN END OF LINE .

SELECTION-SCREEN END OF BLOCK blk_b2.

INITIALIZATION .
  txt_01 = '1:Excel第一行为数据表的字段'.
  txt_02 = '2:Excel里面需要有[MANDT]列'.
  txt_03 = '3:日期字段格式为YYYYMMDD'.
  txt_04 = '4:有前导0的字段需要补齐前导0'.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM frm_file_shp.

START-OF-SELECTION.

  SELECT SINGLE tabname
    INTO gv_tabname
    FROM dd02l
    WHERE tabname = p_sname
      AND as4local = 'A'.
  IF sy-subrc NE 0.
    MESSAGE i899(mm) WITH '结构名：' p_sname '不存在'.
    STOP.
  ENDIF.

  SELECT SINGLE tabname
    INTO gv_tabname
    FROM dd02l
    WHERE tabname = p_tname
      AND as4local = 'A'.
  IF sy-subrc NE 0.
    MESSAGE i899(mm) WITH '表名：' p_tname '不存在'.
    STOP.
  ENDIF.

  IF p_tname(1) <> 'Z'.
    MESSAGE '只能更新自定义表' TYPE 'E'.
    STOP.
  ENDIF.

* 结构对应内表
  CREATE DATA gt_data TYPE TABLE OF (p_sname).
  ASSIGN gt_data->* TO <gt_data>.
  CREATE DATA gs_data LIKE LINE OF <gt_data>.
  ASSIGN gs_data->* TO <gs_data>.

* 表对应内表
  CREATE DATA gt_table TYPE TABLE OF (p_tname).
  ASSIGN gt_table->* TO <gt_table>.
  CREATE DATA gs_table LIKE LINE OF <gt_table>.
  ASSIGN gs_table->* TO <gs_table>.

  PERFORM frm_upload_data TABLES <gt_data>.

*  CALL FUNCTION 'ZGPFM_EXCEL_UPLOAD'
*    EXPORTING
*      iv_filename  = p_file
*      iv_begin_col = p_bcol
*      iv_begin_row = p_brow
*      iv_end_col   = p_ecol
*      iv_end_row   = p_erow
*    TABLES
*      et_data      = <gt_data>.

  CLEAR: gv_save,
         gv_lines,
         <gt_table>.
  gv_cnt =  lines( <gt_data> ).
  gv_cnts = 0.

  DATA:li_fields_info   TYPE TABLE OF dfies WITH HEADER LINE,
       lv_conv_function TYPE rs38l_fnam VALUE 'CONVERSION_EXIT_x_INPUT'.
  FIELD-SYMBOLS: <field> TYPE any.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = p_sname
    TABLES
      dfies_tab      = li_fields_info
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.
  DELETE li_fields_info WHERE convexit IS INITIAL.

  LOOP AT <gt_data> INTO <gs_data>.
    gv_lines = gv_lines + 1.
    gv_cnts  = gv_cnts + 1.

    LOOP AT li_fields_info INTO DATA(ls_fieldcat).
      ASSIGN COMPONENT ls_fieldcat-fieldname OF STRUCTURE <gs_data> TO <field>.

      TRY.
          IF <field> IS ASSIGNED.
            lv_conv_function = 'CONVERSION_EXIT_x_INPUT'.
            REPLACE 'x'  IN lv_conv_function WITH ls_fieldcat-convexit.
            TRANSLATE lv_conv_function TO UPPER CASE.
            CALL FUNCTION lv_conv_function
              EXPORTING
                input  = <field>
              IMPORTING
                output = <field>.
          ENDIF.
        CATCH cx_root.
          EXIT.
      ENDTRY.
    ENDLOOP.

    MOVE-CORRESPONDING <gs_data> TO <gs_table>.
    APPEND <gs_table> TO <gt_table>.

    IF gv_lines = 2000 OR gv_cnt = gv_cnts.
      gv_lines = 0.
      MODIFY (p_tname) FROM TABLE <gt_table>.
      IF sy-subrc EQ 0.
        COMMIT WORK.
      ELSE.
        gv_save = 'X'.
        ROLLBACK WORK.
        EXIT.
      ENDIF.
      CLEAR: <gt_table>.
    ENDIF.
  ENDLOOP.

  IF gv_save IS INITIAL.
    MESSAGE s899(mm) WITH '数据上载成功'.
  ELSE.
    MESSAGE s899(mm) WITH '数据上载失败' DISPLAY LIKE 'E'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  FRM_FILE_SHP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_file_shp .
  DATA: lt_file_table TYPE filetable,
        lv_rc         TYPE i,
        lv_filename   TYPE string.
  REFRESH: lt_file_table.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      file_filter             = cl_gui_frontend_services=>filetype_excel "gc_filter
    CHANGING
      file_table              = lt_file_table
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc = 0.
    READ TABLE lt_file_table INTO lv_filename INDEX 1.
    p_file = lv_filename.
  ENDIF.
ENDFORM.

*Text elements
*----------------------------------------------------------
* 001 选择屏幕


*Selection texts
*----------------------------------------------------------
* P_BCOL         开始列
* P_BROW         开始行
* P_ECOL         结束列
* P_EROW         结束行
* P_FILE         文件
* P_SNAME         模版对应结构名
* P_TNAME         写入数据的表名


*Messages
*----------------------------------------------------------
*
* Message class: Z01
*008   数据保存成功
*009   数据保存失败
*010   模版对应结构名 & 不存在
*011   写入数据的表名 & 不存在REPORT ZABAP_TOOL_3.
*&---------------------------------------------------------------------*
*& Form frm_upload_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_upload_data TABLES et_data .
*  DATA: ls_excel TYPE alsmex_tabline,
*        lt_excel TYPE TABLE OF alsmex_tabline.
  DATA: ls_excel TYPE zalsmex_tabline,
        lt_excel TYPE TABLE OF zalsmex_tabline.
  DATA: cl_descr TYPE REF TO cl_abap_structdescr.
  DATA: BEGIN OF ls_components,
          length    TYPE i,
          decimals  TYPE i,
          type_kind TYPE abap_typekind,
          name      TYPE abap_compname,
        END OF ls_components.

  REFRESH: et_data.

  FIELD-SYMBOLS: <ls_data>  TYPE any,
                 <ls_value> TYPE any.

  cl_descr ?= cl_abap_typedescr=>describe_by_data( et_data ).

*  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
  CALL FUNCTION 'ZFM_CM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = p_bcol
      i_begin_row             = p_brow
      i_end_col               = p_ecol
      i_end_row               = p_erow
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE e006(z01).
  ENDIF.

  ASSIGN et_data TO <ls_data>.
  LOOP AT lt_excel INTO ls_excel.
    READ TABLE cl_descr->components INTO ls_components INDEX ls_excel-col.
    IF sy-subrc EQ 0.
      ASSIGN COMPONENT ls_components-name OF STRUCTURE <ls_data> TO <ls_value>.
      IF sy-subrc EQ 0.
        TRY .
            IF ls_components-type_kind EQ 'N'.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = condense( ls_excel-value )
                IMPORTING
                  output = <ls_value>.
            ELSEIF ls_components-type_kind EQ 'P'.
              WHILE sy-subrc EQ 0.
                REPLACE ',' WITH '' INTO ls_excel-value.
              ENDWHILE.
              CONDENSE ls_excel-value NO-GAPS.
              <ls_value> = ls_excel-value.
            ELSE.
              <ls_value> = condense( ls_excel-value ).
            ENDIF.
          CATCH cx_root.
            CLEAR: <ls_value>.
        ENDTRY.
      ENDIF.
    ENDIF.
    AT END OF row.
      APPEND <ls_data> TO et_data.
      CLEAR: <ls_data>.
    ENDAT.
    CLEAR: ls_excel.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Include          ZOT_29_I_ADOBEF_EXCELUP_CL
*&---------------------------------------------------------------------*
CLASS lcl_main DEFINITION.
  PUBLIC SECTION.

    METHODS: display_alv,
      get_data_alv,
      set_fcat,
      set_layout,
      start_of_selection,
      end_of_selection,
      dosya_yolu,
      etiket,
      form,
      excel_upload,
      save,
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive,
      handle_ucomm FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD display_alv.

    me->set_fcat( ). "fieldkatalog ve layoutu doldurmak için
    me->set_layout( ).

    CREATE OBJECT go_cont
      EXPORTING
        container_name = 'CC_ALV'.

    CREATE OBJECT go_alv
      EXPORTING
        i_parent = go_cont.  "cl_gui_container=>screen0. "alv'yi fullscreen basmak için

    SET HANDLER handle_toolbar FOR go_alv.
    SET HANDLER handle_ucomm FOR go_alv.

    CALL METHOD go_alv->set_table_for_first_display
      EXPORTING
*       i_structure_name = 'GT_ALV'      "programda oluşturduğum itab
        is_layout       = gs_layout      " Layout
      CHANGING
        it_outtab       = gt_alv         " internal Table adı yazmam lazım
        it_fieldcatalog = gt_fieldcatalog. " field katalog oluşturup internal table olarak buraya vereceğiz

  ENDMETHOD.

  METHOD get_data_alv.

    SELECT lp~kunnr,
           ls~vbeln,
           ls~posnr,
           ls~pstyv,
           ls~ernam,
           lp~wadat_ist,
           ls~matnr,
           ls~ntgew,
           lp~route,
           ls~gewei FROM likp AS lp INNER JOIN lips AS ls
                      ON lp~vbeln = ls~vbeln
      INTO CORRESPONDING FIELDS OF TABLE @gt_alv
      WHERE ls~vbeln IN @s_vbeln
        AND ls~posnr IN @s_posnr
        AND ls~matnr IN @s_matnr
        AND lp~kunnr IN @s_kunnr.

  ENDMETHOD.

  METHOD set_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZSN_ALV' "z'li structure oluşturdum
*       I_INTERNAL_TABNAME     =  "internal table name
      CHANGING
        ct_fieldcat            = gt_fieldcatalog "fieldcatalog verilir.
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    LOOP AT gt_fieldcatalog ASSIGNING FIELD-SYMBOL(<fs_fcat>).
      CASE <fs_fcat>-fieldname.
        WHEN 'ROUTE'.
          <fs_fcat>-tech = 'X'.

      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD set_layout.

    CLEAR: gs_layout.
    gs_layout-cwidth_opt = abap_true. "kolon optimizasyon
    gs_layout-zebra      = abap_true. "bir koyu renk bir açık renk
    gs_layout-col_opt    = abap_true.
    gs_layout-sel_mode   = 'A'. "tüm verileri seçmek tüm ya da satır bazında

  ENDMETHOD.

  METHOD start_of_selection.

    IF p_alv EQ abap_true.
      go_class->get_data_alv( ).
      go_class->set_fcat( ).
      go_class->set_layout( ).
    ENDIF.

  ENDMETHOD.


  METHOD dosya_yolu.

    "dosya yolunu search help ile seçebilme kodu

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title = 'Dosya Seçimi'
      CHANGING
        file_table   = gt_file_table
        rc           = gv_rc.
    IF sy-subrc = 0.
      READ TABLE gt_file_table INTO gs_file_table INDEX 1.
      p_file = gs_file_table-filename.
    ENDIF.

  ENDMETHOD.

  METHOD handle_toolbar.

    DATA ls_toolbar TYPE stb_button.
    CLEAR ls_toolbar.

    ls_toolbar-function  = 'FORM'.        "fonksiyon ismi sy-ucomm ile tetiklemek için
    ls_toolbar-text      = 'FORM YAZDIR'. "buton texti
    ls_toolbar-icon      = '@0X@'.        "buton ikon
    ls_toolbar-quickinfo = 'Form'.        "buton üzerine geldiğinde yazacak isim
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-function  = 'ETIKET'.
    ls_toolbar-text      = 'ETİKET YAZDIR'.
    ls_toolbar-icon      = '@0X@'.
    ls_toolbar-quickinfo = 'Etiket'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-function   = 'KAYDET'.
    ls_toolbar-text       = 'KAYDET'.
    ls_toolbar-icon       = '@01@'.
    ls_toolbar-quickinfo  = 'Kaydet'.
    IF p_excel EQ abap_true. "butonun aktif veya aktif olmaması
      ls_toolbar-disabled = abap_false.
    ELSE .
      ls_toolbar-disabled = abap_true.
    ENDIF.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD etiket.

    "seçilen satırı yakalama kodu
    go_alv->get_selected_rows(
      IMPORTING
        et_index_rows = gt_rowindex " Indexes of Selected Rows
*      et_row_no      =              " Numeric IDs of Selected Rows
    ).

    IF gt_rowindex IS NOT INITIAL. "satır seçilirse

    LOOP AT gt_rowindex INTO gs_rowindex .
      DATA lv_index TYPE lvc_index.
      lv_index = gs_rowindex-index.
      READ TABLE gt_alv INTO gs_alv INDEX lv_index.
    ENDLOOP.

    ELSE. "satır seçilmezse
      MESSAGE i008(zot_29).
    ENDIF.

    DATA: lv_text TYPE char45. "QRkode 00002 için
    lv_text = '00002'.
    CONCATENATE  lv_text gs_alv-matnr INTO lv_text.

    gs_params-device   = 'PRINTER' .
    gs_params-dest     = 'LP01' . "tanımlı yazcı
    gs_params-nodialog = 'X' .    "yazıcı seçilen pop-up
    gs_params-preview  = 'X' .    "ön yüzde görüntüleme

    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = gs_params
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.

    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING
        i_name     = 'ZOT_29_AF_ETIKET'
      IMPORTING
        e_funcname = gv_funcname.

    CALL FUNCTION gv_funcname
      EXPORTING
*       /1BCDWB/DOCPARAMS  =
        iv_wadat_ist       = gs_alv-wadat_ist
        iv_matnr           = gs_alv-matnr
        iv_posnr           = gs_alv-posnr
        iv_vbeln           = gs_alv-vbeln
        iv_kunnr           = gs_alv-kunnr
        iv_qrkode          = lv_text
*       IV_BARCODE         = 1234567891
      IMPORTING
        /1bcdwb/formoutput = gs_formoutput
      EXCEPTIONS
        usage_error        = 1
        system_error       = 2
        internal_error     = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'FP_JOB_CLOSE'
      IMPORTING
        e_result       = gv_result
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.

  METHOD form.

    CLEAR gv_funcname.

*    DATA: lv_logo     TYPE xstring,
*          lv_logoname TYPE tdobname.
*
*    lv_logoname = 'BANK'.
*
*    CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
*      EXPORTING
*        p_object       = 'GRAPHICS'  "SAPscript Graphics Management: Application object
*        p_name         = lv_logoname "logo Name
*        p_id           = 'BMAP'      "SAPscript Graphics Management: ID
*        p_btype        = 'BCOL'      "SAPscript: Type of graphic
*      RECEIVING
*        p_bmp          = lv_logo     "Graphic Data
*      EXCEPTIONS
*        not_found      = 1
*        internal_error = 2
*        OTHERS         = 3.

    DATA: lt_af        TYPE TABLE OF zot_29_t_af,
          lt_alv_group TYPE TABLE OF zot_29_t_af, "group by için tanımladım
          ls_af        TYPE zot_29_t_af.

    go_alv->get_selected_rows(
  IMPORTING
    et_index_rows = gt_rowindex " Indexes of Selected Rows
*      et_row_no      =              " Numeric IDs of Selected Rows
).

    IF gt_rowindex IS NOT INITIAL.

      LOOP AT gt_rowindex INTO gs_rowindex .
        DATA: lv_index2 TYPE lvc_index.
        lv_index2 = gs_rowindex-index.
        READ TABLE gt_alv INTO gs_alv INDEX lv_index2.
        ls_af = CORRESPONDING #( gs_alv ).
        APPEND ls_af TO lt_af.
      ENDLOOP.

    ELSE.
      MESSAGE i008(zot_29).
    ENDIF.

    gs_params-device   = 'PRINTER' .
    gs_params-dest     = 'LP01' .
    gs_params-nodialog = 'X' .
    gs_params-preview  = 'X' .

    FREE lt_alv_group.

    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = gs_params
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    LOOP AT lt_af INTO ls_af  "gruplamak için kullandığım kısım
           GROUP BY ls_af-kunnr.

      FREE lt_alv_group.
      LOOP AT GROUP ls_af INTO DATA(ls_alv_group).
        APPEND ls_alv_group TO lt_alv_group.
        CLEAR  ls_alv_group.
      ENDLOOP.

      DATA: lv_name TYPE fpname.
      lv_name = 'ZOT_29_AF_FORM'.

      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name     = lv_name
        IMPORTING
          e_funcname = gv_funcname
*         E_INTERFACE_TYPE           =
*         EV_FUNCNAME_INBOUND        =
        .

      CALL FUNCTION gv_funcname
        EXPORTING
          /1bcdwb/docparams  = gs_docparams
          it_itab            = lt_alv_group "lt_af
        IMPORTING
          /1bcdwb/formoutput = gs_formoutput
        EXCEPTIONS
          usage_error        = 1
          system_error       = 2
          internal_error     = 3
          OTHERS             = 4.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'FP_JOB_CLOSE'
*      IMPORTING
*        e_result       = gv_result
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

*  ENDMETHOD.
  ENDMETHOD.

  METHOD end_of_selection.
    IF gt_alv IS INITIAL.
      MESSAGE e009(zot_29).
    ELSE.
      CALL SCREEN 0100.
    ENDIF.
  ENDMETHOD.

  METHOD handle_ucomm.

    CASE e_ucomm.

      WHEN 'FORM'.
        form( ).

      WHEN 'ETIKET'.
        etiket( ).

      WHEN 'KAYDET'.
        IF p_excel EQ abap_true.
          save( ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD excel_upload.

    IF p_excel EQ abap_true.

      CLEAR gt_alv.

      DATA gt_raw TYPE truxs_t_text_data.

      CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
        EXPORTING
*         I_FIELD_SEPERATOR    =
          i_line_header        = 'X' "başlık satırını alma
          i_tab_raw_data       = gt_raw
          i_filename           = p_file
*         I_STEP               = 1
        TABLES
          i_tab_converted_data = gt_alv
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD save.

    go_alv->get_selected_rows(
           IMPORTING
             et_index_rows = gt_rowindex  ).

    LOOP AT gt_rowindex INTO gs_rowindex .
      READ TABLE gt_alv INDEX gs_rowindex-index INTO gs_alv.

      IF sy-subrc EQ 0.
        gs_excellog = CORRESPONDING #( gs_alv ).

        gs_excellog-log_datum = sy-datum.
        gs_excellog-log_uzeit = sy-uzeit.
        gs_excellog-log_uname = sy-uname.

        APPEND gs_excellog TO gt_excellog.
      ENDIF.
    ENDLOOP.

    MODIFY zot_29_t_eu FROM gs_excellog.

    MESSAGE i004(zot_29) .

  ENDMETHOD.
ENDCLASS.

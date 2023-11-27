*&---------------------------------------------------------------------*
*& Include          ZOT_29_I_ADOBEF_EXCELUP_TOP
*&---------------------------------------------------------------------*

TABLES: likp, lips.

CLASS lcl_main DEFINITION DEFERRED.
DATA: go_class TYPE REF TO lcl_main.

TYPES: BEGIN OF gty_alv,
         kunnr     TYPE likp-kunnr,
         vbeln     TYPE lips-vbeln,
         posnr     TYPE lips-posnr,
         pstyv     TYPE lips-pstyv,
         ernam     TYPE lips-ernam,
         wadat_ist TYPE likp-wadat_ist,
         matnr     TYPE lips-matnr,
         ntgew     TYPE lips-ntgew,
         gewei     TYPE lips-gewei,
         route     TYPE likp-route,
       END OF gty_alv.

DATA: gt_alv TYPE TABLE OF gty_alv,
      gs_alv TYPE gty_alv.

DATA: gt_rowindex TYPE lvc_t_row,
      gs_rowindex LIKE LINE OF gt_rowindex.

DATA: go_alv  TYPE REF TO cl_gui_alv_grid,
      go_cont TYPE REF TO cl_gui_custom_container.

DATA: gt_fieldcatalog TYPE lvc_t_fcat, "fieldcatalog doldurmak için ona ait bir structure olması lazım
      gs_fieldcatalog TYPE lvc_s_fcat.

DATA gs_layout  TYPE lvc_s_layo. "layout için structure oluşturdum

DATA: gv_rc TYPE i.
DATA: gt_file_table TYPE filetable, "file yolu seçmek için
      gs_file_table TYPE file_table.

DATA: gs_params     TYPE sfpoutputparams.
DATA: gv_result     TYPE sfpjoboutput.
DATA: gv_funcname   TYPE funcname.
DATA: gs_docparams  TYPE sfpdocparams.
DATA: gs_formoutput TYPE fpformoutput.

FIELD-SYMBOLS : <gt_data> TYPE STANDARD TABLE .

TYPES: BEGIN OF gty_excelup,
         kunnr     TYPE likp-kunnr,
         vbeln     TYPE lips-vbeln,
         posnr     TYPE lips-posnr,
         pstyv     TYPE lips-pstyv,
         ernam     TYPE lips-ernam,
         wadat_ist TYPE likp-wadat_ist,
         matnr     TYPE lips-matnr,
         ntgew     TYPE lips-ntgew,
         gewei     TYPE lips-gewei,
       END OF gty_excelup.

DATA: gt_excelup TYPE TABLE OF gty_excelup,
      gs_excelup TYPE gty_excelup.

DATA: gt_excellog TYPE TABLE OF zot_29_t_eu,
      gs_excellog TYPE zot_29_t_eu.

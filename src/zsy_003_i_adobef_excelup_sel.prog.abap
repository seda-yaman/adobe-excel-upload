*&---------------------------------------------------------------------*
*& Include          ZOT_29_I_ADOBEF_EXCELUP_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_vbeln FOR lips-vbeln MODIF ID sec,
                  s_posnr FOR lips-posnr MODIF ID sec,
                  s_matnr FOR lips-matnr MODIF ID sec,
                  s_kunnr FOR likp-kunnr MODIF ID sec.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

  PARAMETERS: p_alv   TYPE char1 RADIOBUTTON GROUP gp1 DEFAULT 'X' USER-COMMAND radio,
              p_excel TYPE char1 RADIOBUTTON GROUP gp1.

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.

  PARAMETERS: p_file type rlgrap-filename MODIF ID sc1.

SELECTION-SCREEN END OF BLOCK b3.

AT SELECTION-SCREEN OUTPUT. "genelde entera basınca tetikliyor

  LOOP AT SCREEN INTO DATA(gs_screen).
    IF p_excel <> 'X' AND "excel butonu tıklanırsa üst kısmı kapatıp file alanını açar
       gs_screen-group1 = 'SC1'.
       gs_screen-active = '0'.
    ENDIF.
    IF gs_screen-group1      = 'SC1'.
       gs_screen-intensified = '1'.
      MODIFY SCREEN FROM gs_screen.
      CONTINUE.
    ENDIF.

    IF p_alv <> 'X' AND "alv butonu tıklanırsa üst kısmı geri açmak için
       gs_screen-group1 = 'SC1'.
       gs_screen-active = '1'.
    ENDIF.
    IF gs_screen-group1      = 'SC1'.
       gs_screen-intensified = '0'.
      MODIFY SCREEN FROM gs_screen.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  IF p_excel = 'X'. "b1 blok gizlemek için
    LOOP AT SCREEN.
      IF screen-group1 = 'SEC'.
         screen-input  = 0.
      ELSE.
         screen-input  = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

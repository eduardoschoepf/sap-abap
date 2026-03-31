*========================================================================*
* Report......: Z_RADIOBUTTON                                            *
* Author......: Eduardo Schoepf                                          *
* Date........: 31.03.2026                                               *
*------------------------------------------------------------------------*
* Description.: Implementing a radio button example                      *
*------------------------------------------------------------------------*
*========================================================================*
REPORT Z_RADIOBUTTON.

TABLES: lagp.

*---------------------------------------------------------------------*
* Tela de seleção
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  PARAMETERS:
    p_lgnum TYPE lagp-lgnum,              "Sistema de depósito
    r_tanum RADIOBUTTON GROUP grp1 USER-COMMAND ucom DEFAULT 'X',
    r_pos   RADIOBUTTON GROUP grp1,
    "Ordem de trasporte
    p_tanum TYPE ltak-tanum MODIF ID ot,  "Ordem de transporte
    "Posição
    p_lgtyp TYPE lagp-lgtyp MODIF ID po.  "Tipo depósito
  SELECT-OPTIONS:
    s_lgpla FOR lagp-lgpla MODIF ID po.   "Posição no depósito
SELECTION-SCREEN END OF BLOCK b1.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'OT'.
        screen-active = COND #( WHEN r_tanum = 'X' THEN 1 ELSE 0 ).
        MODIFY SCREEN.
      WHEN 'PO'.
        screen-active = COND #( WHEN r_pos = 'X' THEN 1 ELSE 0 ).
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.

*---------------------------------------------------------------------*
* START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.
  IF r_tanum = 'X'.
    " Lógica para Ordem de Transporte
    WRITE: / 'Ordem de Transporte selecionada:', p_tanum.
  ELSEIF r_pos = 'X'.
    " Lógica para Posição
    WRITE: / 'Posição selecionada - Tipo:', p_lgtyp.
  ENDIF.

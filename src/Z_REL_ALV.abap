*=====================================================================*
* Report......: Z_REL_ALV                                             *
* Author......: ESCHOEPF                                              *
* Date........: 21.08.2025 - 13:00                                    *
*---------------------------------------------------------------------*
* Description.: Demo alv report                                       *
*---------------------------------------------------------------------*
* Date         Author       Description                               *
* 21.08.2025   ESCHOEPF     Create a demo alv report                  *
*=====================================================================*

REPORT Z_REL_ALV.

*-- 1. Criar um REPORT (Transação SE38)
*-- 2. Declarar uma tabela interna 
DATA: IT_SFLIGHT TYPE TABLE OF SFLIGHT.

*-- 3. Seleção de dados da tabela SFLIGHT
SELECT *
  FROM SFLIGHT
  INTO TABLE IT_SFLIGHT
  UP TO 20 ROWS.

IF SY-SUBRC <> 0 OR IT_SFLIGHT IS INITIAL.
  MESSAGE 'Nenhum dado encontrado' TYPE 'I'.
  RETURN.
ENDIF.

*-- 4. Criar o relatório ALV 
TRY.
  CALL METHOD CL_SALV_TABLE=>FACTORY( 
    IMPORTING R_SALV_TABLE = DATA(GR_TABLE)
    CHANGING  T_TABLE      = IT_SFLIGHT
  ).

*-- 5. Exibir o relatório ALV básico
  GR_TABLE->DISPLAY( ).

  CATCH CX_SALV_MSG INTO DATA(LX_MSG).    "Tratamento de exceções especificas do ALV
    MESSAGE LX_MSG->GET_TEXT( ) TYPE 'E'.

  CATCH CX_ROOT INTO DATA(LX_ROOT).       "Tratamento de outras exceções
    MESSAGE LX_ROOT->GET_TEXT( ) TYPE 'E'.

ENDTRY.

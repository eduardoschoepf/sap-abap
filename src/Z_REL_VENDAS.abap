*=====================================================================*
* Report......: Z_REL_VENDAS                                          *
* Author......: ESCHOEPF                                              *
* Date........: 25.09.2025 - 13:00                                    *
*---------------------------------------------------------------------*
* Description.: Create a demo alv report                              *
*---------------------------------------------------------------------*
* Date         Author       Description                               *
* 25.09.2025   ESCHOEPF     Create a demo alv report                  *
*=====================================================================*
REPORT Z_REL_VENDAS.

TABLES EKKO.

*--Seleção de Dados
*--Criar uma tela de seleção (Selection-Screen) com os seguintes parâmetros:
*--Intervalo de número de pedido de compras (EKKO-EBELN).
*--Campo de fornecedor (LIFNR).
*--Intervalo de datas de criação do pedido (EKKO-BEDAT).
SELECTION-SCREEN BEGIN OF BLOCK b001.
SELECT-OPTIONS s_nped  FOR  EKKO-EBELN.
PARAMETERS     p_fornc TYPE LIFNR.
SELECT-OPTIONS s_data  FOR  EKKO-BEDAT.
SELECTION-SCREEN END OF BLOCK b001.

START-OF-SELECTION.
*--Leitura de Tabelas
*--Ler dados da tabela EKKO (Cabeçalho de pedidos de compras):
*--Número do pedido (EKKO-EBELN).
*--Data do pedido (EKKO-BEDAT).
*--Fornecedor (EKKO-LIFNR).
*--Tratamento dos Dados
*--Aplicar os filtros da seleção.
  DATA lt_result TYPE STANDARD TABLE OF EKKO.
  
  IF p_fornc IS INITIAL.
    SELECT *
      FROM ekko
      INTO TABLE lt_result
      WHERE ebeln IN s_nped
        AND bedat IN s_data.
  ELSE.
    SELECT *
      FROM ekko
      INTO TABLE lt_result
      WHERE ebeln IN s_nped
        AND lifnr EQ  p_fornc
        AND bedat IN s_data.
  ENDIF.
  
*--Relatório
*--Exibir os campos da tabela EKKO na tela utilizando WRITE ou ALV
  IF SY-SUBRC EQ 0.
    TRY.
        CALL METHOD CL_SALV_TABLE=>FACTORY(
          IMPORTING
            R_SALV_TABLE = DATA(lr_alv)
          CHANGING
            T_TABLE      = lt_result
                           ).

        lr_alv->display( ).

      CATCH CX_SALV_MSG INTO DATA(lr_msg).
        MESSAGE lr_msg->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDIF.

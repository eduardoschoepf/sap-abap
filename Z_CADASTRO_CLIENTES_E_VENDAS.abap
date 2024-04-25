*&---------------------------------------------------------------------*
*& Report Z_CADASTROCLIENTESEVENDAS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_CADASTROCLIENTESEVENDAS.

TABLES: ztcad_clientes,
        ztcad_venda_085.

DATA: lv_nome_cliente TYPE string ,
      lv_rg           TYPE string,
      lv_cpf          TYPE string,
      lv_endereco     TYPE string,
      lv_email        TYPE string,
      lv_telefone     TYPE string.


* Declaração de variáveis
PARAMETERS: r_cli RADIOBUTTON GROUP rd USER-COMMAND selection_changed DEFAULT 'X',
            r_vnd RADIOBUTTON GROUP rd,
            r_rlt RADIOBUTTON GROUP rd.

SELECTION-SCREEN BEGIN OF BLOCK A WITH FRAME TITLE text-001.
    PARAMETERS: p_clie1 TYPE char50 MODIF ID cli,
                p_rg1 TYPE char10 MODIF ID cli,
                p_cpf1 TYPE char10 MODIF ID cli,
                p_end1 TYPE char100 MODIF ID cli,
                p_email1 TYPE char40 MODIF ID cli,
                p_telef1 TYPE string MODIF ID cli.
  SELECTION-SCREEN END OF BLOCK A.



  SELECTION-SCREEN BEGIN OF BLOCK B WITH FRAME TITLE text-002.
    PARAMETERS: p_rg2 TYPE char10 MODIF ID vnd,
                p_cpf2 TYPE char10 MODIF ID vnd,
                p_end2 TYPE char10 MODIF ID vnd,
                p_data2 TYPE dats MODIF ID vnd,
                p_prod2 TYPE char30 MODIF ID vnd,
                p_valor2 TYPE char12 MODIF ID vnd.
  SELECTION-SCREEN END OF BLOCK B.


SELECTION-SCREEN BEGIN OF BLOCK C WITH FRAME TITLE text-003.
    PARAMETERS: p_cod3 TYPE int2 MODIF ID rlt,
                p_rg3 TYPE char10 MODIF ID rlt,
                p_cpf3 TYPE char10 MODIF ID rlt,
                p_data3 TYPE dats MODIF ID rlt,
                p_prod3 TYPE char30 MODIF ID rlt.
  SELECTION-SCREEN END OF BLOCK C.

AT SELECTION-SCREEN OUTPUT.
LOOP AT SCREEN.
  IF r_cli EQ 'X'.
    IF SCREEN-GROUP1 = 'CLI'.
      SCREEN-ACTIVE = '1'.
    ENDIF.

    IF SCREEN-GROUP1 = 'VND' OR SCREEN-GROUP1 = 'RLT'.
      SCREEN-ACTIVE = '0'.
    ENDIF.

  ELSEIF r_vnd EQ 'X'.
    IF SCREEN-GROUP1 = 'VND'.
      SCREEN-ACTIVE = '1'.
    ENDIF.

    IF SCREEN-GROUP1 = 'CLI' OR SCREEN-GROUP1 = 'RLT'.
      SCREEN-ACTIVE = '0'.
    ENDIF.

   ELSEIF r_rlt EQ 'X'.
     IF SCREEN-GROUP1 = 'RLT'.
      SCREEN-ACTIVE = '1'.
    ENDIF.

    IF SCREEN-GROUP1 = 'CLI' OR SCREEN-GROUP1 = 'VND'.
      SCREEN-ACTIVE = '0'.
    ENDIF.
  ENDIF.

  MODIFY SCREEN.
ENDLOOP.

START-OF-SELECTION.

* Lógica de seleção e exibição de tela
IF r_cli = 'X'.
  WRITE: / 'Você escolheu "Cadastrar cliente"',
         / 'Campos específicos: Nome, RG, CPF'.
ELSEIF r_vnd = 'X'.
  WRITE: / 'Você escolheu "Cadastrar venda"',
         / 'Campos específicos: RG, CPF, Produto'.
ELSEIF r_rlt = 'X'.
  WRITE: / 'Você escolheu "Relatório de vendas"',
         / 'Campos específicos: Cód. Venda, RG, CPF'.
ENDIF.

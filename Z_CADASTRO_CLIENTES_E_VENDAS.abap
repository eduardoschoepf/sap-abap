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
    PARAMETERS: p_cli TYPE char50 MODIF ID cli,
                p_rgcli TYPE char10 MODIF ID cli,
                p_cpfcli TYPE char10 MODIF ID cli,
                p_endcli TYPE char100 MODIF ID cli,
                p_emlcli TYPE char40 MODIF ID cli,
                p_telcli TYPE string MODIF ID cli.
  SELECTION-SCREEN END OF BLOCK A.



  SELECTION-SCREEN BEGIN OF BLOCK B WITH FRAME TITLE text-002.
    PARAMETERS: p_rgvnd TYPE char10 MODIF ID vnd,
                p_cpfvnd TYPE char10 MODIF ID vnd,
                p_endvnd TYPE char10 MODIF ID vnd,
                p_datvnd TYPE dats MODIF ID vnd,
                p_prdvnd TYPE char30 MODIF ID vnd,
                p_valvnd TYPE char12 MODIF ID vnd.
  SELECTION-SCREEN END OF BLOCK B.


SELECTION-SCREEN BEGIN OF BLOCK C WITH FRAME TITLE text-003.
    PARAMETERS: p_codrlt TYPE int2 MODIF ID rlt,
                p_rgrlt TYPE char10 MODIF ID rlt,
                p_cpfrlt TYPE char10 MODIF ID rlt,
                p_datrlt TYPE dats MODIF ID rlt,
                p_prdrlt TYPE char30 MODIF ID rlt.
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
         / 'Campos específicos: Nome, RG, CPF',
         / p_cli, p_rgcli, p_cpfcli, p_endcli, p_emlcli, p_telcli.
ELSEIF r_vnd = 'X'.
  WRITE: / 'Você escolheu "Cadastrar venda"',
         / 'Campos específicos: RG, CPF, Produto'.
ELSEIF r_rlt = 'X'.
  WRITE: / 'Você escolheu "Relatório de vendas"',
         / 'Campos específicos: Cód. Venda, RG, CPF'.
ENDIF.

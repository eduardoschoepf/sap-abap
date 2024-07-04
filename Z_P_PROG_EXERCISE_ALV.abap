*=====================================================================*
* Report......: Z_P_PROG_EXERCISE_ALV                                 *
* Author......: ESCHOEPF                                              *
* Date........: 11.04.2024 - 23:14                                    *
*---------------------------------------------------------------------*
* Description.:                                                       *
*---------------------------------------------------------------------*
* Date         Author       Description                               *
* 04.07.2024   ESCHOEPF     Create a header template                  *
*=====================================================================*
REPORT Z_P_PROG_EXERCISE_ALV.

**** Declarar uma tabela interna e uma opção de seleção
TABLES: mara, makt.

***** Para criar ALV, é preciso declarar para um grupo de tipos.
TYPE-POOLS: slis.

**** Begin of é um bloco de declaracao de variaveis
TYPES: BEGIN OF ty_mara_criados,
        MATNR TYPE mara-matnr,
        ERSDA TYPE mara-ersda,
        ERNAM TYPE mara-ernam,
        LVORM TYPE mara-lvorm,
        MTART TYPE mara-mtart,
        MATKL TYPE mara-matkl,
      END OF ty_mara_criados.

TYPES: BEGIN OF ty_mara_eliminados,
        MATNR TYPE mara-matnr,
        ERSDA TYPE mara-ersda,
        AENAM TYPE mara-aenam,
        LVORM TYPE mara-lvorm,
        MTART TYPE mara-mtart,
        MATKL TYPE mara-matkl,
      END OF ty_mara_eliminados.

TYPES: BEGIN OF ty_makt,
        MATNR TYPE makt-matnr,
        MAKTX TYPE makt-maktx,
        SPRAS TYPE makt-spras,
       END OF ty_makt.

DATA: t_mara_makt_criados TYPE TABLE OF ZEMATERIAIS_CRIADOS,
      t_mara_makt_eliminados TYPE TABLE OF ZEMATERIAIS_ELIMINADOS,
      t_mara_criados TYPE TABLE OF ty_mara_criados,
      t_mara_eliminados TYPE TABLE OF ty_mara_eliminados,
      t_makt TYPE TABLE OF ty_makt,
      w_mara_makt_criados TYPE ZEMATERIAIS_CRIADOS,
      w_mara_makt_eliminados TYPE ZEMATERIAIS_ELIMINADOS,
      w_mara_criados TYPE ty_mara_criados,
      w_mara_eliminados TYPE ty_mara_eliminados,
      w_makt TYPE ty_makt.

**** Declaracoes que sao usadas no ALV. Tabelas e estrutura que terao as informacoes dos campos do ralatorio.
DATA t_fieldcat TYPE slis_t_fieldcat_alv. "categoria de tabela
DATA w_fieldcat TYPE slis_fieldcat_alv. "linha, estrutura."

**** Declaração dois botões de push
SELECTION-SCREEN: BEGIN OF LINE,
   PUSHBUTTON 5(20) botao1 USER-COMMAND criados,
   PUSHBUTTON 30(20) botao2 USER-COMMAND eliminados,
END OF LINE.

***** Aqui começa os eventos. INITIALIZATION é um evento que é chamado ANTES de exibir a tela. Iremos dar os nomes dos botoes
***** A ordem dos eventos é importante : 1. INITIALIZATION, 2. AT SELECTION-SCREEN e 3. START-OF-SELECTION
INITIALIZATION.
botao1 = 'Materiais Criados'.
botao2 = 'Materiais Eliminados'.

**** A opção de seleção "s_ersda" é criada para o campo "ersda" da tabela "mara". O usuário poderá selecionar um intervalo de valores para o campo "ersda" ao executar o programa.
SELECT-OPTIONS: s_ersda FOR mara-ersda.

***** Esse comando serve para validar as acoes. ***** sy-ucomm é uma variavel do sistema que retorna qual botao foi clicado.
AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'CRIADOS'.
      PERFORM f_selec_dados_criados.
    WHEN 'ELIMINADOS'.
      PERFORM f_selec_dados_eliminados.
  ENDCASE.


START-OF-SELECTION.

FORM F_SELEC_DADOS_criados.
    "Com os dados da seleçao abaixo ir na tabela MAKT e buscar os campos MATNR, MAKTX et SPRAS com as condiçoes MATNR = MATNR selecionados na tabela MARA e SPRAS = 'PT'
    SELECT matnr ersda ernam lvorm mtart matkl
    FROM mara
    INTO TABLE t_mara_criados
    WHERE ERSDA IN s_ersda AND
          LVORM = space.

          IF SY-SUBRC = 0.
             SELECT matnr maktx spras
             FROM makt
             INTO TABLE t_makt
             FOR ALL ENTRIES IN t_mara_criados
             WHERE matnr = t_mara_criados-matnr AND
                   spras = 'PT'.
                   IF sy-subrc = 0. "Seleção OK
                     PERFORM f_monta_fieldcat_criados.
                   ELSE.
                     MESSAGE 'Não existem materiais criados para o periodo informado' TYPE 'I'.
                   ENDIF.
         ELSE.
            MESSAGE 'Não existem materiais criados para o periodo informado' TYPE 'I'.
         ENDIF.
ENDFORM.


FORM F_SELEC_DADOS_ELIMINADOS.
     SELECT matnr ersda aenam lvorm mtart matkl
     FROM mara
     INTO TABLE t_mara_eliminados
     WHERE ersda IN s_ersda AND
           lvorm = 'X'.

          IF sy-subrc = 0.
             SELECT matnr maktx spras
             FROM makt
             INTO TABLE t_makt
             FOR ALL ENTRIES IN t_mara_eliminados
             WHERE matnr = t_mara_eliminados-matnr AND
                   spras = 'PT'.
                   "Verifica se a seleção da tabela MAKT retornou dados
                   IF sy-subrc = 0.
                     PERFORM f_monta_fieldcat_eliminados.
                   ELSE.
                     MESSAGE 'Não existem materiais eliminados para o periodo informado' TYPE 'I'.
                   ENDIF.
         ELSE.
           MESSAGE 'Não existem materiais eliminados para o periodo informado' TYPE 'I'.
         ENDIF.
ENDFORM.

FORM F_MONTA_FIELDCAT_CRIADOS.
  LOOP AT t_makt INTO w_makt.
    READ TABLE t_mara_criados INTO w_mara_criados WITH KEY matnr = w_makt-matnr.
    IF sy-subrc = 0.
      w_mara_makt_criados-matnr = w_mara_criados-matnr.
      w_mara_makt_criados-ersda = w_mara_criados-ersda.
      w_mara_makt_criados-ernam = w_mara_criados-ernam.
      w_mara_makt_criados-lvorm = w_mara_criados-lvorm.
      w_mara_makt_criados-mtart = w_mara_criados-mtart.
      w_mara_makt_criados-matkl = w_mara_criados-matkl.
      w_mara_makt_criados-maktx = w_makt-maktx.
      APPEND w_mara_makt_criados TO t_mara_makt_criados.
    ELSE.
      MESSAGE 'Não existem materiais criados para o periodo informado' TYPE 'I'.
    ENDIF.
  ENDLOOP.
  PERFORM f_fieldcat USING:
        'MATNR' 'Material' 1,
        'ERSDA' 'Data de criação' 2,
        'ERNAM' 'Nome do responsavel que adicionou o objeto' 3,
        'LVORM' 'Marcar mat.para eliminação a nivel de mandante' 4,
        'MTART' 'Tipo de material' 5,
        'MATKL' 'Grupo de mercadorias' 6,
        'MAKTX' 'Descriçao do material' 7.
  PERFORM f_exibe_alv_criados.
ENDFORM.

FORM F_MONTA_FIELDCAT_ELIMINADOS.
  LOOP AT t_makt INTO w_makt.
    READ TABLE t_mara_eliminados INTO w_mara_eliminados WITH KEY matnr = w_makt-matnr.
    IF sy-subrc = 0.
      w_mara_makt_eliminados-matnr = w_mara_eliminados-matnr.
      w_mara_makt_eliminados-ersda = w_mara_eliminados-ersda.
      w_mara_makt_eliminados-aenam = w_mara_eliminados-aenam.
      w_mara_makt_eliminados-lvorm = w_mara_eliminados-lvorm.
      w_mara_makt_eliminados-mtart = w_mara_eliminados-mtart.
      w_mara_makt_eliminados-matkl = w_mara_eliminados-matkl.
      w_mara_makt_eliminados-maktx = w_makt-maktx.
      APPEND w_mara_makt_eliminados TO t_mara_makt_eliminados.
    ELSE.
      MESSAGE 'Não existem materiais criados para o periodo informado' TYPE 'I'.
    ENDIF.
  ENDLOOP.
  PERFORM f_fieldcat USING:
        'MATNR' 'Material' 1,
        'ERSDA' 'Data de criação' 2,
        'AENAM' 'Nome do responsavel pela modificação do objeto' 3,
        'LVORM' 'Marcar mat.para eliminação a nivel de mandante' 4,
        'MTART' 'Tipo de material' 5,
        'MATKL' 'Grupo de mercadorias' 6,
        'MAKTX' 'Descriçao do material' 7.
  PERFORM f_exibe_alv_eliminados.
ENDFORM.

FORM F_FIELDCAT USING p_campo p_descricao p_posicao.
  w_fieldcat-fieldname = p_campo.
  w_fieldcat-seltext_m = p_descricao.
  w_fieldcat-col_pos   = p_posicao.

  APPEND w_fieldcat TO t_fieldcat.
  CLEAR  w_fieldcat.
ENDFORM.

FORM F_EXIBE_ALV_CRIADOS.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    I_CALLBACK_PROGRAM = sy-repid "Nome do programa"
    IT_FIELDCAT = t_fieldcat " tabela interna com os campos do relatorio"
  TABLES
     T_OUTTAB = t_mara_makt_criados
  EXCEPTIONS
    PROGRAM_ERROR = 1
    OTHERS = 2.
ENDFORM.

FORM F_EXIBE_ALV_ELIMINADOS.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    I_CALLBACK_PROGRAM = sy-repid "Nome do programa"
    IT_FIELDCAT = t_fieldcat " tabela interna com os campos do relatorio"
  TABLES
     T_OUTTAB = t_mara_makt_eliminados
  EXCEPTIONS
    PROGRAM_ERROR = 1
    OTHERS = 2.
ENDFORM.

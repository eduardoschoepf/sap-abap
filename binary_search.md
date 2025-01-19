# **Como otimizar suas buscas em tabelas internas no SAP ABAP com o comando READ TABLE BINARY SEARCH**

A comunidade ABAP está sempre buscando formas de otimizar a performance de seus programas. Para quem está começando ou mesmo para consultores experientes, entender o funcionamento de comandos como o **READ TABLE com BINARY SEARCH** é essencial para criar códigos mais performáticos e eficientes. Neste artigo, vou explicar o que é, para que serve e como funciona essa funcionalidade, além de apresentar algumas dicas práticas.

---

## **O que é READ TABLE com BINARY SEARCH?**
O comando **READ TABLE** com a opção **BINARY SEARCH** é usado para localizar um registro específico em uma tabela interna, utilizando o método de pesquisa binária. Trata-se de uma forma muito eficiente de busca, que reduz consideravelmente o número de comparações necessárias em tabelas internas grandes.

---

## **Para que serve?**
O principal objetivo do **BINARY SEARCH** é melhorar a performance de buscas em tabelas internas. Enquanto a pesquisa sequencial analisa cada registro até encontrar uma correspondência (ou alcançar o final da tabela), a pesquisa binária reduz esse esforço dividindo a tabela em partes menores a cada iteração.

---

## **Como funciona a pesquisa binária?**
O funcionamento do **BINARY SEARCH** pode ser resumido em quatro etapas:  

1. **Divisão da tabela**: A tabela interna é dividida em duas partes.  
2. **Verificação do elemento central**: O registro central é comparado com o valor buscado.  
3. **Determinação da próxima etapa**: Se o valor buscado for menor que o registro central, a busca continua na metade inferior; caso contrário, na metade superior.  
4. **Repetição**: O processo é repetido até encontrar o registro desejado ou determinar que ele não existe.  

Este método é significativamente mais rápido porque reduz exponencialmente o número de registros analisados.

---

## **Pré-requisitos**
Para que o **BINARY SEARCH** funcione corretamente, é preciso atender a dois pré-requisitos:  

1. **Tabela Interna Ordenada**: Antes de usar o comando, a tabela interna deve ser ordenada pelo(s) campo(s) usado(s) como critério de busca. Isso é feito com o comando **SORT**.  
2. **Chave de Busca Definida**: Os campos utilizados na busca devem ser especificados com precisão no comando **READ TABLE**.

---

## **Declaração e Exemplo Prático**
Abaixo está um exemplo de como usar o comando **READ TABLE com BINARY SEARCH** no SAP ABAP:

```abap
*=====================================================================*
* Report......: Z_PROG_BINARY_SEARCH                                  *
* Author......: ESCHOEPF                                              *
* Date........: 16.01.2025 - 22:00                                    *
*---------------------------------------------------------------------*
* Description.: Buscas em tabelas com BINARY SEARCH                   *
*---------------------------------------------------------------------*
* Date         Author       Description                               *
* 19.01.2025   ESCHOEPF     Implementação do exemplo                  *
*=====================================================================*
REPORT Z_PROG_BINARY_SEARCH.

" --- Declaração de dados ---
" Opção 1: Tabela interna do tipo padrão (Standard Table), será necessário ordenar manualmente.
DATA: it_materiais TYPE TABLE OF mara,    " Tabela interna padrão compatível com mara
      wa_material TYPE mara.              " Work area compatível com mara

" Opção 2: Tabela interna ordenada automaticamente (SORTED TABLE).
" Comentada aqui como uma alternativa que elimina a necessidade de um comando SORT manual.
" DATA: it_materiais TYPE SORTED TABLE OF mara
"                    WITH UNIQUE KEY matnr.

" --- Preenchendo a tabela interna ---
" Opção 1: SELECT sem ordenação explícita (necessário ordenar manualmente após o SELECT).
" Neste exemplo, a busca é limitada a 1000 registros apenas para fins de demonstração.
SELECT *
  FROM mara
  UP TO 1000 ROWS
  INTO TABLE it_materiais
  WHERE matnr IS NOT NULL.

" Opção 2: SELECT com ordenação explícita no banco de dados (dispensa o comando SORT).
" Comentada aqui como uma alternativa ao SORT manual.
" SELECT *
"   FROM mara
"   UP TO 1000 ROWS
"   INTO TABLE it_materiais
"   WHERE matnr IS NOT NULL
"   ORDER BY matnr ASCENDING.

" --- Verificar o preenchimento da tabela ---
IF sy-subrc <> 0.
  WRITE: / 'Nenhum material encontrado na tabela MARA.'.
  EXIT. " Finaliza o programa caso não haja registros.
ENDIF.

" --- Ordenação da tabela interna ---
" Caso tenha utilizado a Opção 1 (Standard Table e SELECT sem ORDER BY),
" será necessário ordenar manualmente a tabela interna antes de realizar a busca binária.
SORT it_materiais BY matnr.

" --- Busca otimizada com BINARY SEARCH ---
" Procurar um registro específico (exemplo: material '000000000000000023').
READ TABLE it_materiais
    WITH KEY matnr = '000000000000000023' " Chave de busca
    INTO wa_material                      " Armazena o registro encontrado
    BINARY SEARCH.                        " Usa a pesquisa binária para otimizar a busca

" --- Verificar o resultado da busca ---
IF sy-subrc = 0.
    WRITE: / 'Material encontrado (BINARY SEARCH):', wa_material-matnr.
ELSE.
    WRITE: / 'Material não encontrado (BINARY SEARCH).'.
ENDIF.

" --- Comentários adicionais ---
" 1. Caso tenha utilizado uma SORTED TABLE na declaração, o comando SORT é desnecessário,
"    pois a tabela interna já estará automaticamente ordenada pelo campo matnr.
" 2. Caso o SELECT tenha sido feito com ORDER BY matnr ASCENDING, o comando SORT também pode ser omitido.
" 3. Este exemplo mantém o SORT para fins didáticos, mostrando como garantir que a tabela esteja ordenada.
```

## **Explicação do Código**
A tabela interna **it_materiais** é ordenada pelo campo **matnr** (código do material) com o comando **SORT**.
O comando **READ TABLE** procura um registro com o valor '000000000000000050' no campo **matnr**.
A opção **BINARY SEARCH** é utilizada para otimizar a busca.
O sistema verifica a variável **sy-subrc** para identificar se o registro foi encontrado.

## **Diferença para a Busca Linear**
Na busca linear, cada elemento é analisado um a um, o que pode ser ineficiente em listas grandes. Já na busca binária, o número de elementos analisados diminui exponencialmente. Em uma lista com 1.000 elementos, a busca linear pode realizar até 1.000 comparações, enquanto a busca binária realizará, no máximo, 10 (log₂(1000) ≈ 10).

```abap
*=====================================================================*
* Report......: Z_PROG_BINARY_SEARCH_PERFORM                          *
* Author......: ESCHOEPF                                              *
* Date........: 18.01.2025 - 22:00                                    *
*---------------------------------------------------------------------*
* Description.: Buscas em tabelas com BINARY SEARCH                   *
*---------------------------------------------------------------------*
* Date         Author       Description                               *
* 19.01.2025   ESCHOEPF     Implementação do exemplo                  *
*=====================================================================*
REPORT Z_PROG_BINARY_SEARCH_PERFORM.

DATA: it_materiais TYPE SORTED TABLE OF mara " Tabela interna compatível com mara ordenada pela chave matnr
                   WITH UNIQUE KEY matnr,
      wa_material TYPE mara,                 " Work area compatível com mara
      lv_start_time TYPE timestamp,          " Tempo inicial
      lv_end_time TYPE timestamp,            " Tempo final
      lv_duration TYPE i,                    " Duração (em microsegundos)
      lv_line_exists_duration TYPE i,        " Duração do LINE_EXISTS
      lv_line_index_duration TYPE i,         " Duração do LINE_INDEX
      lv_index TYPE sy-tabix.                " Índice retornado pelo LINE_INDEX

" Preencher a tabela interna com até 1000 registros da tabela MARA
SELECT *
  FROM mara
  UP TO 1000 ROWS
  INTO TABLE it_materiais
  WHERE matnr IS NOT NULL.

" Verificar se a tabela interna foi preenchida corretamente
IF sy-subrc <> 0.
  WRITE: / 'Nenhum material encontrado na tabela MARA'.
  EXIT.
ENDIF.

" --- LINE_EXISTS ---
" Capturar o tempo de início para LINE_EXISTS
GET TIME STAMP FIELD lv_start_time.

" Verificar se o registro existe com LINE_EXISTS
IF line_exists( it_materiais[ matnr = '000000000000000023' ] ).
  READ TABLE it_materiais WITH KEY matnr = '000000000000000023' INTO wa_material.
  WRITE: / 'Material encontrado (LINE_EXISTS):', wa_material-matnr.
ELSE.
  WRITE: / 'Material não encontrado (LINE_EXISTS).'.
ENDIF.

" Capturar o tempo de fim para LINE_EXISTS
GET TIME STAMP FIELD lv_end_time.

" Calcular a duração da busca em microsegundos para LINE_EXISTS
lv_line_exists_duration = lv_end_time - lv_start_time.

" Exibir a duração da busca em microsegundos para LINE_EXISTS
WRITE: / 'Duração da busca LINE_EXISTS (microsegundos): ', lv_line_exists_duration.

" --- LINE_INDEX ---
" Capturar o tempo de início para LINE_INDEX
GET TIME STAMP FIELD lv_start_time.

" Determinar o índice do registro com LINE_INDEX
lv_index = line_index( it_materiais[ matnr = '000000000000000023' ] ).

" Capturar o tempo de fim para LINE_INDEX
GET TIME STAMP FIELD lv_end_time.

" Calcular a duração da busca em microsegundos para LINE_INDEX
lv_line_index_duration = lv_end_time - lv_start_time.

" Verificar o resultado da busca com LINE_INDEX
IF lv_index > 0.
  READ TABLE it_materiais INDEX lv_index INTO wa_material.
  WRITE: / 'Material encontrado (LINE_INDEX):', wa_material-matnr.
ELSE.
  WRITE: / 'Material não encontrado (LINE_INDEX).'.
ENDIF.

" Exibir a duração da busca em microsegundos para LINE_INDEX
WRITE: / 'Duração da busca LINE_INDEX (microsegundos): ', lv_line_index_duration.

" --- BINARY SEARCH ---
" Capturar o tempo de início para BINARY SEARCH
GET TIME STAMP FIELD lv_start_time.

" Procurar um registro específico com BINARY SEARCH
READ TABLE it_materiais
    WITH KEY matnr = '000000000000000023'
    INTO wa_material
    BINARY SEARCH.

" Capturar o tempo de fim para BINARY SEARCH
GET TIME STAMP FIELD lv_end_time.

" Calcular a duração da busca em microsegundos para BINARY SEARCH
lv_duration = lv_end_time - lv_start_time.

" Verificar o resultado da busca com BINARY SEARCH
IF sy-subrc = 0.
    WRITE: / 'Material encontrado (BINARY SEARCH):', wa_material-matnr.
ELSE.
    WRITE: / 'Material não encontrado (BINARY SEARCH).'.
ENDIF.

" Exibir a duração da busca em microsegundos para BINARY SEARCH
WRITE: / 'Duração da busca BINARY SEARCH (microsegundos): ', lv_duration.

" Comparação de duração
WRITE: / 'Comparação de Duração:'.
WRITE: / 'Duração BINARY SEARCH: ', lv_duration, 'microsegundos'.
WRITE: / 'Duração LINE_EXISTS: ', lv_line_exists_duration, 'microsegundos'.
WRITE: / 'Duração LINE_INDEX: ', lv_line_index_duration, 'microsegundos'.
```

---
## **Conclusão**
O uso do **READ TABLE** com **BINARY SEARCH** é uma prática recomendada para otimizar programas ABAP que lidam com grandes volumes de dados.
Certifique-se sempre de ordenar suas tabelas internas antes de usar o comando e defina corretamente os critérios de busca.
Se você está começando sua jornada no SAP ABAP, lembre-se: pequenos ajustes podem fazer uma grande diferença na eficiência do seu código!

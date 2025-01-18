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
* --.--.----   --------     -----------------                         *
*=====================================================================*
REPORT Z_PROG_BINARY_SEARCH.

" Declarar uma tabela interna e um work area, ambos de tipo mara
DATA: it_materiais TYPE TABLE OF mara,  " Tabela interna compatível com mara
      wa_material TYPE mara.            " Work area compatível com mara

" Preencher a tabela interna com até 1000 registros da tabela MARA
SELECT * 
  FROM mara
  UP TO 1000 ROWS.
  INTO TABLE it_materiais
  WHERE matnr IS NOT NULL.

" Verificar se a tabela interna foi preenchida corretamente
IF sy-subrc <> 0.
  WRITE: / 'Nenhum material encontrado na tabela MARA'.
  EXIT.
ENDIF.

" Ordenar a tabela interna pela chave matnr
SORT it_materiais BY matnr.

" Procurar um registro específico com BINARY SEARCH
READ TABLE it_materiais
    WITH KEY matnr = '000000000000000050'
    INTO wa_material
    BINARY SEARCH.

" Verificar o resultado da busca
IF sy-subrc = 0.
    WRITE: / 'Material encontrado:', wa_material-matnr.
ELSE.
    WRITE: / 'Material não encontrado.'.
ENDIF.
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
* Report......: Z_PROG_BINARY_SEARCH_VS_LINE_EXISTS                   *
* Author......: ESCHOEPF                                              *
* Date........: 18.01.2025 - 22:00                                    *
*---------------------------------------------------------------------*
* Description.: Buscas em tabelas com BINARY SEARCH                   *
*---------------------------------------------------------------------*
* Date         Author       Description                               *
* --.--.----   --------     -----------------                         *
*=====================================================================*
REPORT Z_PROG_BINARY_SEARCH_VS_LINE_EXISTS.

" Declarar uma tabela interna e um work area, ambos de tipo mara
DATA: it_materiais TYPE TABLE OF mara,  " Tabela interna compatível com mara
      wa_material TYPE mara,            " Work area compatível com mara
      lv_start_time TYPE timestamp,    " Variável para armazenar o tempo inicial
      lv_end_time TYPE timestamp,      " Variável para armazenar o tempo final
      lv_duration TYPE i,              " Variável para armazenar a duração
      lv_line_exists_duration TYPE i.  " Duração do line_exists

" Preencher a tabela interna com até 3000 registros da tabela MARA
SELECT *
  FROM mara
  UP TO 3000 ROWS
  INTO TABLE it_materiais
  WHERE matnr IS NOT NULL.

" Verificar se a tabela interna foi preenchida corretamente
IF sy-subrc <> 0.
  WRITE: / 'Nenhum material encontrado na tabela MARA'.
  EXIT.
ENDIF.

" Ordenar a tabela interna pela chave matnr
SORT it_materiais BY matnr.

" --- BINARY SEARCH ---
" Capturar o tempo de início para BINARY SEARCH
GET TIME STAMP FIELD lv_start_time.

" Procurar um registro específico com BINARY SEARCH
READ TABLE it_materiais
    WITH KEY matnr = '000000000000002590'
    INTO wa_material
    BINARY SEARCH.

" Capturar o tempo de fim para BINARY SEARCH
GET TIME STAMP FIELD lv_end_time.

" Calcular a diferença de tempo em microsegundos para BINARY SEARCH
lv_duration = lv_end_time - lv_start_time.

" Verificar o resultado da busca com BINARY SEARCH
IF sy-subrc = 0.
    WRITE: / 'Material encontrado (BINARY SEARCH):', wa_material-matnr.
ELSE.
    WRITE: / 'Material não encontrado (BINARY SEARCH).'.
ENDIF.

" Exibir a duração da busca em microsegundos para BINARY SEARCH
WRITE: / 'Duração da busca BINARY SEARCH (microsegundos): ', lv_duration.

" Limpar variáveis antes de reutilizá-las
CLEAR: lv_start_time, lv_end_time.

" --- LINE_EXISTS ---
" Capturar o tempo de início para LINE_EXISTS
GET TIME STAMP FIELD lv_start_time.

" Verificar se o registro existe com LINE_EXISTS
IF line_exists( it_materiais[ matnr = '000000000000002590' ] ).
  " Procurar o registro utilizando LINE_EXISTS
  READ TABLE it_materiais WITH KEY matnr = '000000000000002590' INTO wa_material.

  " Verificar o resultado da busca com LINE_EXISTS
  WRITE: / 'Material encontrado (LINE_EXISTS):', wa_material-matnr.
ELSE.
  WRITE: / 'Material não encontrado (LINE_EXISTS).'.
ENDIF.

" Capturar o tempo de fim para LINE_EXISTS
GET TIME STAMP FIELD lv_end_time.

" Calcular a diferença de tempo em microsegundos para LINE_EXISTS
lv_line_exists_duration = lv_end_time - lv_start_time.

" Exibir a duração da busca em microsegundos para LINE_EXISTS
WRITE: / 'Duração da busca LINE_EXISTS (microsegundos): ', lv_line_exists_duration.

" Comparação de duração
WRITE: / 'Comparação de Duração:'.
WRITE: / 'Duração BINARY SEARCH: ', lv_duration, 'microsegundos'.
WRITE: / 'Duração LINE EXISTS: ', lv_line_exists_duration, 'microsegundos'.
```

---
## **Conclusão**
O uso do **READ TABLE** com **BINARY SEARCH** é uma prática recomendada para otimizar programas ABAP que lidam com grandes volumes de dados.
Certifique-se sempre de ordenar suas tabelas internas antes de usar o comando e defina corretamente os critérios de busca.
Se você está começando sua jornada no SAP ABAP, lembre-se: pequenos ajustes podem fazer uma grande diferença na eficiência do seu código!

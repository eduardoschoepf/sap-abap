## **Definição e implementação de classes**
Ao definir uma classe, você define um plano para um tipo de dados. Na verdade, isso não define nenhum dado, mas define o que significa o nome da classe, em que consistirá um objeto na classe e quais operações podem ser executadas em tal objeto. 
Ou seja, ele define as características abstratas de um objeto, como atributos, campos e propriedades.

Exemplo:
```abap
CLASS lcl_currency DEFINITION.                                               " Define a classe que representará cada moeda
  PUBLIC SECTION.                                                            " Declaração da seção pública da classe, onde métodos e atributos são acessíveis externamente
    " Método construtor para inicializar a moeda com nome e valor em relação ao dólar
    METHODS: constructor IMPORTING iv_name TYPE string                       " Nome da moeda (ex.: 'EUR', 'BRL')
                                   iv_value_to_usd TYPE p DECIMALS 4,        " Valor da moeda em relação ao dólar

             " Método para realizar a conversão entre moedas
             convert IMPORTING iv_amount TYPE p DECIMALS 2                   " Valor a ser convertido
                               io_target_currency TYPE REF TO lcl_currency   " Referência da moeda destino
                     RETURNING VALUE(rv_converted_amount) TYPE p DECIMALS 2. " Resultado da conversão

    " Métodos para retornar o nome e o valor da moeda
    METHODS: get_name RETURNING VALUE(rv_name) TYPE string,                  " Retorna o nome da moeda
             get_value_to_usd RETURNING VALUE(rv_value) TYPE p DECIMALS 4.   " Retorna o valor da moeda em relação ao dólar

PRIVATE SECTION.                                                             " Declaração da seção privada da classe, onde atributos são acessíveis apenas internamente
    DATA: name TYPE string,                                                  " Nome da moeda
          value_to_usd TYPE p DECIMALS 4.                                    " Valor da moeda em relação ao dólar
ENDCLASS.

CLASS lcl_currency IMPLEMENTATION.                                           " Implementação dos métodos da classe
  METHOD constructor.
    " Inicializa os atributos da moeda com os valores fornecidos
    name = iv_name.                                                          " Atribui o nome recebido ao atributo interno 'name'
    value_to_usd = iv_value_to_usd.                                          " Atribui o valor em relação ao dólar
  ENDMETHOD.

  METHOD convert.
    " Realiza a conversão entre a moeda atual e a moeda de destino
    DATA: lv_rate TYPE p DECIMALS 6.                                        " Declara variável para armazenar a taxa de conversão

    " Calcula a taxa de conversão (moeda destino / moeda atual)
    lv_rate = io_target_currency->get_value_to_usd( ) / value_to_usd.

    " Multiplica o valor a ser convertido pela taxa de conversão
    rv_converted_amount = iv_amount * lv_rate.
  ENDMETHOD.

  METHOD get_name.
    " Retorna o nome da moeda
    rv_name = name.
  ENDMETHOD.

  METHOD get_value_to_usd.
    " Retorna o valor da moeda em relação ao dólar
    rv_value = value_to_usd.
  ENDMETHOD.
ENDCLASS.

" --- Programa Principal ---
START-OF-SELECTION. " Início do processamento do programa
  " Declaração de objetos para representar moedas (EUR, BRL, USD)
  DATA: lo_eur TYPE REF TO lcl_currency,                                  " Objeto para representar o Euro
        lo_brl TYPE REF TO lcl_currency,                                  " Objeto para representar o Real Brasileiro
        lo_usd TYPE REF TO lcl_currency.                                  " Objeto para representar o Dólar Americano

  " Inicializa as moedas com seus respectivos valores em relação ao dólar
  lo_eur = NEW lcl_currency( iv_name = 'EUR' iv_value_to_usd = '1.10' ).  " 1 EUR = 1.10 USD
  lo_brl = NEW lcl_currency( iv_name = 'BRL' iv_value_to_usd = '0.18' ).  " 1 BRL = 0.18 USD
  lo_usd = NEW lcl_currency( iv_name = 'USD' iv_value_to_usd = '1.00' ).  " 1 USD = 1.00 USD

  " Declara variáveis para os valores a converter e o resultado
  DATA(lv_amount) TYPE p DECIMALS 2. " Valor a ser convertido
  DATA(lv_result) TYPE p DECIMALS 2. " Resultado da conversão

  " Exemplo de conversão: EUR -> BRL
  lv_amount = 100.                                                        " Valor inicial em Euros
  " Converte o valor de Euros para Reais usando o método 'convert'
  lv_result = lo_eur->convert( iv_amount = lv_amount io_target_currency = lo_brl ).
  " Imprime o resultado da conversão na tela
  WRITE: / 'EUR -> BRL:', lv_amount, '=>', lv_result.

  " Exemplo de conversão: BRL -> USD
  lv_amount = 550.                                                        " Valor inicial em Reais
  " Converte o valor de Reais para Dólares usando o método 'convert'
  lv_result = lo_brl->convert( iv_amount = lv_amount io_target_currency = lo_usd ).
  " Imprime o resultado da conversão na tela
  WRITE: / 'BRL -> USD:', lv_amount, '=>', lv_result.

  " Exemplo de conversão: USD -> EUR
  lv_amount = 100.                                                       " Valor inicial em Dólares
  " Converte o valor de Dólares para Euros usando o método 'convert'
  lv_result = lo_usd->convert( iv_amount = lv_amount io_target_currency = lo_eur ).
  " Imprime o resultado da conversão na tela
  WRITE: / 'USD -> EUR:', lv_amount, '=>', lv_result.

```

*&---------------------------------------------------------------------*
*& Report Z_P_PROG_EXERCISE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_P_PROG_EXERCISE.

PARAMETERS: p_fname  TYPE string,
            p_lname  TYPE string,
            p_length TYPE i,
            p_width  TYPE i,
            p_height TYPE i.

DATA: lv_volume    TYPE i,      " receberá o valor do cálculo do volume 
      lv_check     TYPE string, " é alimentado com um valor Par ou Impar
      lv_user_name TYPE string. " nome completo do usuario

FORM calc_volume USING v_length    TYPE i
                       v_width     TYPE i
                       v_height    TYPE i
                 CHANGING c_volume TYPE i.
  c_volume = v_length * v_width * v_height.
ENDFORM.

FORM check_even_or_odd USING v_number   TYPE i
                       CHANGING c_check TYPE string.
  IF v_number MOD 2 = 0.
    c_check = 'Par'.
  ELSE.
    c_check = 'Impar'.
  ENDIF.
ENDFORM.

START-OF-SELECTION.
  PERFORM calc_volume USING    p_length
                               p_width
                               p_height
                      CHANGING lv_volume.

* Mensagem informativa com o valor do cálculo do volume.
  MESSAGE i000(ZUSER085AG_PROG_TEST) WITH lv_volume.

* Exibe o nome completo do usuário, separado por um espaço. 
  CONCATENATE p_fname p_lname INTO lv_user_name SEPARATED BY space.
  WRITE lv_user_name.
  NEW-LINE.

* Montra na téla o dia e data no formato DD/MM/AAA HH:MM:SS.
  WRITE: sy-datum, sy-uzeit. 
  NEW-LINE.

* Verifica se o volume calculado é um número par ou ímpar.  
  PERFORM check_even_or_odd USING    lv_volume
                            CHANGING lv_check.
  WRITE: 'O volume é um número ', lv_check. 
  NEW-LINE.

* Verifica se o volume é um múltiplo de 10.
* Se não for, é feita uma incrementação para obter um múltiplo de 10. 
* Quando isso acontece, o número de interações é exibido. 
  WHILE lv_volume MOD 10 <> 0.
    lv_volume = lv_volume + 1.
    IF lv_volume MOD 10 = 0.
      WRITE sy-index.
    ENDIF.
  ENDWHILE.

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

*-- 4. Criar o relatório ALV 
*-- 5. Exibir o relatório ALV básico

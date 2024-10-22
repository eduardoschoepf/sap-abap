*=====================================================================*
* Report......: Z_CREATING_INTERNAL_TABLE                             *
* Author......: ESCHOEPF                                              *
* Date........: 22.11.2024 - 20:00                                    *
*---------------------------------------------------------------------*
* Description.:                                                       *
*---------------------------------------------------------------------*
* Date         Author       Description                               *
* 22.11.2024   ESCHOEPF     Creating an internal table                *
*=====================================================================*

REPORT Z_CREATING_INTERNAL_TABLE.

DATA:   BEGIN OF customer_line,
            customer_id TYPE CHAR10,
            name        TYPE CHAR40,
            city        TYPE CHAR30,
        END OF customer_line.

DATA:   customers LIKE TABLE OF customer_line WITH HEADER LINE.


customers-customer_id = '001'.
customers-name = 'Cliente A'.
customers-city = 'São Paulo'.
APPEND customers.

customers-customer_id = '002'.
customers-name = 'Cliente B'.
customers-city = 'Rio de Janeiro'.
APPEND customers.
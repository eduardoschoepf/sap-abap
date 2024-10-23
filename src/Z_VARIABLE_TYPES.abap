*=====================================================================*
* Report......: Z_VARIABLE_TYPES                                      *
* Author......: ESCHOEPF                                              *
* Date........: 12.06.2024 - 13:00                                    *
*---------------------------------------------------------------------*
* Description.: Simple data types                                     *
*---------------------------------------------------------------------*
* Date         Author       Description                               *
* 04.07.2024   ESCHOEPF     Create a header template                  *
*=====================================================================*

REPORT Z_VARIABLE_TYPES.

DATA: lv_int       TYPE i,                     " Integer
      lv_float     TYPE f,                     " Floating-point number
      lv_char      TYPE c LENGTH 10,           " Character
      lv_string    TYPE string,                " String
      lv_date      TYPE d,                     " Date
      lv_time      TYPE t,                     " Time
      lv_dec       TYPE p LENGTH 8 DECIMALS 2, " Decimal with 2 decimals
      lv_xstring   TYPE xstring,               " Hexadecimal string
      lv_bool      TYPE abap_bool,             " Boolean
      lv_timestamp TYPE timestamp.             " Timestamp
     
CONSTANTS pi TYPE p LENGTH 8 DECIMALS 14 VALUE '3.14159265358979'.
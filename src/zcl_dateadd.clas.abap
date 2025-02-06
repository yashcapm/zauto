CLASS zcl_dateadd DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DATEADD IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

  data:wa_day TYPE ztt_dateno.

  DELETE from ztt_dateno.

  wa_day-zday = sy-datum+6(2).

insert ztt_dateno FROM @wa_day.


  ENDMETHOD.
ENDCLASS.

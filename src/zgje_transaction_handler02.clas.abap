CLASS zgje_transaction_handler02 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  CLASS-DATA: go_instance TYPE REF TO zgje_transaction_handler02.

    CLASS-METHODS: get_instance RETURNING VALUE(result) TYPE REF TO zgje_transaction_handler02.

    TYPES: BEGIN OF ty_temp_key,
             cid TYPE abp_behv_cid,
             pid TYPE abp_behv_pid,
           END OF ty_temp_key,
           tt_temp_key TYPE STANDARD TABLE OF ty_temp_key WITH DEFAULT KEY,
           BEGIN OF ty_final_key,
             cid   TYPE abp_behv_cid,
             ebeln TYPE ebeln,
             mblnr TYPE mblnr,
             vbeln TYPE vbeln,
           END OF ty_final_key,
            tt_final_key type STANDARD TABLE OF ty_final_key WITH DEFAULT KEY,
           tt_header  TYPE STANDARD TABLE OF ztt_salesorder WITH DEFAULT KEY.

  DATA: temp_key     TYPE tt_temp_key.


    METHODS: set_temp_key IMPORTING it_temp_key TYPE tt_temp_key,
*                                    IT_so       TYPE zcm_journalitem
*                                    it_so_main  TYPE zcm_journalitem,
      convert_temp_to_final RETURNING VALUE(result) TYPE tt_final_key,
      additional_save IMPORTING it_create type tt_header  OPTIONAL
                                 it_delete TYPE tt_header OPTIONAL
EXPORTING it_documents TYPE tt_final_key,
  clean_up.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZGJE_TRANSACTION_HANDLER02 IMPLEMENTATION.


METHOD additional_save.

    data: lt_create type TABLE of ztt_salesorder.

    DATA(lt_je_key) = convert_temp_to_final(  ).
    it_documents = lt_je_key[].
    loop at it_create into data(ls_create).
      read TABLE lt_je_key into data(ls_je_key) with key cid = ls_create-uuid.
      if sy-subrc = 0.
      ls_create-ebeln = ls_je_key-ebeln.
      append ls_create to lt_create.
      endif.
    ENDLOOP.

    IF lt_create IS NOT INITIAL.
      INSERT ztt_salesorder FROM TABLE @lt_create.
    ENDIF.

    IF it_delete IS NOT INITIAL.
      DELETE ztt_salesorder FROM TABLE @it_delete.
    ENDIF.

  ENDMETHOD.


  METHOD clean_up.
    CLEAR temp_key.
  ENDMETHOD.


  METHOD convert_temp_to_final.
    DATA: ls_final_key TYPE ty_final_key.
    IF temp_key IS NOT INITIAL.
      LOOP AT temp_key INTO DATA(ls_temp_key).
        CONVERT KEY OF i_purchaseordertp_2
          FROM ls_temp_key-pid
          TO FINAL(lv_root_key).

        ls_final_key-cid = ls_temp_key-cid.
*        ls_final_key-bukrs = lv_root_key-companycode.
*        ls_final_key-belnr = lv_root_key-accountingdocument.
*        ls_final_key-gjahr = lv_root_key-fiscalyear.

        APPEND ls_final_key TO result.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_instance.
    IF go_instance IS NOT BOUND.
      go_instance = NEW #( ).
    ENDIF.
    result = go_instance.
ENDMETHOD.


  METHOD set_temp_key.
    temp_key = it_temp_key.
 ENDMETHOD.
ENDCLASS.

CLASS lhc_zr_auto_view DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zr_auto_view RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zr_auto_view RESULT result.

    METHODS autopost FOR MODIFY
      IMPORTING keys FOR ACTION zr_auto_view~autopost RESULT result.

    METHODS grnpost FOR MODIFY
      IMPORTING keys FOR ACTION zr_auto_view~grnpost RESULT result.

    METHODS invpost FOR MODIFY
      IMPORTING keys FOR ACTION zr_auto_view~invpost RESULT result.

ENDCLASS.

CLASS lhc_zr_auto_view IMPLEMENTATION.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD autopost.
  READ ENTITIES OF zr_auto_view IN LOCAL MODE
         ENTITY zr_auto_view ALL FIELDS WITH CORRESPONDING #( keys ) RESULT FINAL(data_read).

        DATA: purchase_orders      TYPE TABLE FOR CREATE i_purchaseordertp_2,
          purchase_order       LIKE LINE OF purchase_orders,
          purchase_order_items TYPE TABLE FOR CREATE i_purchaseordertp_2\_purchaseorderitem,
          purchase_order_item  LIKE LINE OF purchase_order_items,
          lv_matnr             TYPE matnr,
          update_lines         TYPE TABLE FOR UPDATE zr_auto_view,
          update_line          TYPE STRUCTURE FOR UPDATE zr_auto_view.
    DATA:it_po TYPE TABLE OF i_purchaseordertp_2,
         wa_po TYPE i_purchaseordertp_2.


    DATA :purchase_order_description TYPE c LENGTH 40.
    DATA(n1) = 0.
    DATA(n2) = 0.
*    <<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  data:   lt_temp_key TYPE zgje_transaction_handler02=>tt_temp_key,
          ls_temp_key LIKE LINE OF lt_temp_key.

*    <<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    CLEAR:purchase_orders,purchase_order,
         purchase_order_items,purchase_order_item,
         lv_matnr,n1,n2.


    LOOP AT data_read ASSIGNING FIELD-SYMBOL(<fs_final>).
      CLEAR:purchase_orders,purchase_order,
       purchase_order_items,purchase_order_item,
       lv_matnr.

      DATA: lv_ebeln TYPE ebeln.

      n1 += 1.
      purchase_order =  VALUE #( %cid = |My%CID_{ n1 }|
      purchaseordertype      = 'NB'
      companycode            = '0194'
      purchasingorganization = '9401'
      purchasinggroup        = '30'
      supplier               = '1000000001'"<fs_final>-Supplier
      purchaseorderdate      = cl_abap_context_info=>get_system_date( )
                   %control = VALUE #(
                                   purchaseordertype      = cl_abap_behv=>flag_changed
                                   companycode            = cl_abap_behv=>flag_changed
                                   purchasingorganization = cl_abap_behv=>flag_changed
                                   purchasinggroup        = cl_abap_behv=>flag_changed
                                   supplier               = cl_abap_behv=>flag_changed
                                   purchaseorderdate      = cl_abap_behv=>flag_changed
                                                            ) ).
      APPEND purchase_order TO purchase_orders.

      n2 += 1.

      purchase_order_item = VALUE #(  %cid_ref = |My%CID_{ n1 }|
      %target = VALUE #( ( %cid = |My%CID_ITEM{ n2 }|
      material          = <fs_final>-material
      plant             = <fs_final>-plant
      invoiceisgoodsreceiptbased = 'X'
      orderquantity     = <fs_final>-orderedquantity
      purchaseorderitem = '00010'
      netpriceamount    = '0.1'"<fs_final>-rate "'5'
*      PurchasingItemIsFreeOfCharge = 'X'
      goodsreceiptisnonvaluated = 'X'
      documentcurrency  = 'USD'
      purchaserequisition = <fs_final>-purchaserequisition
      purchaserequisitionitem = <fs_final>-purchaserequisitionitem
*      Batch             = 'TEST999111'
                        %control = VALUE #( material          = cl_abap_behv=>flag_changed
                                            plant             = cl_abap_behv=>flag_changed
                                            orderquantity     = cl_abap_behv=>flag_changed
                                            purchaseorderitem = cl_abap_behv=>flag_changed
                                            invoiceisgoodsreceiptbased = cl_abap_behv=>flag_changed
                                            netpriceamount    = cl_abap_behv=>flag_changed
*                                            PurchasingItemIsFreeOfCharge = cl_abap_behv=>flag_changed
                                            goodsreceiptisnonvaluated = cl_abap_behv=>flag_changed
                                            documentcurrency  = cl_abap_behv=>flag_changed
                                            purchaserequisition = cl_abap_behv=>flag_changed
                                            purchaserequisitionitem = cl_abap_behv=>flag_changed
*                                            Batch    = cl_abap_behv=>flag_changed
                                                            ) ) )  ).
      APPEND purchase_order_item TO purchase_order_items.

      "Purchase Order Header Data
      MODIFY ENTITIES OF i_purchaseordertp_2
      ENTITY purchaseorder
      CREATE FROM purchase_orders
      CREATE BY \_purchaseorderitem
      FROM purchase_order_items
      MAPPED DATA(mapped_po_headers)
      REPORTED DATA(reported_po_headers)
      FAILED DATA(failed_po_headers).

      WAIT UP TO 2 SECONDS.
*      <<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>
       LOOP AT mapped_po_headers-purchaseorder INTO DATA(ls_report).
          APPEND VALUE #(
                          %create = if_abap_behv=>mk-on
                       ) TO reported-zr_auto_view.
        ENDLOOP.

        LOOP AT mapped_po_headers-purchaseorder INTO DATA(ls_je_mapped).
        ls_temp_key-cid = ls_je_mapped-%cid.
        ls_temp_key-pid = ls_je_mapped-%pid.
        APPEND ls_temp_key TO lt_temp_key.
      ENDLOOP.

      zgje_transaction_handler02=>get_instance( )->set_temp_key( lt_temp_key ).

*      <<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>
*      zbp_r_auto_view=>mapped_purchase_order-purchaseorder = mapped_po_headers-purchaseorder.
*
*      READ TABLE zbp_r_auto_view=>mapped_purchase_order-purchaseorder INTO DATA(ls) INDEX 1.
*      ls_final_purch = ls.
*      APPEND ls_final_purch TO zbp_r_auto_view=>lt_purchaseorder.
*
*
*
*
*
*
*      LOOP AT keys INTO DATA(key).
*
*        update_line-%tky                   = key-%tky.
*        update_line-ebeln                  = 'X'.
*        APPEND update_line TO update_lines.
*      ENDLOOP.
*      SORT update_lines BY %tky.
*      DELETE ADJACENT DUPLICATES FROM update_lines COMPARING %tky.
*
*      MODIFY ENTITIES OF zr_auto_view IN LOCAL MODE
*            ENTITY zr_auto_view
*              UPDATE
*                FIELDS ( ebeln )
*                WITH update_lines
*                REPORTED reported
*                FAILED failed
*                MAPPED mapped.
*
*      IF failed IS INITIAL.
*        "Read the changed data for action result
*        READ ENTITIES OF zr_auto_view IN LOCAL MODE
*          ENTITY zr_auto_view
*            ALL FIELDS WITH
*            CORRESPONDING #( keys )
*          RESULT DATA(result_read).
*        "return result entities
*        result = VALUE #( FOR result_order IN result_read ( %tky   = result_order-%tky
*                                                            %param = result_order ) ).
*      ENDIF.
    ENDLOOP.



  ENDMETHOD.

  METHOD grnpost.
  READ ENTITIES OF zr_auto_view IN LOCAL MODE
         ENTITY zr_auto_view ALL FIELDS WITH CORRESPONDING #( keys ) RESULT FINAL(data_read).

    SELECT purchaseorder,
 purchaseorderitem
 FROM i_purordaccountassignmentapi01
 FOR ALL ENTRIES IN @data_read
 WHERE salesorder = @data_read-salesorder
 AND   salesorderitem = @data_read-salesorderitem
 INTO TABLE @DATA(it_pur) .

    IF it_pur[] IS NOT INITIAL.
      SELECT a~purchaseorder,
      a~purchaseorderitem,
      a~material,
      a~plant,
      a~netpricequantity
*       a~ItemWeightUnit
      FROM i_purchaseorderitemapi01 AS a
      INNER JOIN i_purchaseorderapi01 AS b ON a~purchaseorder = b~purchaseorder
      FOR ALL ENTRIES IN @it_pur
      WHERE b~creationdate = @sy-datum
      AND   a~purchaseorder = @it_pur-purchaseorder
      AND   a~purchaseorderitem = @it_pur-purchaseorderitem
      INTO TABLE @DATA(i_podata).

    ENDIF.

    DATA st_date TYPE d.

    LOOP AT i_podata INTO DATA(member).


      MODIFY ENTITIES OF i_materialdocumenttp
       ENTITY materialdocument
       CREATE FROM VALUE #( ( %cid = 'CID_001'
       goodsmovementcode          = '01'
       postingdate                = sy-datum "creation_date
       documentdate               = sy-datum
       %control-goodsmovementcode = cl_abap_behv=>flag_changed
       %control-postingdate       = cl_abap_behv=>flag_changed
       %control-documentdate      = cl_abap_behv=>flag_changed
       ) )

         ENTITY materialdocument
         CREATE BY \_materialdocumentitem
         FROM VALUE #( (
         %cid_ref                   = 'CID_001'
         %target                    = VALUE #( ( %cid = 'CID_ITM_001'
         plant                      = member-plant
         material                   = member-material
         goodsmovementtype          = '101'
         storagelocation            = '94US'
         quantityinentryunit        = member-netpricequantity
         entryunit                  = space"member-ItemWeightUnit
         goodsmovementrefdoctype    = 'B'
*         Batch                      = member-Batch
         purchaseorder              = member-purchaseorder
         purchaseorderitem          = member-purchaseorderitem "'00010'
         %control-plant             = cl_abap_behv=>flag_changed
         %control-material          = cl_abap_behv=>flag_changed
         %control-goodsmovementtype = cl_abap_behv=>flag_changed
         %control-storagelocation   = cl_abap_behv=>flag_changed
         %control-quantityinentryunit     = cl_abap_behv=>flag_changed
         %control-entryunit               = cl_abap_behv=>flag_changed
         %control-batch                   = cl_abap_behv=>flag_changed
         %control-purchaseorder           = cl_abap_behv=>flag_changed
         %control-purchaseorderitem       = cl_abap_behv=>flag_changed
         %control-goodsmovementrefdoctype = cl_abap_behv=>flag_changed
         ) )

         ) )
         MAPPED DATA(ls_create_mapped)
         FAILED DATA(ls_create_failed)
         REPORTED DATA(ls_create_reported).

      WAIT UP TO 2 SECONDS.
    ENDLOOP.
  ENDMETHOD.

  METHOD invpost.
  READ ENTITIES OF zr_auto_view IN LOCAL MODE
          ENTITY zr_auto_view ALL FIELDS WITH CORRESPONDING #( keys ) RESULT FINAL(data_read).

    TYPES:BEGIN OF ty_so,
            soldtoparty TYPE kunnr,
          END OF ty_so.
    DATA:it_inv TYPE TABLE OF ztt_salesorder,
         wa_inv TYPE ztt_salesorder,
         it_so  TYPE TABLE OF ty_so,
         wa_so  TYPE ty_so.
    CLEAR:it_inv[],wa_inv.

    LOOP AT data_read ASSIGNING  FIELD-SYMBOL(<fs_final>).
      wa_so-soldtoparty = <fs_final>-soldtoparty.
      COLLECT wa_so INTO it_so.
      CLEAR:wa_so.
    ENDLOOP.

    LOOP AT it_so INTO wa_so.
      CLEAR:it_inv[], wa_inv.
      LOOP AT data_read ASSIGNING <fs_final> WHERE soldtoparty = wa_so-soldtoparty.
        wa_inv-salesorder = <fs_final>-salesorder.
        APPEND wa_inv TO it_inv.
      ENDLOOP.
      MODIFY ENTITIES OF i_billingdocumenttp
       ENTITY billingdocument
       EXECUTE createfromsddocument AUTO FILL CID
       WITH
       VALUE  #(
       ( %param = VALUE #( _reference = VALUE #( FOR ls_final IN it_inv (
                                              sddocument = ls_final-salesorder"'0000000161'
                          %control = VALUE #( sddocument = if_abap_behv=>mk-on ) )
        )

       %control = VALUE #( _reference = if_abap_behv=>mk-on ) ) ) )

       RESULT DATA(lt_result_modify)
       FAILED DATA(ls_failed_modify)
       REPORTED DATA(ls_reported_modify).
      CLEAR:wa_so.
      WAIT UP TO 2 SECONDS.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_zr_auto_view DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zr_auto_view IMPLEMENTATION.

  METHOD save_modified.
   DATA: Lt_create type TABLE of ztt_salesorder,
          lt_delete type TABLE of ztt_salesorder.


*    lt_create = CORRESPONDING #( create-zr_auto_view mapping from entity ).
*    lt_delete = CORRESPONDING #( delete-zr_auto_view mapping from entity ).
    zgje_transaction_handler02=>get_instance( )->additional_save(
*      EXPORTING
*        it_create    =
*        it_delete    =
*      IMPORTING
*        it_documents =
    ).

  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.

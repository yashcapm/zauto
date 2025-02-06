CLASS lhc_zr_auto_um DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zr_auto_um RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zr_auto_um RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE zr_auto_um.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE zr_auto_um.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE zr_auto_um.

    METHODS read FOR READ
      IMPORTING keys FOR READ zr_auto_um RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zr_auto_um.

    METHODS autopost FOR MODIFY
      IMPORTING keys FOR ACTION zr_auto_um~autopost RESULT result.

ENDCLASS.

CLASS lhc_zr_auto_um IMPLEMENTATION.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.
  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD autopost.
      READ ENTITIES OF ZR_AUTO_UM IN LOCAL MODE
         ENTITY ZR_AUTO_UM ALL FIELDS
         WITH CORRESPONDING #( keys ) RESULT FINAL(data_read).

         DATA: purchase_orders      TYPE TABLE FOR CREATE i_purchaseordertp_2,
          purchase_order       LIKE LINE OF purchase_orders,
          purchase_order_items TYPE TABLE FOR CREATE i_purchaseordertp_2\_purchaseorderitem,
          purchase_order_item  LIKE LINE OF purchase_order_items,
          lv_matnr             TYPE matnr.

    DATA :purchase_order_description TYPE c LENGTH 40.
    DATA(n1) = 0.
    DATA(n2) = 0.




    LOOP AT data_read ASSIGNING FIELD-SYMBOL(<fs_final>).
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

*      COMMIT ENTITIES BEGIN RESPONSE OF i_purchaseordertp_2 FAILED DATA(Lt_failed_data) REPORTED DATA(lt_reported1).
* COMMIT ENTITIES END.

   ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lsc_zr_auto_um DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zr_auto_um IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.



  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.

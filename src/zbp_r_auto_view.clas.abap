CLASS zbp_r_auto_view DEFINITION PUBLIC ABSTRACT FINAL FOR BEHAVIOR OF zr_auto_view.

PUBLIC SECTION.
    CLASS-DATA mapped_purchase_order TYPE RESPONSE FOR MAPPED i_purchaseordertp_2.
    CLASS-DATA mapped_material_document TYPE RESPONSE FOR MAPPED i_materialdocumenttp.
    CLASS-DATA mapped_invoice   TYPE RESPONSE FOR MAPPED           i_billingdocumenttp.
    TYPES: ty_t_zclass TYPE STANDARD TABLE OF REF TO ztt_salesorder,
           BEGIN OF ty_purchaseorder,
             cid           TYPE abp_behv_cid,
             pid           TYPE abp_behv_pid,
             purchaseorder TYPE i_purchaseordertp_2-PurchaseOrder,
           END OF ty_purchaseorder,
           tt_purchorder type table for mapped early i_purchaseordertp_2\\purchaseorder.

   CLASS-DATA : lt_purchaseorder  type table for mapped early i_purchaseordertp_2\\purchaseorder.


*    CLASS-DATA mapped_miro TYPE RESPONSE FOR MAPPED i_supplierinvoicetp.
  PROTECTED SECTION.
  PRIVATE SECTION.




ENDCLASS.



CLASS ZBP_R_AUTO_VIEW IMPLEMENTATION.
ENDCLASS.

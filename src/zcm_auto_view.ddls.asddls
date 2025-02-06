@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Basic View for Auto View'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZCM_AUTO_VIEW as select distinct from zcm_auto_inv_base
association [0..1] to I_SalesOrderItem as _SalesItem on $projection.Salesorder = _SalesItem.SalesOrder
                                                     and $projection.Salesorderitem = _SalesItem.SalesOrderItem
association [0..1] to I_SalesDocumentPartner as _Patner on $projection.Salesorder = _Patner.SalesDocument 
                                                        and _Patner.PartnerFunction = 'WE'     
 association [0..1] to I_Customer as _SoldToParty       on  $projection.SoldToParty = _SoldToParty.Customer
 association [0..1] to I_BillingDocumentItem as _BillItem on $projection.Salesorder = _BillItem.SalesDocument
                                                         and $projection.Salesorderitem = _BillItem.SalesDocumentItem 
 association [0..1] to I_Customer as _ShipToParty       on  $projection.Customer = _ShipToParty.Customer                                                                                                       
{
key SalesOrder as Salesorder,
key SalesOrderItem as Salesorderitem,
//key uuid,
min(PurchaseRequisition) as Purchaserequisition,
min(PurchaseRequisitionItem) as Purchaserequisitionitem,
min(Supplier) as Supplier,
min(Material) as Material,
min(Plant) as Plant,
unit,
 @Semantics.quantity.unitOfMeasure : 'unit'
 min(OrderedQuantity) as OrderedQuantity,
 min(SoldToParty) as SoldToParty,
//min(_SoldToParty.CustomerName) as CustomerName,
 min(_SalesItem.CustomerPriceGroup) as CustomerPriceGroup,
 min(_SalesItem.MaterialByCustomer) as MaterialByCustomer,
 min(_SalesItem.PurchaseOrderByShipToParty) as PurchaseOrderByShipToParty,
 min(_Patner.Customer) as Customer,
 min(Study_Id) as Study_Id,
// ebeln,
// mblnr,
// vbeln,
 min(_BillItem.BillingDocument) as BillingDocument,
 min(_BillItem.BillingDocumentItem) as BillingDocumentItem,
 min(MaterialText) as MaterialText,
 min(CompanyCode) as CompanyCode,
 
 
 
 //_ShipToParty.CustomerName as ShipToPartyName,
 
 _SalesItem,
 _BillItem
 
 
 
} 
  group by SalesOrder,SalesOrderItem,unit


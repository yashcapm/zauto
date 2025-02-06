@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Automation Root View'
define root view entity ZR_AUTO_VIEW
 as select from ZCM_AUTO_VIEW
 association [0..1] to I_Customer as _ShipToParty       on  $projection.Customer = _ShipToParty.Customer    
 association [0..1] to I_CustomerPriceGroupText as _GroupText on $projection.CustomerPriceGroup =  _GroupText.CustomerPriceGroup
                                                             and _GroupText.Language  = 'E'    
 association [0..1] to ztt_salesorder_n as _Order on $projection.Salesorder =   _Order.salesorder   
                                                  and $projection.Salesorderitem = _Order.salesorderitem  
 association [0..1] to I_SalesOrder as _SalesOrder on $projection.Salesorder = _SalesOrder.SalesOrder   
 association [0..1] to I_Customer as _SoldToParty       on  $projection.SoldToParty = _SoldToParty.Customer   
 association [0..1] to I_Plant    as _plant             on $projection.Plant =  _plant.Plant                                                                                                                                                                                             
{
key ZCM_AUTO_VIEW.Salesorder,
key ZCM_AUTO_VIEW.Salesorderitem,
//key ZCM_AUTO_VIEW.uuid,
ZCM_AUTO_VIEW.Purchaserequisition,
ZCM_AUTO_VIEW.Purchaserequisitionitem,
ZCM_AUTO_VIEW.Supplier,
ZCM_AUTO_VIEW.Material,
ZCM_AUTO_VIEW.Plant,
ZCM_AUTO_VIEW.unit,
ZCM_AUTO_VIEW.OrderedQuantity,
ZCM_AUTO_VIEW.SoldToParty,
_SoldToParty.CustomerName as SoldToPartyName,
ZCM_AUTO_VIEW.CustomerPriceGroup,
ZCM_AUTO_VIEW.MaterialByCustomer,
_GroupText.CustomerPriceGroupName,
ZCM_AUTO_VIEW.PurchaseOrderByShipToParty,
ZCM_AUTO_VIEW.Customer,
_ShipToParty.CustomerName ,

_Order.ebeln,
case 
 when _Order.ebeln is not null then 'Relesed' else ' ' end as PoStatus,
_Order.mblnr,
case 
 when _Order.mblnr is not initial then 'Relesed' else ' ' end as grnStatus,
 
  
 _Order.vbeln,
BillingDocument,
BillingDocumentItem,
Study_Id,
MaterialText,
CompanyCode,
_plant.PlantName,

/* Associations */
ZCM_AUTO_VIEW._SalesItem,
_ShipToParty,
_GroupText,
_SalesOrder.SalesOrderDate
 
 
} 
where //_Order.vbeln is initial
  BillingDocument is null

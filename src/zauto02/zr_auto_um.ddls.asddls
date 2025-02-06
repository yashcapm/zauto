@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Root View'
define root view entity ZR_AUTO_UM 
as select from ZCM_AUTO_VIEW
 association [0..1] to I_Customer as _ShipToParty       on  $projection.Customer = _ShipToParty.Customer    
 association [0..1] to I_CustomerPriceGroupText as _GroupText on $projection.CustomerPriceGroup =  _GroupText.CustomerPriceGroup
                                                             and _GroupText.Language  = 'E'  
                                                                                                                                                           
{
key ZCM_AUTO_VIEW.Salesorder,
key ZCM_AUTO_VIEW.Salesorderitem,
ZCM_AUTO_VIEW.Purchaserequisition,
ZCM_AUTO_VIEW.Purchaserequisitionitem,
ZCM_AUTO_VIEW.Supplier,
ZCM_AUTO_VIEW.Material,
ZCM_AUTO_VIEW.Plant,
ZCM_AUTO_VIEW.unit,
ZCM_AUTO_VIEW.OrderedQuantity,
ZCM_AUTO_VIEW.SoldToParty,
//ZCM_AUTO_VIEW.soldtopartyName,
ZCM_AUTO_VIEW.CustomerPriceGroup,
ZCM_AUTO_VIEW.MaterialByCustomer,
_GroupText.CustomerPriceGroupName,
ZCM_AUTO_VIEW.PurchaseOrderByShipToParty,
ZCM_AUTO_VIEW.Customer,
_ShipToParty.CustomerName,

/* Associations */
ZCM_AUTO_VIEW._SalesItem,
_ShipToParty,
_GroupText
 
 
}

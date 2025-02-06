@EndUserText.label: 'Projection View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZI_AUTO_UM 
provider contract transactional_query
as projection on ZR_AUTO_UM
{
    key Salesorder,
    key Salesorderitem,
    Purchaserequisition,
    Purchaserequisitionitem,
    Supplier,
    Material,
    Plant,
    unit,
    orderedquantity,
//    soldtoparty,
//    soldtopartyName,
    CustomerPriceGroup,
    MaterialByCustomer,
    CustomerPriceGroupName,
    PurchaseOrderByShipToParty,
    Customer,
    CustomerName,
    /* Associations */
    _GroupText,
    _SalesItem,
    _ShipToParty
}

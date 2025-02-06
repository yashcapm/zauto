@EndUserText.label: 'Automation Projection View'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
    }

define root view entity ZI_AUTO_NEW
provider contract transactional_query
 as projection on ZR_AUTO_VIEW
// association [0..1] to I_Customer as _ShipToParty       on  $projection.Customer = _ShipToParty.Customer  
{
    key Salesorder,
    key Salesorderitem,
//    key UUID,
    Purchaserequisition,
    Purchaserequisitionitem,
    Supplier,
    Material,
    @ObjectModel.text.element: [ 'PlantName' ]
    Plant,
    unit,
    @Semantics.text: true
    PlantName,
 @Semantics.quantity.unitOfMeasure : 'unit'
 OrderedQuantity,
 @ObjectModel.text.element: [ 'SoldToPartyName' ]
 SoldToParty,
 @EndUserText.label: 'Sold To Party'
 @Semantics.text: true
 SoldToPartyName,
 @EndUserText.label: 'Order Type Group'
 CustomerPriceGroup,
 @EndUserText.label: 'DOS Id'
 MaterialByCustomer,
 CustomerPriceGroupName,
// @EndUserText.label: 'Study Id'
 PurchaseOrderByShipToParty,
 @EndUserText.label: 'Ship to Party'
 Customer,
 @EndUserText.label: 'Ship to Party'
 CustomerName,
 @EndUserText.label: 'PO No'
 ebeln,
 @EndUserText.label: 'PO Status'
 PoStatus,
 @EndUserText.label: 'GRN No'
 mblnr,
 @EndUserText.label: 'GRN Status'
 grnStatus,
 @EndUserText.label: 'Invoice'
 vbeln,
 @EndUserText.label: 'Invoice No'
 BillingDocument,
 @EndUserText.label: 'Invoice Item'
 BillingDocumentItem,
  @EndUserText.label: 'Study Id'
 Study_Id,
 @EndUserText.label: 'Material'
 MaterialText,
 CompanyCode,
  @EndUserText.label: 'SO Date'
 SalesOrderDate
 
 
 
 
// _ShipToParty.CustomerName
}

@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Base View'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zcm_auto_inv_base as select from I_SalesOrderItem as a
inner join I_SalesOrder as b on b.SalesOrder = a.SalesOrder
                             and b.OverallSDProcessStatus <> 'C'
                             and b.HeaderBillingBlockReason is initial
left outer join I_SalesOrderScheduleLine as c  on a.SalesOrder = c.SalesOrder
                                                      and a.SalesOrderItem = c.SalesOrderItem
left outer join I_PurchaseRequisitionItemAPI01 as d on c.PurchaseRequisition = d.PurchaseRequisition
                                                           and c.PurchaseRequisitionItem = d.PurchaseRequisitionItem
//inner join ZC_INV_MAIN as e on b.SoldToParty = e.Kunnr
inner join ZC_DATE_UNION as e on b.SoldToParty = e.Kunnr                                                                                        
{
   key a.SalesOrder,
   key a.SalesOrderItem,
      min(b.CreationDate) as CreationDate,
      min(c.PurchaseRequisition) as PurchaseRequisition,
       min(c.PurchaseRequisitionItem) as PurchaseRequisitionItem,
       min(d.Supplier) as Supplier,
       min(d.Material) as Material,
       min(a.SalesOrderItemText) as MaterialText,
       min(a.Plant) as Plant,
       a.BaseUnit as unit,
       @Semantics.quantity.unitOfMeasure : 'Unit'
       min(a.OrderQuantity) as OrderedQuantity,
       min(b.SoldToParty) as SoldToParty,
       min(b.YY1_StudyID_SDH) as Study_Id,
       min(a.MaterialByCustomer) as MaterialByCustomer,
       min(b.BillingCompanyCode) as CompanyCode,
       min(b.CustomerPriceGroup) as CustomerPriceGroup  
} where b.SalesOrderDate <=  e.yesterday
  and   b.CustomerPriceGroup = e.Customerpricegroup
  and   b.BillingCompanyCode = e.Bukrs
  and   a.ItemBillingBlockReason is initial
group by a.SalesOrder,a.SalesOrderItem,a.BaseUnit
  

@AbapCatalog.sqlViewName: 'ZCINVDAY'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Day Wise View'
define view ZC_INV_DAY as select from ZC_INVNEW

{
    key Kunnr,
    Bukrs,
    Customerpricegroup,
    Billcycle,
    Billfre,
    status
    
    

} where Billcycle = 'D'
  

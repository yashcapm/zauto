@AbapCatalog.sqlViewName: 'ZCINVMAIN'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Register'
@ObjectModel.usageType.serviceQuality:#D
@ObjectModel.usageType.dataClass:#MIXED
@ObjectModel.usageType.sizeCategory:#XXL
define view ZC_INV_MAIN as select from ZC_INV_DAY as com
{key Kunnr,
 Bukrs,
 Customerpricegroup,
 Billcycle,
 Billfre,
 status}

union

  select from ZC_INV_mnt as dom
  {key Kunnr,
   Bukrs,
   Customerpricegroup,
   Billcycle,
   Billfre,
   status 
   }




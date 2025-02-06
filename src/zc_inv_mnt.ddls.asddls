@AbapCatalog.sqlViewName: 'ZCINVMNT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Month Wise'
define view ZC_INV_mnt as select from ZC_INVNEW
association[0..1] to ztt_dateno as _Dateno on _Dateno.zday <> '00'
{
    key Kunnr,
    Bukrs,
    Customerpricegroup,
    Billcycle,
    Billfre,
    status
} where Billcycle = 'M'
  and Billfre = _Dateno.zday
  

@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZR_ASHOPCART_000'
define root view entity ZC_ASHOPCART_000
  provider contract transactional_query
  as projection on ZR_ASHOPCART_000
{
  key OrderUUID,
  OrderID,
  OrderedItem,
  Price,
  TotalPrice,
  Currency,
  OrderQuantity,
  DeliveryDate,
  OverallStatus,
  Notes,
  LocalLastChangedAt,
  PurchaseRequisition,
  PrCreationDate
  
}

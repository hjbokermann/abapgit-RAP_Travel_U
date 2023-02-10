@EndUserText.label: 'Projection View Booking Data unmanaged'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Search.searchable: true
@Metadata.allowExtensions: true
define view entity zbc_c_rap_booking_U_007
  as projection on Zbc_I_Rap_Booking_U_007
{
      @Search.defaultSearchElement: true
  key TravelID,
      @Search.defaultSearchElement: true
  key BookingID,
      BookingDate,
      @Consumption.valueHelpDefinition: [ { entity: { name:     '/DMO/I_Customer',  element:     'CustomerID' } } ]
      CustomerID,
      @Consumption.valueHelpDefinition: [ { entity: { name:     '/DMO/I_Carrier',   element:     'AirlineID' } } ]
      CarrierID,
      @Consumption.valueHelpDefinition: [ { entity: { name:    '/DMO/I_Flight',     element: 'ConnectionID' },
                                       additionalBinding: [ { localElement: 'FlightDate',
                                                              element:      'FlightDate',
                                                              usage: #RESULT },
                                                            { localElement: 'CarrierID',
                                                                   element: 'AirlineID',
                                                                     usage: #RESULT },
                                                            { localElement: 'FlightPrice',
                                                                   element: 'Price',
                                                                     usage: #RESULT },
                                                            { localElement: 'CurrencyCode',
                                                                   element: 'CurrencyCode',
                                                                     usage: #RESULT } ]
                                       } ]
      ConnectionID,
      FlightDate,
      FlightPrice,
      @Consumption.valueHelpDefinition: [ {entity: { name:    'I_Currency',
                                               element: 'Currency' } } ]
      CurrencyCode,
      /* Associations */
      _Carrier,
      _Connection,
      _Customer,
      _Flight,
      _Travel : redirected to parent Zbc_C_Rap_Travel_U_007
}

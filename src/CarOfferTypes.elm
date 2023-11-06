module CarOfferTypes exposing (..)
import Http 

type Msg
  = FetchedCSV (Result Http.Error String)
  | SelectChangeX String
  | SelectChangeY String

type Model 
    = Failure
    | Loading 
    | Success CarOfferData

type alias CarOfferData =
    {
        data: List CarOffer
        , yAxis : String
        , xAxis : String
    }
type alias CarOffer =
    { brand : String,
    model : String,
    color : String,
    registration_date : String,
    year : Int,
    price_in_euro : Int,
    power_kw : Int,
    power_ps : Int,
    transmission_type : String,
    fuel_type : String,
    fuel_consumption_l_100km : Float,
    fuel_consumption_g_km : Float,
    mileage_in_km : Float,
    offer_description : String
    }
carOfferAttributes : List String
carOfferAttributes =
    [ "brand"
    , "model"
    , "color"
    , "registration_date"
    , "year"
    , "price_in_euro"
    , "power_kw"
    , "power_ps"
    , "transmission_type"
    , "fuel_type"
    , "fuel_consumption_l_100km"
    , "fuel_consumption_g_km"
    , "mileage_in_km"
    , "offer_description"
    ]
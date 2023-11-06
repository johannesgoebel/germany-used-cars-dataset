module CarOfferTypes exposing (..)
import Http 

type Msg
  = FetchedCSV (Result Http.Error String)

type Model 
    = Failure
    | Loading 
    | Success 
        {data: String}
type alias Point =
    { pointName : String, x : Float, y : Float , k :Float , z: Float}


type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    , autoTyp : List CarOffer --Cartype eigentlich
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

module CarOfferTypes exposing (..)
import Http 

type Msg
  = FetchedCSV (Result Http.Error String)
  | FetchedCSVStarAvg (Result Http.Error String)
  | FetchedCSVStarSum (Result Http.Error String)
  | SelectChangeXScatterplot String
  | SelectChangeYScatterplot String
  | SelectChange1PolarPlot String
  | SelectChange2PolarPlot String
  | SelectChange3PolarPlot String
  | SelectChange4PolarPlot String
  | SelectChangeStarPlot String

type Model 
    = Failure
    | Loading 
    | Success CarOfferData

type alias CarOfferData =
    {
        data: List CarOffer
        , dataStarAvg: List StarData
        , dataStarSum: List StarData
        , yAxis : String
        , xAxis : String
        , firstCoordinate : String
        , secondCoordinate : String
        , thirdCoordinate : String
        , forthCoordinate : String
        , starParameter : String
    }
type alias CarOffer =
    { brand : String,
    model : String,
    color : String,
    registration_date : String,
    year : Int,
    price_in_euro : Float,
    power_kw : Int,
    power_ps : Int,
    transmission_type : String,
    fuel_type : String,
    fuel_consumption_l_100km : Float,
    mileage_in_km : Float,
    offer_description : String,
    length_offer_description : Int
    }
type alias StarData =
    { brand : String,
    year : Float,
    price_in_euro : Float,
    power_kw : Float,
    power_ps : Float,
    fuel_consumption_l_100km : Float,
    mileage_in_km : Float,
    length_offer_description : Float
    }

type alias ParallelAxisCarOffer =
    { pointName : String, values : List Float}
    
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
    , "mileage_in_km"
    , "offer_description"
    , "length_offer_description"
    ]
carOfferAttributesNumeric : List String 
carOfferAttributesNumeric = 
    [
        "year",
        "price_in_euro" ,
        "power_kw" ,
        "power_ps" ,
        "fuel_consumption_l_100km" ,
        "mileage_in_km",
        "length_offer_description"
    ]
carBrandList : List String 
carBrandList = 
    [
        "alfa-romeo"
        ,"aston-martin"
        ,"audi"
        ,"bentley"
        ,"bmw"
        ,"cadillac"
        ,"chevrolet"
       , "chrysler"
       , "citroen"
       , "dacia"
       , "daewoo"
       , "daihatsu"
       , "dodge"
       , "ferrari"
       , "fiat"
       , "ford"
       , "honda"
       , "hyundai"
       , "Infiniti"
       , "Isuzu"
       , "Jaguar"
       , "Jeep"
       , "Kia"
       , "Lada"
       , "Lamborghini"
       , "Lancia"
       , "Land-Rover"
       , "Maserati"
       , "Mazda"
       , "Mercedes-Benz"
       , "Mini"
       , "Mitsubishi"
       , "Nissan"
       , "Opel"
       , "Peugeot"
       , "Porsche"
       , "Proton"
       , "Renault"
       , "Rover"
       , "Saab"
       , "Seat"
       , "Skoda"
       , "Smart"
       , "Ssangyong"
       , "Toyota"
       , "Volkswagen"
       , "Volvo"
    ]
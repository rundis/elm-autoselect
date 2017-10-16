module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (WebData)
import Autoselect

---- MODEL ----


type alias Model =
    { autoSelect : Autoselect.State
    , foundCountries : WebData (List Country)
    , selectedCountry : Maybe Country
    }


init : ( Model, Cmd Msg )
init =
    ( { autoSelect = Autoselect.defaultState |> Autoselect.timeToSettle 200
      , foundCountries = RemoteData.NotAsked
      , selectedCountry = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | AutoselectMsg Autoselect.Msg
    | CountriesResponse (WebData (List Country))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CountriesResponse webdata ->
            ( { model | foundCountries = webdata }, Cmd.none )

        AutoselectMsg subMsg ->
            let
                ( autoState, returnMsg, autoCmd ) =
                    Autoselect.update subMsg model.autoSelect

                ( updModel, returnCmd ) =
                    handleAutoselectReturn returnMsg { model | autoSelect = autoState }
            in
                ( updModel, Cmd.batch [ Cmd.map AutoselectMsg autoCmd, returnCmd ] )



handleAutoselectReturn : Autoselect.ReturnMsg -> Model -> ( Model, Cmd Msg)
handleAutoselectReturn msg model =
    case msg of
        Autoselect.None ->
            ( model, Cmd.none )

        Autoselect.OnSelectItem id ->
            ( { model
                | foundCountries = RemoteData.NotAsked
                , selectedCountry = selectCountry id model.foundCountries
              }
            , Cmd.none
            )

        Autoselect.OnRemoveSelected ->
            ( { model | selectedCountry = Nothing }, Cmd.none )

        Autoselect.OnSearch query ->
            if String.length (String.trim query) > 1 then
                ( { model | foundCountries = RemoteData.Loading }
                , searchCountries query
                )
            else
                ( { model | foundCountries = RemoteData.NotAsked }
                , Cmd.none
                )

selectCountry : String -> WebData (List Country) -> Maybe Country
selectCountry countryCode webCountries =
    RemoteData.toMaybe webCountries
        |> Maybe.andThen
            (\countries ->
                List.filter (\c -> c.code == countryCode) countries
                    |> List.head
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ style [("width", "500px"), ("margin-left", "auto"), ("margin-right", "auto")]]
        [ h1 [] [ text "Lets autoselect" ]
        , div []
            [ input [ type_ "text"] [] ]
        , p [] [ text "Some text for fun "]
        , Autoselect.view model.autoSelect (autoSelectData model) autoSelectConfig
        , p [] [ text "Some more text for fun "]
        , div []
            [ input [ type_ "text"] [] ]
        ]


autoSelectData : Model -> Autoselect.ViewData Country
autoSelectData model =
    case model.foundCountries of
            RemoteData.Success countries ->
                { availableItems = countries
                , selectedItem = model.selectedCountry
                }
            _ ->
                { availableItems = []
                , selectedItem = model.selectedCountry
                }



autoSelectConfig : Autoselect.ViewConfig Country Msg
autoSelectConfig =
    Autoselect.viewConfig
        { toMsg = AutoselectMsg
        , inputId = "myautoselect"
        , placeholder = "Enter countryname"
        , idFn = \c -> c.code
        , itemFn = \c -> {attributes = [], children = [ text c.name] } -- { attributes = [], [ text c.name ] }
        , selectedFn = \c -> {attributes = [], children = [ text c.name] }
        }





type alias Country =
    { code : String
    , name : String
    }

searchCountries : String -> Cmd Msg
searchCountries term =
    Http.get ("https://restcountries.eu/rest/v2/name/" ++ term) countriesDecoder
        |> RemoteData.sendRequest
        |> Cmd.map CountriesResponse



countriesDecoder :  Decoder (List Country)
countriesDecoder =
    Decode.list <|
        Decode.map2 Country
            (Decode.field "alpha3Code" Decode.string)
            (Decode.field "name" Decode.string)








---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

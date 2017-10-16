module Autoselect
    exposing
        ( view
        , update
        , defaultState
        , timeToSettle
        , viewConfig
        , State
        , Msg
        , SelectedItem
        , MenuItem
        , ViewConfig
        , ViewData
        , ReturnMsg(..)
        )

{-| [Select2](https://select2.github.io/examples.html)-inspired autocompleter for Elm.

**Key features**:

  - Fully stylable (currently only through css classes)
  - Flexible presentation of selected item and dropdown items
  - All state management of underlying items data is managed outside of the Autocomplete module
      - So it's agnostic about how you implement your search logic (in-memory, http/remotedata etc)
      - No duplication of state
  - Optionally supports debouncing (handy for remote/ajax searches)
  - Keyboard support for selecting items, moving through available selections, closing menu etc
  - Scrolling of selection list menu when configured to be smaller than size of presented items


## Example

    module Main exposing (main)

    import Html exposing (..)
    import Html.Attributes exposing (..)
    import Autocomplete as Autocomplete


    main : Program Never Model Msg
    main =
        Html.program
            { view = view
            , init = init
            , update = update
            , subscriptions = \_ -> Sub.none
            }


    -- MODEL

    type alias Model =
        { autoState : Autocomplete.State
        , selectedCompany : Maybe Company
        , companies : List Company
        }


    type alias Company =
        { id : String
        , name : String
        }


    init : ( Model, Cmd Msg )
    init =
        ( { autoState = Autocomplete.defaultState
          , selectedCompany = Nothing
          , companies = []
          }
        , Cmd.none
        )


    -- UPDATE
    type Msg
        = AutocompleteMsg


    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            AutocompleteMsg autoMsg ->
                let
                    (autoState, returnMsg, cmd)
                        = Autocomplete.update autoMsg model.autoState

                    newModel =
                        { model | autoState = autoState }

                    autoCmd =
                        Cmd.map AutocompleteMsg cmd
                in
                    ( handleAutocompleteReturn returnMsg newModel
                    , autoCmd
                    )



    handleAutocompleteReturn : Autocomplete.ReturnMsg -> Model -> Model
    handleAutocompleteReturn msg model =
        case msg of
            Autocomplete.None ->
                model

            Autocomplete.OnSearch query =
                { model | companies = searchCompanies query }

            Autocomplete.OnSelectItem id ->
                { model | selectedCompany = selectCompany id }

            Autocomplete.RemoveSelected ->
                { model | selectedCompany = Nothing }


    searchCompanies : String -> List Company
    searchCompanies term =
        List.filter
            (\company -> String.contains term company.name)
            dummyCompanies


    selectCompany : String -> Maybe Company
    selectCompany id =
        List.filter (\company -> company.id == id ) dummyCompanies
            |> List.head


    dummyCompanies : List Company
    dummyCompanies =
        [ Company 1 "Alfa Core"
        , Company 2 "Beta"
        -- etc
        ]



    ---- VIEW ----


    view : Model -> Html Msg
    view model =
        div
            []
            [ h1 [] [ text "Autocompleter " ]
            , div
                [ style
                    [ ( "width", "500px" )
                    ]
                ]
                [ Autocomplete.view
                    model.autoState
                    (autoViewdata model)
                    autoViewConfig
                ]
            , div []
                [ button [] [ text "Push me"]]
            ]


    autoViewdata : Model -> Autocomplete.ViewData Company
    autoViewdata model =
        { availableItems = model.companies
        , selectedItem = model.selectedCompany
        }


## State

@docs defaultState, timeToSettle, State


## Update

@docs update, ReturnMsg, Msg


## View

@docs view, viewConfig, SelectedItem, MenuItem, ViewConfig, ViewData

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (onInput, onClick, onBlur)
import Html.Keyed as Keyed
import Json.Decode as Decode
import DOM as DomUtils
import Dom.Scroll as Scroll
import Dom
import Task
import Process
import Time exposing (Time)


-- MODEL


{-| Opaque state that is used internally within the autocomplete module.
-}
type State
    = State StateRec


type alias StateRec =
    { containerRect : Maybe DomUtils.Rectangle
    , showMenu : Bool
    , activeIdx : Int
    , query : String
    , debounceInfo : Maybe DebounceInfo
    , preventBlur : Bool
    }


type alias DebounceInfo =
    { timeToSettle : Time
    , sleepCount : Int
    }


{-| Initialize the autocomplete state with defaults.

A great place invoke this function would be your `init` function.

-}
defaultState : State
defaultState =
    State
        { showMenu = False
        , activeIdx = 0
        , containerRect = Nothing
        , query = ""
        , debounceInfo = Nothing
        , preventBlur = False
        }


{-| Allows you to define a settle down period (in milliseconds) before triggering searches when users type really fast.

If you are doing an in-memory search, this setting doesnt't make much sense, but if you are
doing ajax/remote requests to do the actual search this is a very useful feature.

    autoState =
        Autocomplete.defaultState
            |> Autocomplete.timeToSettle 250

With the value set above, the autocompleter will only give `OnSearch` return messages
once the user has stopped typing for 250 milliseconds. If you're handling of OnSearch triggers a http request,
you'll be able to skip a whole bunch of requests that most likely wouldn't have had time to display before firing of the next.

-}
timeToSettle : Int -> State -> State
timeToSettle tts (State state) =
    State { state | debounceInfo = Just { sleepCount = 0, timeToSettle = toFloat tts * Time.millisecond } }



-- UPDATE


{-| Opaque type for all the module internal messages in the Autocompleter.
-}
type Msg
    = NoOp
    | SetQuery String
    | Timeout Int
    | SetActiveItem Int
    | SelectItem String
    | RemoveSelected InputId
    | OpenMenu InputId
    | CloseMenu InputId
    | PreventBlur Bool
    | OnBlur
    | Move MoveDirection Int ScrollInfo MenuId
    | CalculateRect DomUtils.Rectangle


type MoveDirection
    = Up
    | Down


{-| These are the messages that may be returned from the update function.

  - `OnSearch String` - This message is returned when the user performs an action that should result
    in a search (typically typing something)
  - `OnSelectItem String` - Returned when the user selects an item. The string is the id for the item based on the `idFn` field/function given in [`viewConfig`](viewConfig)
  - `OnRemoveSelected` - Returned when the user removes the selected item

-}
type ReturnMsg
    = None
    | OnSearch String
    | OnSelectItem String
    | OnRemoveSelected


{-| The update function steps the autocomplete state forward. You must remember
to call this function from your own update function.

It's similar to a standard TEA update function, but it returns one more piece of information.
It also returns a `ReturnMsg` such that you may respond to state changes that are considered
out of bounds for this module to handle. If it makes it easier, we can pretend that these are callbacks.
(but really, they are just values returned from a function that happens to be called update).

-}
update : Msg -> State -> ( State, ReturnMsg, Cmd Msg )
update msg (State state) =
    (case msg of
        NoOp ->
            ( state, None, Cmd.none )

        SetQuery query ->
            case state.debounceInfo of
                Just debounceInfo ->
                    let
                        newCount =
                            debounceInfo.sleepCount + 1
                    in
                        ( { state | activeIdx = 0, query = query } |> updateSleepCount newCount
                        , None
                        , Process.sleep debounceInfo.timeToSettle
                            |> Task.perform (always (Timeout newCount))
                        )

                Nothing ->
                    ( { state | activeIdx = 0, query = query }
                    , OnSearch query
                    , Cmd.none
                    )

        Timeout count ->
            case state.debounceInfo of
                Just debounceInfo ->
                    if count == debounceInfo.sleepCount then
                        ( updateSleepCount 0 state
                        , OnSearch state.query
                        , Cmd.none
                        )
                    else
                        ( state, None, Cmd.none )

                Nothing ->
                    ( state, None, Cmd.none )

        SetActiveItem idx ->
            ( { state | activeIdx = idx }
            , None
            , Cmd.none
            )

        SelectItem itemId ->
            ( { state | showMenu = False, query = "" } |> updateSleepCount 0
            , OnSelectItem itemId
            , Cmd.none
            )

        RemoveSelected inputId ->
            ( { state | showMenu = True }
            , OnRemoveSelected
            , requestFocus inputId
            )

        OpenMenu inputId ->
            ( { state | showMenu = True }
            , None
            , requestFocus inputId
            )

        CloseMenu inputId ->
            ( { state | showMenu = False, query = "" } |> updateSleepCount 0
            , OnSearch ""
            , Cmd.none
            )

        PreventBlur val ->
            ( { state | preventBlur = val }, None, Cmd.none )

        OnBlur ->
            if state.preventBlur then
                ( state, None, Cmd.none )
            else
                ( { state | showMenu = False, query = "" } |> updateSleepCount 0
                , OnSearch ""
                , Cmd.none
                )

        Move dir activeIdx scrollInfo menuId ->
            ( { state | activeIdx = activeIdx }
            , None
            , requestScroll dir scrollInfo menuId
            )

        CalculateRect rect ->
            ( { state | containerRect = Just rect }
            , None
            , Cmd.none
            )
    )
        |> (\( newState, returnMsg, cmd ) ->
                ( State newState, returnMsg, cmd )
           )


updateSleepCount : Int -> StateRec -> StateRec
updateSleepCount count state =
    case state.debounceInfo of
        Just debounceInfo ->
            { state | debounceInfo = Just { debounceInfo | sleepCount = count } }

        Nothing ->
            state


requestScroll : MoveDirection -> ScrollInfo -> MenuId -> Cmd Msg
requestScroll dir { menuHeight, scrollTop, nextItem, previousItem } menuId =
    case ( dir, menuHeight, scrollTop, nextItem, previousItem ) of
        ( Down, Just mh, Just st, Just next, _ ) ->
            if (next.offsetTop + next.offsetHeight - mh) > st then
                requestScrollCmd (next.offsetTop + next.offsetHeight - mh) menuId
            else
                Cmd.none

        ( Up, Just mh, Just st, _, Just prev ) ->
            if prev.offsetTop < st then
                requestScrollCmd (st - prev.offsetHeight) menuId
            else
                Cmd.none

        _ ->
            Cmd.none


requestScrollCmd : Float -> MenuId -> Cmd Msg
requestScrollCmd scrollTo (MenuId id) =
    Scroll.toY id scrollTo
        |> Task.attempt (\_ -> NoOp)


requestFocus : InputId -> Cmd Msg
requestFocus (InputId id) =
    Dom.focus id
        |> Task.attempt (\_ -> NoOp)



-- VIEW


{-| Opaque type representing the view configuration for the autocompleter.
-}
type ViewConfig data msg
    = ViewConfig (ViewConfigRec data msg)


type alias ViewConfigRec data msg =
    { toMsg : Msg -> msg
    , inputId : InputId
    , placeholder : String
    , idFn : data -> String
    , itemFn : data -> MenuItem Never
    , selectedFn : data -> SelectedItem Never
    }


type InputId
    = InputId String


type MenuId
    = MenuId String


{-| Type alias describing a menu item.
Gives you pretty good control on how you would like to present your autocomplete menu items.
-}
type alias MenuItem msg =
    { attributes : List (Html.Attribute msg)
    , children : List (Html.Html msg)
    }


{-| Once a user has selected an item, you'll need to describe how the selected items should look.
This type alias contains the necessary view details.
-}
type alias SelectedItem msg =
    { attributes : List (Html.Attribute msg)
    , children : List (Html.Html msg)
    }


{-| The autocompleter doesn't know anything about your autocompletion data and how they are represented (or stored).
It only expects to get a list of something and maybe a selected something.

This type alias captures that and you pass viewData as a param to the view function.

-}
type alias ViewData data =
    { availableItems : List data
    , selectedItem : Maybe data
    }


inputId : String -> InputId
inputId id =
    InputId id


{-| To be able to work with your very generic data, you need to provide
a little bit of config information such that the autocompleter can present your
items properly (and in a type safe manner).

  - toMsg - A message constructor (see example at the top for inspiration)
  - inputId - To be able to focus, scroll etc we need the autocomplete to have a unique id
  - idFn - A function that can take your item data type and produce an (item) id
  - itemFn - A function that can take your item data type and produce a [`MenuItem`](#MenuItem). This is used when creatong the menu li elements.
  - selectedFn - A function that takes your item data type and produces a [`SelectedItem`](#SelectedItem). This is used when displaying a selected value.

-}
viewConfig :
    { toMsg : Msg -> msg
    , inputId : String
    , placeholder : String
    , idFn : data -> String
    , itemFn : data -> MenuItem Never
    , selectedFn : data -> SelectedItem Never
    }
    -> ViewConfig data msg
viewConfig config =
    ViewConfig
        { toMsg = config.toMsg
        , inputId = InputId config.inputId
        , placeholder = config.placeholder
        , idFn = config.idFn
        , itemFn = config.itemFn
        , selectedFn = config.selectedFn
        }


{-| The view function for displaying the autocompleter.

It takes a few parameters. It's really important to note that the autocomplete
module doesn't know anything about what items you are presenting and it doesn't keep track of them
in state. Therefore you need to pass state explicitly in separate params.

  - `state` - This is the module internal state (active index in dropdown list, scrollposition etc). This state you should store in your model.
  - `viewData` - This is external state representing the items to be shown in the autocompleter (selected item and items to be displayed in the dropdown `ul`)
  - `viewConfig` - This is configuration related to displaying stuff you need to pass on. You shold **deffo** not store this in you model.

-}
view : State -> ViewData data -> ViewConfig data msg -> Html msg
view ((State ({ showMenu } as stateRec)) as state) viewData (ViewConfig viewConfig) =
    let
        isShowMenu =
            showMenu && List.length viewData.availableItems > 0
    in
        div [ class "autocomplete-container" ]
            ([ div
                [ classList
                    [ ( "autocomplete-selected-item", True )
                    , ( "autocomplete-menu-open", isShowMenu )
                    ]
                ]
                (case viewData.selectedItem of
                    Just item ->
                        [ viewSelectedItem item viewConfig |> Html.map viewConfig.toMsg
                        , viewSymbols
                            [ viewRemove viewConfig.inputId
                            , viewArrow isShowMenu
                            ]
                            |> Html.map viewConfig.toMsg
                        ]

                    Nothing ->
                        [ viewInput stateRec viewData viewConfig |> Html.map viewConfig.toMsg
                        , viewSymbols [ viewArrow isShowMenu ]
                            |> Html.map viewConfig.toMsg
                        ]
                )
             ]
                ++ if isShowMenu then
                    [ viewMenu stateRec viewData viewConfig |> Html.map viewConfig.toMsg ]
                   else
                    []
            )


viewSelectedItem : data -> ViewConfigRec data msg -> Html Msg
viewSelectedItem selected viewConfig =
    let
        item =
            viewConfig.selectedFn selected
    in
        div (class "autocomplete-selected-item-value" :: (List.map mapNeverToMsg item.attributes))
            (List.map (Html.map (\_ -> NoOp)) item.children)


viewSymbols : List (Html Msg) -> Html Msg
viewSymbols items =
    div [ class "autocomplete-symbols" ] items


viewArrow : Bool -> Html Msg
viewArrow isShowMenu =
    div [ classList [ ( "autocomplete-arrow-up", isShowMenu ), ( "autocomplete-arrow-down", not isShowMenu ) ] ]
        []


viewRemove : InputId -> Html Msg
viewRemove inputId =
    button
        [ type_ "button"
        , class "autocomplete-btn"
        , onClick <| RemoveSelected inputId
        ]
        [ div [ class "autocomplete-cross" ] [] ]


viewInput : StateRec -> ViewData data -> ViewConfigRec data msg -> Html Msg
viewInput state viewData viewConfig =
    let
        (InputId id) =
            viewConfig.inputId
    in
        input
            [ Html.Attributes.id id
            , type_ "text"
            , placeholder viewConfig.placeholder
            , class "autocomplete-input"
            , value state.query
            , onInput SetQuery
            , Events.on "focus" <| Decode.succeed <| OpenMenu viewConfig.inputId
            , Events.on "blur" <| Decode.succeed <| OnBlur
            , onKeyDown state viewData viewConfig
            ]
            []


onKeyDown : StateRec -> ViewData data -> ViewConfigRec data msg -> Html.Attribute Msg
onKeyDown state viewData ({ idFn } as config) =
    let
        menuId =
            toMenuId config.inputId

        handleMenuOpen info =
            case info.keyCode of
                13 ->
                    onEnter state viewData config

                40 ->
                    Decode.succeed <|
                        Move Down (getNextIndex state viewData) info menuId

                38 ->
                    Decode.succeed <|
                        Move Up (getPreviousIndex state viewData) info menuId

                27 ->
                    Decode.succeed (CloseMenu config.inputId)

                _ ->
                    Decode.fail "passthrough"
    in
        Events.onWithOptions "keydown"
            { preventDefault = True
            , stopPropagation = True
            }
            (keyDownDecoder state.activeIdx
                |> Decode.andThen handleMenuOpen
            )


onEnter : StateRec -> ViewData data -> ViewConfigRec data msg -> Decode.Decoder Msg
onEnter state viewData ({ idFn } as config) =
    case getActiveItem state viewData config of
        Just item ->
            Decode.succeed <| SelectItem (idFn item)

        Nothing ->
            Decode.fail "No selection"


type alias ScrollInfo =
    { keyCode : Int
    , menuHeight : Maybe Float
    , scrollTop : Maybe Float
    , nextItem : Maybe ItemInfo
    , previousItem : Maybe ItemInfo
    }


type alias ItemInfo =
    { offsetHeight : Float
    , offsetTop : Float
    }


keyDownDecoder : Int -> Decode.Decoder ScrollInfo
keyDownDecoder idx =
    Decode.map5 ScrollInfo
        Events.keyCode
        (Decode.maybe <|
            Decode.at (menuPath ++ [ "offsetHeight" ]) Decode.float
        )
        (Decode.maybe <|
            Decode.at (menuPath ++ [ "scrollTop" ]) Decode.float
        )
        (Decode.maybe <|
            Decode.at (menuPath ++ [ "childNodes", toString <| idx + 1 ]) itemDecoder
        )
        (Decode.maybe <|
            Decode.at (menuPath ++ [ "childNodes", toString <| idx - 1 ]) itemDecoder
        )


itemDecoder : Decode.Decoder ItemInfo
itemDecoder =
    Decode.map2 ItemInfo
        (Decode.field "offsetHeight" Decode.float)
        (Decode.field "offsetTop" Decode.float)


menuPath : List String
menuPath =
    [ "target", "parentElement", "parentElement", "childNodes", "1" ]


viewMenu : StateRec -> ViewData data -> ViewConfigRec data msg -> Html Msg
viewMenu state viewData viewConfig =
    Keyed.ul
        [ class "autocomplete-menu"
        , id <| menuIdStr viewConfig.inputId
        ]
        (List.indexedMap
            (\idx item -> menuItem state viewConfig idx item)
            viewData.availableItems
        )


menuItem : StateRec -> ViewConfigRec data msg -> Int -> data -> ( String, Html.Html Msg )
menuItem state ({ itemFn, idFn } as config) idx item =
    let
        itemDetails =
            itemFn item

        itemId =
            idFn item
    in
        ( itemId
        , li
            (List.map mapNeverToMsg itemDetails.attributes
                ++ [ classList
                        [ ( "autocomplete-menu-item", True )
                        , ( "autocomplete-menu-item-active", state.activeIdx == idx )
                        ]
                   , href "#"
                   , Events.on "mouseenter" <| Decode.succeed <| SetActiveItem idx
                   , Events.on "mousedown" <| Decode.succeed <| PreventBlur True
                   , Events.on "mouseup" <| Decode.succeed <| PreventBlur False
                   , Events.onWithOptions "click"
                        { preventDefault = True
                        , stopPropagation = True
                        }
                        (Decode.succeed <| SelectItem itemId)
                   ]
            )
            (List.map (Html.map (\_ -> NoOp)) itemDetails.children)
        )


getNextIndex : StateRec -> ViewData data -> Int
getNextIndex { activeIdx } { availableItems } =
    if activeIdx < (List.length availableItems) - 1 then
        activeIdx + 1
    else
        activeIdx


getPreviousIndex : StateRec -> ViewData data -> Int
getPreviousIndex { activeIdx } { availableItems } =
    if activeIdx > 0 then
        activeIdx - 1
    else
        activeIdx


getActiveItem : StateRec -> ViewData data -> ViewConfigRec data msg -> Maybe data
getActiveItem { activeIdx } { availableItems } { idFn } =
    List.indexedMap (,) availableItems
        |> List.filter (\x -> Tuple.first x == activeIdx)
        |> List.map Tuple.second
        |> List.head


indexOf : (data -> String) -> data -> List data -> Maybe Int
indexOf idFn item items =
    List.indexedMap (,) items
        |> List.filter (\( _, item_ ) -> idFn item_ == idFn item)
        |> List.map Tuple.first
        |> List.head


menuIdStr : InputId -> String
menuIdStr (InputId id) =
    id ++ "-menu"


toMenuId : InputId -> MenuId
toMenuId inputId =
    MenuId <| menuIdStr inputId


mapNeverToMsg : Html.Attribute Never -> Html.Attribute Msg
mapNeverToMsg msg =
    Html.Attributes.map (\_ -> NoOp) msg

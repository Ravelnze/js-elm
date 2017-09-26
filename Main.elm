module BootstrapLayout exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Navigation exposing (Location)
import UrlParser exposing ((</>))
import Bootstrap.CDN as CDN
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Card as Card
import Bootstrap.Button as Button
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Modal as Modal


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }


type alias Model =
    { page : Page
    , navState : Navbar.State
    , modalState : Modal.State
    }


type Page
    = Home
    | Reception
    | Music
    | NotFound


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate location { navState = navState, page = Home, modalState = Modal.hiddenState }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )


type Msg
    = UrlChange Location
    | NavMsg Navbar.State
    | ModalMsg Modal.State


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            urlUpdate location model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        ModalMsg state ->
            ( { model | modalState = state }
            , Cmd.none
            )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    case decode location of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Location -> Maybe Page
decode location =
    UrlParser.parseHash routeParser location


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map Reception (UrlParser.s "reception")
        , UrlParser.map Music (UrlParser.s "music")
        ]


view : Model -> Html Msg
view model =
    div []
        [ CDN.stylesheet
        , customStyle
        , menu model
        , mainContent model
        , modal model
        ]


customStyle : Html msg
customStyle = 
    node "link"
        [ rel "stylesheet" 
        , href "css/site.css"
        ]
        []


header : Model -> Html Msg
header model = 
    Grid.row [ Row.attrs [ class "text-center", class "page-header" ] ]
        [ Grid.col [ Col.attrs [ class "" ] ] 
            [ h1 
                [ class "tangerine-header" 
                , title "Wedding DJ & Guitarist"
                ] 
                [ text "John Sutcliffe" ] 
            , h4 []
                 [ a [ href "http://www.easyweddings.com.au/WeddingMusic/Perth/JohnSutcliffeWeddingGuitaristandDJ/"
                     , target "_blank" 
                     ] 
                     [ text "Wedding DJ & Guitarist" ] 
                 ]
            ]
        ]

menu : Model -> Html Msg
menu model =
    Navbar.config NavMsg
        |> Navbar.brand 
            [ href "#"
            , class "tangerine-header" 
            , title "Wedding DJ & Guitarist"
            ] 
            [ text "John Sutcliffe" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#" ] [ text "Home" ]
            , Navbar.itemLink [ href "#reception" ] [ text "Reception" ]
            , Navbar.itemLink [ href "#music" ] [ text "Music" ]
            ]
        |> Navbar.customItems [
            Navbar.textItem 
                [] 
                [ a 
                    [ href "http://www.easyweddings.com.au/WeddingMusic/Perth/JohnSutcliffeWeddingGuitaristandDJ/" 
                    , target "_blank"
                    , class "nav-right" 
                    ] 
                    [ text "Easy Weddings" ] 
                ]
        ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.containerFluid [] <|
        case model.page of
            Home ->
                welcome model

            Reception ->
                pageReception model

            Music ->
                pageMusic model

            NotFound ->
                pageNotFound


welcome : Model -> List (Html Msg)
welcome model =
    [ Grid.row []
        [ Grid.col 
            [ Col.attrs 
                [ class "black-bg"
                , class "remove-padding"
                ] 
            ]
            [ img 
                [ class "img-responsive"
                , src "img/Cover.png"
                ] 
                []
            ]
        ]
    ]


pageReception : Model -> List (Html Msg)
pageReception model =
    [ h2 [] [ text "Getting started" ]
    , Button.button
        [ Button.success
        , Button.large
        , Button.block
        , Button.attrs [ onClick <| ModalMsg Modal.visibleState ]
        ]
        [ text "Click me" ]
    ]


pageMusic : Model -> List (Html Msg)
pageMusic model =
    [ h1 [] [ text "Modules" ]
    , Listgroup.ul
        [ Listgroup.li [] [ text "Alert" ]
        , Listgroup.li [] [ text "Badge" ]
        , Listgroup.li [] [ text "Card" ]
        ]
    ]


pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "SOrry couldn't find that page"
    ]


modal : Model -> Html Msg
modal model =
    Modal.config ModalMsg
        |> Modal.large
        |> Modal.h4 [] [ text "Getting started ?" ]
        |> Modal.body []
            [ Grid.containerFluid []
                [ Grid.row []
                    [ Grid.col
                        [ Col.xs6 ]
                        [ text "Col 1" ]
                    , Grid.col
                        [ Col.xs6 ]
                        [ text "Col 2" ]
                    ]
                ]
            ]
        |> Modal.view model.modalState


module BootstrapLayout exposing (main)

import Html exposing (..)
import Date exposing (Date)
import Html.Attributes exposing (..)
import Navigation exposing (Location)
import UrlParser exposing ((</>))
import Bootstrap.CDN as CDN
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Carousel as Carousel exposing (defaultStateOptions)
import Bootstrap.Carousel.Slide as Slide
import Bootstrap.Form as Form


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
    , carouselState: Carousel.State
    , modalState : Modal.State
    , firstName : String
    , lastName : String
    , email : String
    , phone : Int
    , venue : String
    -- , date : Date
    , comments : String 
    }


type Page
    = Home
    | Reception
    | Music
    | Contact
    | NotFound


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate location 
                { navState = navState
                , page = Home
                , carouselState = Carousel.initialStateWithOptions
                    { defaultStateOptions
                        | interval = Just 30000
                    }
                , modalState = Modal.hiddenState 
                , firstName = ""
                , lastName = ""
                , email = ""
                , phone = 0
                , venue = ""
                -- , date = date???
                , comments = ""
                }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )


type Msg
    = UrlChange Location
    | NavMsg Navbar.State
    | CarouselMsg Carousel.Msg
    | ModalMsg Modal.State


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Navbar.subscriptions model.navState NavMsg
    , Carousel.subscriptions model.carouselState CarouselMsg
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            urlUpdate location model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        CarouselMsg subMsg ->
            ( { model | carouselState = Carousel.update subMsg model.carouselState }
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
        , UrlParser.map Contact (UrlParser.s "contact")
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
            , Navbar.itemLink [ href "#contact" ] [ text "Enquiries" ]
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


footerContent : Model -> Html Msg
footerContent model = 
    footer 
        [ class "footer" ] 
        [ p [] 
            [ text "Phone: "
            , a [] [ text "0410 925 297 | " ] 
            , text "Email: "
            , a [] [ text "Click Here | " ]
            , text "A "
            , a [ href "http://pyweb.com.au" ] [ text "PyWeb " ]
            , text "Technology"
            ]
        ]


mainContent : Model -> Html Msg
mainContent model =
    Grid.containerFluid [] <|
        case model.page of
            Home ->
                pageWelcome model

            Reception ->
                pageReception model

            Music ->
                pageMusic model

            Contact ->
                pageContact model

            NotFound ->
                pageNotFound


pageWelcome : Model -> List (Html Msg)
pageWelcome model =
    [ Grid.row []
        [ Grid.col 
            [ Col.attrs 
                [ class "black-bg"
                , class "remove-padding"
                ] 
            ]
            [ img 
                [ class "img-fluid"
                , src "img/Cover.png"
                ] 
                []
            ]
        ]
    , Grid.row []
        [ Grid.col [ Col.sm1 ] []
        , Grid.col [] 
            [ blockquote 
                [ class "blockquote cream-border" ] 
                [ h3 
                    [ class "upper-pad" ] 
                    [ text "Welcome" ] 
                ] 
            ]
        ]
    , Grid.row []
        [ Grid.col [ Col.xs1, Col.sm2 ] []
        , Grid.col [ Col.xs11, Col.sm8 ]
            [ p 
                [ class "lg-text" ] 
                [ text "John Sutcliffe is an experienced and classically trained guitarist specialising in wedding receptions. John plays live Jazz, Latin and Lounge guitar, providing the perfect ambience while you dine with your wedding guests." ] 
            , p 
                [ class "lg-text" ] 
                [ text "Your reception continues with John's personalised DJ service, having planned every detail with you prior to the night." ]
            ]
        ]
    ]


pageReception : Model -> List (Html Msg)
pageReception model =
    [ Grid.row []
        [ Grid.col [ Col.sm1 ] []
        , Grid.col [] 
            [ blockquote 
                [ class "blockquote cream-border" ] 
                [ h3 
                    [ class "upper-pad" ] 
                    [ text "Your Reception" ] 
                ] 
            ]
        ]
    , Grid.row []
        [ Grid.col [ Col.xs1, Col.sm2 ] []
        , Grid.col [ Col.xs11, Col.sm7, Col.attrs [ class "md-text" ] ]
            [ p 
                [] 
                [ text "From the venue to the table centre pieces, your wedding reception is unique to you. The music at your reception should also be exactly as you desire which John delivers with his comprehensive service. John organises everything you require, including your first dance song, music during the wedding games, the last song of the night, and even the exact music you want playing as you cut your wedding cake." ] 
            , br [] []
            , p 
                [] 
                [ text "Your reception with John includes:" ]
            , ul [] 
                [ li [] [ text "Complete music package covering the duration of your reception" ]
                , li [] [ text "Live guitar performed throughout dinner" ]
                , li [] [ text "High quality sound system" ]
                , li [] [ text "LED effects lighting" ]
                , li [] [ text "Wireless microphone for speeches" ]
                , li [] [ text "Music selection planning meeting" ]
                , li [] [ text "Facilitation of wedding games" ]
                , li [] [ text "Music requests from your guests on the night" ]
                ]
            , br [] []
            , p 
                [] 
                [ text "Whether it be snippets of songs for each speech or separate songs for each member of your bridal party as they enter, John plans out your entire reception from beginning to end. With John also playing music as requested by your guests you can rest assured that your reception will be a party that reflects the personalities of yourselves along with your family and friends." ]
            ]
        , Grid.col [ Col.sm3 ] 
            [ img 
                [ class "img-fluid rounded-circle" 
                , src "img/Reception.jpg"
                ] 
                [] 
            ]
        ]
    ]


pageMusic : Model -> List (Html Msg)
pageMusic model =
    [ Grid.row []
        [ Grid.col [ Col.sm1 ] []
        , Grid.col [] 
            [ blockquote 
                [ class "blockquote cream-border" ] 
                [ h3 
                    [ class "upper-pad" ] 
                    [ text "Music Gallery" ] 
                ] 
            ]
        ]
    , Grid.row []
        [ Grid.col [ Col.xs1, Col.sm2 ] []
        , Grid.col [ Col.xs11, Col.sm8 ]
            [ p 
                [ class "lg-text lower-pad" ] 
                [ text "With over 20 years experience John performs live Jazz, Latin & Lounge guitar throughout dinner to create a relaxed atmosphere. Click below to hear some of John’s live playing. John is the perfect choice for your wedding reception, providing you and your guests with a unique wedding experience to be remembered." ] 
            , br [] []
            ]
        ]
    , Grid.row []
        [ Grid.col [ Col.xs1, Col.sm2 ] []
        , Grid.col [ Col.xs11, Col.sm8 ]
            [ Carousel.config CarouselMsg [ class "lower-pad" ]
                |> Carousel.withControls
                |> Carousel.withIndicators
                |> Carousel.slides
                    [ Slide.config [] ( Slide.image [] "img/slides/1.jpg" )
                    , Slide.config [] ( Slide.image [] "img/slides/2.jpg" )
                    , Slide.config [] ( Slide.image [] "img/slides/3.jpg" )
                    , Slide.config [] ( Slide.image [] "img/slides/4.jpg" )
                    , Slide.config [] ( Slide.image [] "img/slides/5.jpg" ) 
                    ]
                |> Carousel.view model.carouselState
            ]
        ]
    ]

pageContact : Model -> List (Html Msg)
pageContact model = 
    [ Grid.row []
        [ Grid.col [ Col.sm1 ] []
        , Grid.col [] 
            [ blockquote 
                [ class "blockquote cream-border" ] 
                [ h3 
                    [ class "upper-pad" ] 
                    [ text "Enquiries" ] 
                ] 
            ]
        ]
    , Grid.row []
        [ Grid.col [ Col.xs1, Col.sm2 ] []
        , Grid.col [ Col.xs11, Col.sm8 ]
            [ fieldset
                []
                [ legend 
                    []
                    [ text "Send John an enquiry for an obligation free quote" ]
                ]
            ]
        ] 
    ]


pageNotFound : List (Html Msg)
pageNotFound =
    [ Grid.row []
        [ Grid.col [ Col.sm1 ] []
        , Grid.col [] 
            [ blockquote 
                [ class "blockquote cream-border" ] 
                [ h3 
                    [ class "upper-pad" ] 
                    [ text "Not Found" ] 
                ] 
            ]
        ]
    , Grid.row []
        [ Grid.col [ Col.xs1, Col.sm2 ] []
        , Grid.col [ Col.xs11, Col.sm8 ]
            [ p 
                [ class "lg-text lower-pad" ] 
                [ text "Sorry but this page must have been misplaced!" ] 
            ]
        ]
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


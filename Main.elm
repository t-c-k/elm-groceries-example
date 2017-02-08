import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http exposing (Error)
import Json.Decode exposing (Decoder, list, string, int)
import Json.Decode.Pipeline exposing (decode, required)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL


type alias Item =
    { name : String
    , price : Int
    , imageUrl : String
    }


type alias Model =
    { items : List Item
    , cart : List Item
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] [], getItems )


-- UPDATE


type Msg
    = ReceiveItems (Result Error (List Item))
    | AddToCart Item


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddToCart item ->
            ( { model | cart = model.cart ++ [item] }, Cmd.none )
        ReceiveItems (Ok items) ->
            ( { model | items = items }, Cmd.none )
        ReceiveItems (Err _) ->
            Debug.crash "GET" "Failed to fetch items"


getItems : Cmd Msg
getItems =
    Http.send ReceiveItems (Http.get "./items.json" decodeItems)


-- VIEW


viewCartItem : Item -> Html Msg
viewCartItem item =
    li []
        [ text item.name ]


viewCart : List Item -> List (Html Msg)
viewCart items =
    List.map viewCartItem items


viewItem : Item -> Html Msg
viewItem item =
    div [ class "item", onClick (AddToCart item) ]
        [ img [ src item.imageUrl ] []
        , div [ class "overlay" ]
            [ text "+" ]
        ]


viewItems : List Item -> List (Html Msg)
viewItems items =
    List.map viewItem items


viewCartPrice : List Item -> Html Msg
viewCartPrice items =
    let
        total = items
            |> List.map (\p -> p.price)
            |> List.sum
            |> toString
    in
        text ("Total: £ " ++ total)


view : Model -> Html Msg
view model =
    div [ class "content" ] [ div [ class "left" ]
        [ div [ class "cart-title" ]
            [ text "Cart" ]
        , ul [ class "cart-list" ]
            <| viewCart model.cart
        , div [ class "cart-total" ]
            [ viewCartPrice model.cart ]
        ]
    , div [ class "right" ]
        [ div [ class "items" ]
            <| viewItems model.items
        ]
    ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- DECODERS


decodeItem : Decoder Item
decodeItem =
    decode Item
        |> required "name" string
        |> required "price" int
        |> required "image_url" string


decodeItems : Decoder (List Item)
decodeItems =
    list decodeItem

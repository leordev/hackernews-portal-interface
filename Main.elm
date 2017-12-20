module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, src)
import Http
import Json.Decode exposing (Decoder, list, field, string, map7, map3, map2, float, int, maybe)


-- initialization


init : ( Model, Cmd Msg )
init =
    ( initModel, getStoriesCmd )


initModel : Model
initModel =
    { stories = [], isLoading = True, error = Nothing }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }



-- model


type alias Story =
    { id : Int
    , storyType : String
    , title : String
    , url : Maybe String
    , score : Int
    , time : Int
    , page : StoryPageDetails
    }


type alias StoryPageDetails =
    { image : Maybe String
    , description : Maybe String
    , tags : Maybe (List PageTag)
    }


type alias PageTag =
    { name : String
    , accuracy : Float
    }


type alias Model =
    { stories : List Story
    , isLoading : Bool
    , error : Maybe String
    }



-- data handlers


storiesDecoder : Decoder (List Story)
storiesDecoder =
    list
        (map7 Story
            (field "id" int)
            (field "type" string)
            (field "title" string)
            (field "url" (maybe string))
            (field "score" int)
            (field "time" int)
            (field "page"
                (map3 StoryPageDetails
                    (field "image" (maybe string))
                    (field "description" (maybe string))
                    (field "tags"
                        (maybe
                            (list
                                (map2 PageTag
                                    (field "name" string)
                                    (field "accuracy" float)
                                )
                            )
                        )
                    )
                )
            )
        )



-- update


type Msg
    = Refresh
    | StoriesResponse (Result Http.Error (List Story))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            ( { model | isLoading = True }, getStoriesCmd )

        StoriesResponse (Ok stories) ->
            ( { model | stories = stories, isLoading = False }, Cmd.none )

        StoriesResponse (Err error) ->
            ( { model
                | error =
                    Just (Debug.log "error while getting stories" (toString error))
                , isLoading = False
              }
            , Cmd.none
            )


getStoriesCmd : Cmd Msg
getStoriesCmd =
    let
        req =
            Http.get "http://localhost:4000/top" storiesDecoder
    in
        Http.send StoriesResponse req



-- view


hnLink : Story -> Bool -> String
hnLink story forceHn =
    let
        hnUrl =
            "http://news.ycombinator.com/item?id=" ++ (toString story.id)

        url =
            if forceHn then
                hnUrl
            else
                Maybe.withDefault hnUrl story.url
    in
        url


viewThumb : Story -> Html Msg
viewThumb story =
    let
        srcImg =
            case story.page.image of
                Just imgUrl ->
                    imgUrl

                Nothing ->
                    if story.url == Nothing then
                        "http://wiredcraft.com/images/posts/hacker-news.png"
                    else
                        "https://wearesocial-net.s3.amazonaws.com/wp-content/themes/wearesocial/img/post/contact.png"

        thumbImage =
            img [ src srcImg ] []
    in
        a [ class "thumb", href (hnLink story False) ]
            [ thumbImage
            , p [] [ text story.title ]
            ]


viewHeaderLink : Story -> Html Msg
viewHeaderLink story =
    a [ href (hnLink story False) ] [ text story.title ]


viewMain : List Story -> Html Msg
viewMain stories =
    case stories of
        [ main, subMain1, subMain2, subMain3, thumb1, thumb2, thumb3 ] ->
            div [ class "main" ]
                [ p [ class "main-first" ]
                    [ a [ href (hnLink main False) ] [ text main.title ] ]
                , div [ class "main-subs" ]
                    [ viewHeaderLink subMain1
                    , viewHeaderLink subMain2
                    , viewHeaderLink subMain3
                    ]
                , div [ class "main-subs" ]
                    [ viewThumb thumb1
                    , viewThumb thumb2
                    , viewThumb thumb3
                    ]
                ]

        _ ->
            text ""


viewSub : String -> List Story -> Html Msg
viewSub name stories =
    case stories of
        [ sub1, sub2, sub3 ] ->
            div [ class ("top-sub " ++ name) ]
                [ viewThumb sub1
                , viewHeaderLink sub2
                , viewThumb sub3
                ]

        _ ->
            text ""


viewCol : String -> List Story -> Html Msg
viewCol name stories =
    case stories of
        main :: stories ->
            div [ class name ]
                [ viewThumb main
                , div [ class "subcol" ]
                    (List.map viewThumb stories)
                ]

        _ ->
            text ""


view : Model -> Html Msg
view model =
    let
        mains =
            List.take 13 model.stories

        main =
            List.take 7 mains

        subs =
            List.drop 7 mains

        sub1 =
            List.take 3 subs

        sub2 =
            List.drop 3 subs

        bottoms =
            List.drop 13 model.stories

        colLeft =
            List.take 5 bottoms

        colCenter =
            List.drop 5 bottoms |> List.take 5

        colRight =
            List.drop 10 bottoms |> List.take 5
    in
        div [ class "container" ]
            [ h1 [] [ text "Portal Hacker News" ]
            , div [ class "topper" ]
                [ viewMain main
                , viewSub "left" sub1
                , viewSub "right" sub2
                ]
            , div [ class "bottoms" ]
                [ viewCol "left" colLeft
                , viewCol "center" colCenter
                , viewCol "right" colRight
                ]
            ]

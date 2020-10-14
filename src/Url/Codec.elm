module Url.Codec exposing (UrlCodec, adt, buildCustom)

import Html exposing (Html)
import Html.Attributes
import Url exposing (Url)
import Url.Builder exposing (QueryParameter)
import Url.Codec.Advanced as Advanced exposing (Pretty)


type UrlCodec a b
    = UrlCodec (Advanced.UrlCodec a b a b)


type AdtUrlCodec a b match r
    = AdtUrlCodec (Advanced.AdtUrlCodec a b match r)


adt : a -> AdtUrlCodec (x -> y) y a r
adt match =
    AdtUrlCodec <| Advanced.adt match


top : UrlCodec b b
top =
    UrlCodec <| Advanced.top


string : UrlCodec (String -> r) r
string =
    UrlCodec Advanced.string


int : UrlCodec (Int -> r) r
int =
    UrlCodec Advanced.int


variant :
    a
    -> UrlCodec a b
    -> AdtUrlCodec (b -> e) e (a -> y) b
    -> AdtUrlCodec (b -> e) e y b
variant ctor (UrlCodec piece) (AdtUrlCodec old) =
    AdtUrlCodec
        (Advanced.variant
            ctor
            piece
            old
        )


join : UrlCodec a b -> UrlCodec b c -> UrlCodec a c
join (UrlCodec left) (UrlCodec right) =
    UrlCodec (Advanced.join left right)


s : String -> UrlCodec r r
s x =
    UrlCodec <| Advanced.s x


buildCustom : AdtUrlCodec match r match r -> UrlCodec match r
buildCustom (AdtUrlCodec codec) =
    UrlCodec (Advanced.buildCustom codec)


toUrl : UrlCodec (x -> Pretty) Pretty -> x -> String
toUrl (UrlCodec codec) =
    Advanced.toUrl codec


fromUrl : UrlCodec (b -> b) b -> Url -> Maybe b
fromUrl (UrlCodec codec) =
    Advanced.fromUrl codec



-- TEST --


type Route
    = Blog Int
    | Article String Int
    | Mull String Int Int Int
    | User UserRoute
    | Home


type UserRoute
    = About
    | Settings


route : UrlCodec (Route -> a) a
route =
    adt
        (\fhome fblog farticle fmull fuser value ->
            case value of
                Home ->
                    fhome

                Blog i ->
                    fblog i

                Article se i ->
                    farticle se i

                Mull se i1 i2 i3 ->
                    fmull se i1 i2 i3

                User user_route ->
                    fuser user_route
        )
        |> variant Home top
        |> variant Blog
            (join (s "blog") int)
        |> variant Article
            (join string int)
        |> variant Mull
            (join string <| join int <| join int int)
        |> variant User
            (join (s "user") userRoute)
        |> buildCustom


userRoute : UrlCodec (UserRoute -> a) a
userRoute =
    adt
        (\fabout fsettings value ->
            case value of
                About ->
                    fabout

                Settings ->
                    fsettings
        )
        |> variant About (s "about")
        |> variant Settings (s "settings")
        |> buildCustom


main : Html msg
main =
    Html.table [] <|
        List.map
            test
            [ Blog 0
            , Blog 1
            , Blog -3
            , Article "Foo" 3
            , Article "" -1
            , Home
            , User About
            , User Settings
            , Mull "ghoul" 1 2 3
            ]


test : Route -> Html msg
test r =
    let
        a =
            toUrl route r

        b =
            "http://localhost" ++ a

        c =
            Url.fromString b

        d =
            Maybe.andThen (Advanced.fromUrl route) c
    in
    [ Debug.toString r
    , a
    , b
    , Debug.toString d
    ]
        |> List.intersperse "=>"
        |> List.map
            (\cell ->
                Html.td
                    [ Html.Attributes.style "padding" "10px"
                    ]
                    [ Html.text cell ]
            )
        |> Html.tr
            [ Html.Attributes.style "background-color" <|
                if Just r == d then
                    "lightgreen"

                else
                    "red"
            ]

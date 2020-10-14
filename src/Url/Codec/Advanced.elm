module Url.Codec.Advanced exposing
    ( AdtUrlCodec
    , Pretty
    , UrlCodec
    , adt
    , buildCustom
    , custom
    , fromUrl
    , int
    , intQuery
    , join
    , map
    , s
    , string
    , stringQuery
    , toUrl
    , top
    , variant
    )

import Html exposing (Html)
import Html.Attributes
import Url exposing (Url)
import Url.Builder exposing (QueryParameter)
import Url.Parser exposing ((</>), Parser)
import Url.Parser.Query


type Pretty
    = Pretty
        { segments : List String
        , query : List QueryParameter
        }


type UrlCodec a b c r
    = UrlCodec
        { parser : Parser a b
        , prettyPrinter : (Pretty -> r) -> c
        }



-- Base


int : UrlCodec (Int -> a) a (Int -> r) r
int =
    UrlCodec
        { parser = Url.Parser.int
        , prettyPrinter = \k i -> k <| fromSegment (String.fromInt i)
        }


intQuery : String -> UrlCodec (Maybe Int -> a) a (Int -> r) r
intQuery key =
    UrlCodec
        { parser = Url.Parser.Query.int key |> Url.Parser.query
        , prettyPrinter = \k value -> k <| fromQuery <| Url.Builder.int key value
        }


string : UrlCodec (String -> a) a (String -> r) r
string =
    UrlCodec
        { parser = Url.Parser.string
        , prettyPrinter = \k x -> k <| fromSegment x
        }


stringQuery : String -> UrlCodec (Maybe String -> a) a (String -> r) r
stringQuery key =
    UrlCodec
        { parser = Url.Parser.Query.string key |> Url.Parser.query
        , prettyPrinter = \k value -> k <| fromQuery <| Url.Builder.string key value
        }


s : String -> UrlCodec a a r r
s fixed =
    UrlCodec
        { parser = Url.Parser.s fixed
        , prettyPrinter = \k -> k <| fromSegment fixed
        }


join : UrlCodec a b pa pb -> UrlCodec b c pb pc -> UrlCodec a c pa pc
join (UrlCodec left) (UrlCodec right) =
    UrlCodec
        { parser = left.parser </> right.parser
        , prettyPrinter =
            \k ->
                left.prettyPrinter
                    (\(Pretty ls) ->
                        right.prettyPrinter
                            (\(Pretty rs) ->
                                k <|
                                    Pretty
                                        { segments = ls.segments ++ rs.segments
                                        , query = ls.query ++ rs.query
                                        }
                            )
                    )
        }


top : UrlCodec a a b b
top =
    UrlCodec
        { parser = Url.Parser.top
        , prettyPrinter = \k -> k <| Pretty { segments = [], query = [] }
        }


map : a -> (c -> b) -> UrlCodec a d c r -> UrlCodec (d -> e) e b r
map f g (UrlCodec { parser, prettyPrinter }) =
    UrlCodec
        { parser = Url.Parser.map f parser
        , prettyPrinter = prettyPrinter >> g
        }



-- Custom


custom :
    String
    -> (String -> Maybe a)
    -> (x -> String)
    -> UrlCodec (a -> b) b (x -> r) r
custom name parser printer =
    UrlCodec
        { parser = Url.Parser.custom name parser
        , prettyPrinter = \k x -> k <| fromSegment (printer x)
        }


type AdtUrlCodec a b match r
    = AdtUrlCodec
        { parser : List (Parser a b)
        , prettyPrinter : (Pretty -> r) -> match
        }


adt : match -> AdtUrlCodec (x -> y) y match r
adt match =
    AdtUrlCodec
        { parser = []
        , prettyPrinter = \_ -> match
        }


variant :
    a
    -> UrlCodec a b c d
    -> AdtUrlCodec (b -> e) e (c -> y) d
    -> AdtUrlCodec (b -> e) e y d
variant ctor (UrlCodec piece) (AdtUrlCodec old) =
    AdtUrlCodec
        { parser = Url.Parser.map ctor piece.parser :: old.parser
        , prettyPrinter = \k -> old.prettyPrinter k (piece.prettyPrinter k)
        }


buildCustom : AdtUrlCodec a b c d -> UrlCodec a b c d
buildCustom (AdtUrlCodec { parser, prettyPrinter }) =
    UrlCodec
        { parser = Url.Parser.oneOf <| List.reverse parser
        , prettyPrinter = prettyPrinter
        }



-- Running Codecs


fromUrl : UrlCodec (a -> a) a x y -> Url -> Maybe a
fromUrl (UrlCodec { parser }) =
    Url.Parser.parse parser


toUrl : UrlCodec a b (x -> Pretty) Pretty -> x -> String
toUrl (UrlCodec { prettyPrinter }) x =
    let
        (Pretty { segments, query }) =
            prettyPrinter identity x
    in
    Url.Builder.absolute segments query



-- Utils


fromSegment : String -> Pretty
fromSegment x =
    Pretty
        { segments = [ x ]
        , query = []
        }


fromQuery : QueryParameter -> Pretty
fromQuery p =
    Pretty
        { segments = []
        , query = [ p ]
        }



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


mullPathCodec : UrlCodec (String -> Int -> Int -> Int -> a) a (String -> Int -> Int -> Int -> r) r
mullPathCodec =
    join string <| join int <| join int int


route : UrlCodec (Route -> a) a (Route -> b) b
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
        |> variant Home
            top
        |> variant Blog
            (join (s "blog") int)
        |> variant Article
            (join string int)
        |> variant Mull
            mullPathCodec
        |> variant User
            (join (s "user") userRoute)
        |> buildCustom


userRoute : UrlCodec (UserRoute -> b) b (UserRoute -> d) d
userRoute =
    adt
        (\fabout fsettings value ->
            case value of
                About ->
                    fabout

                Settings ->
                    fsettings
        )
        |> variant About
            (s "about")
        |> variant Settings
            (s "settings")
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
            Maybe.andThen (fromUrl route) c
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

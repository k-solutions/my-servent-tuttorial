module Generated.BooksApi exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

type alias User  =
   { usrName: String
   , usrAge: Int
   }

jsonDecUser : Json.Decode.Decoder ( User )
jsonDecUser =
   Json.Decode.succeed (\pusrName pusrAge -> {usrName = pusrName, usrAge = pusrAge})
   |> required "usrName" (Json.Decode.string)
   |> required "usrAge" (Json.Decode.int)

jsonEncUser : User -> Value
jsonEncUser  val =
   Json.Encode.object
   [ ("usrName", Json.Encode.string val.usrName)
   , ("usrAge", Json.Encode.int val.usrAge)
   ]



type Product  = Product
   { prdName: String
   }

jsonDecProduct : Json.Decode.Decoder ( Product )
jsonDecProduct =
   Json.Decode.succeed (\pprdName -> (Product {prdName = pprdName}))
   |> required "prdName" (Json.Decode.string)

jsonEncProduct : Product -> Value
jsonEncProduct  (Product val) =
   Json.Encode.object
   [ ("prdName", Json.Encode.string val.prdName)
   ]


getXByX : Int -> (Maybe Int) -> (Result Http.Error  (Int)  -> msg) -> Cmd msg
getXByX capture_x query_y toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [ [ query_y
                    |> Maybe.map (String.fromInt
                                  >> Url.Builder.string "y") ]
                ])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8000"
                    [ "x"
                    , (capture_x |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg Json.Decode.int
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postXByX : Int -> (Result Http.Error  (Int)  -> msg) -> Cmd msg
postXByX capture_x toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8000"
                    [ "x"
                    , (capture_x |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg Json.Decode.int
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getUsers : (Result Http.Error  ((List User))  -> msg) -> Cmd msg
getUsers toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8000"
                    [ "users"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecUser))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getUsersById : Int -> (Result Http.Error  (User)  -> msg) -> Cmd msg
getUsersById capture_id toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8000"
                    [ "users"
                    , (capture_id |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecUser
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postUsers : User -> (Result Http.Error  (NoContent)  -> msg) -> Cmd msg
postUsers body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8000"
                    [ "users"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncUser body)
            , expect =
                Http.expectJson toMsg jsonDecNoContent
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getProducts : (Result Http.Error  ((List Product))  -> msg) -> Cmd msg
getProducts toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8000"
                    [ "products"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecProduct))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getProductsById : Int -> (Result Http.Error  (Product)  -> msg) -> Cmd msg
getProductsById capture_id toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8000"
                    [ "products"
                    , (capture_id |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecProduct
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postProducts : Product -> (Result Http.Error  (NoContent)  -> msg) -> Cmd msg
postProducts body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8000"
                    [ "products"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncProduct body)
            , expect =
                Http.expectJson toMsg jsonDecNoContent
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

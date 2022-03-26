module Generated.CoreApi exposing(..)

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


getFooByI : Int -> (Result Http.Error  (())  -> msg) -> Cmd msg
getFooByI capture_i toMsg =
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
                Url.Builder.crossOrigin "http://localhost:3001"
                    [ "foo"
                    , (capture_i |> String.fromInt)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getBar : (Result Http.Error  (())  -> msg) -> Cmd msg
getBar toMsg =
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
                Url.Builder.crossOrigin "http://localhost:3001"
                    [ "bar"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
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
                Url.Builder.crossOrigin "http://localhost:3001"
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
                Url.Builder.crossOrigin "http://localhost:3001"
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
                Url.Builder.crossOrigin "http://localhost:3001"
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

post : Msg -> (Result Http.Error  (NoContent)  -> msg) -> Cmd msg
post body toMsg =
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
                Url.Builder.crossOrigin "http://localhost:3001"
                    []
                    params
            , body =
                Http.jsonBody (jsonEncMsg body)
            , expect =
                Http.expectJson toMsg jsonDecNoContent
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

get : (Result Http.Error  ((List Msg))  -> msg) -> Cmd msg
get toMsg =
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
                Url.Builder.crossOrigin "http://localhost:3001"
                    []
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecMsg))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

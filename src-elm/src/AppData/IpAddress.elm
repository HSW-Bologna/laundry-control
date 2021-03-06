module AppData.IpAddress exposing (IpAddress, asList, changePart, fromInts, localhost, toString, fromString)

import Json.Decode as Decode


type alias IpAddress =
    { ip1 : Int
    , ip2 : Int
    , ip3 : Int
    , ip4 : Int
    }




localhost : IpAddress
localhost =
    IpAddress 127 0 0 1


fromInts : Int -> Int -> Int -> Int -> Maybe IpAddress
fromInts ip1 ip2 ip3 ip4 =
    let
        validateInt int =
            int < 0 || int > 255
    in
    if List.all validateInt [ ip1, ip2, ip3, ip4 ] then
        Just <| IpAddress ip1 ip2 ip3 ip4

    else
        Nothing


toString : IpAddress -> String
toString { ip1, ip2, ip3, ip4 } =
    String.fromInt ip1
        ++ "."
        ++ String.fromInt ip2
        ++ "."
        ++ String.fromInt ip3
        ++ "."
        ++ String.fromInt ip4


fromString : String -> Maybe IpAddress
fromString ip =
    case String.split "." ip |> List.map String.toInt of
        ((Just ip1) :: (Just ip2) :: (Just ip3) :: (Just ip4) :: _) as list ->
            if
                List.all
                    (\el ->
                        case el of
                            Just x ->
                                x >= 0 && x < 256

                            Nothing ->
                                False
                    )
                    list
            then
                Just <| IpAddress ip1 ip2 ip3 ip4

            else
                Nothing

        _ ->
            Nothing


changePart : IpAddress -> Int -> Int -> IpAddress
changePart ip i val =
    if val >= 0 && val <= 255 then
        case i of
            0 ->
                { ip | ip1 = val }

            1 ->
                { ip | ip2 = val }

            2 ->
                { ip | ip3 = val }

            3 ->
                { ip | ip4 = val }

            _ ->
                ip

    else
        ip


asList : IpAddress -> List Int
asList { ip1, ip2, ip3, ip4 } =
    [ ip1, ip2, ip3, ip4 ]

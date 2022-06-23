module Pages.Machine.Tabs.LocalDevicesList exposing (Model, Msg(..), SharedModel, buildModel, update, view)

import AUTOGEN_FILE_translations as Intl
import AppData.IpAddress exposing (IpAddress, asList, changePart, localhost, toString)
import AppWidgets.AppWidgets as AppWidgets
import AppWidgets.Style as Style
import Context exposing (Context, translate)
import Element as Ui
import Element.Font as Font
import Element.Input as Input
import Ports
import Widget
import Widget.Material as Material


type alias SharedModel a =
    { a
        | context : Context
        , localMachines : Maybe (List ( IpAddress, String ))
    }


type alias Model =
    { ipAddress : IpAddress
    }


buildModel : Model
buildModel =
    Model localhost


type Msg
    = RefreshButton
    | ConnectButton IpAddress
    | IpInput IpAddress


update : Msg -> SharedModel a -> Model -> ( SharedModel a, Model, Cmd msg )
update msg sharedModel model =
    case msg of
        RefreshButton ->
            ( { sharedModel | localMachines = Nothing }, model, Ports.searchMachines )

        ConnectButton ip ->
            ( sharedModel, model, Ports.washingMachineHttpConnect <| toString ip )

        IpInput ip ->
            ( sharedModel, { model | ipAddress = ip }, Cmd.none )



-- VIEW


view : SharedModel a -> Model -> Ui.Element Msg
view { context, localMachines } { ipAddress } =
    let
        refreshButton =
            AppWidgets.textButton (translate Intl.Aggiorna context) (Just RefreshButton)
    in
    Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16, Ui.spacing 32 ]
        [ Ui.paragraph [ Font.size 32, Ui.centerX, Ui.width Ui.fill ] [ Ui.text (translate Intl.DispositiviLocali context) ]
        , Ui.column [ Ui.centerX, Ui.spacing 16 ]
            [ Ui.row [ Ui.centerX, Ui.spacing 2 ] <|
                (List.intersperse (Ui.text ".") <|
                    List.indexedMap
                        (\i val ->
                            Input.text [ Ui.width <| Ui.px 120 ]
                                { onChange = \s -> IpInput <| changePart ipAddress i (Maybe.withDefault 0 <| String.toInt s)
                                , text = String.fromInt val
                                , placeholder = Nothing
                                , label = Input.labelHidden "ip"
                                }
                        )
                        (asList ipAddress)
                )
            , AppWidgets.textButton (translate Intl.Connetti context) (Just <| ConnectButton ipAddress) |> Ui.el [ Ui.centerX ]
            ]
        , case localMachines of
            Nothing ->
                Ui.column [ Ui.centerX, Ui.centerY, Ui.spacing 64 ]
                    [ Widget.circularProgressIndicator (Material.progressIndicator Style.palette) Nothing |> Ui.el [ Ui.centerX ]
                    , refreshButton |> Ui.el [ Ui.centerX ]
                    ]

            Just [] ->
                Ui.el [ Ui.centerX, Ui.centerY ] refreshButton

            Just machines ->
                Ui.el
                    [ Ui.width Ui.fill
                    , Ui.height Ui.fill
                    , Ui.inFront <| Ui.el [ Ui.alignBottom, Ui.alignRight ] refreshButton
                    ]
                <|
                    Ui.wrappedRow [ Ui.centerX, Ui.spacing 32, Ui.padding 64 ] <|
                        List.map (\( d, _ ) -> AppWidgets.machineSelectionButton (ConnectButton d) (toString d)) machines
        ]
        |> AppWidgets.scrollbarYEl [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16 ]

module AppData.WashingMachineConfiguration exposing (..)

import AUTOGEN_FILE_translations as Intl exposing (IntlString, Language, Translation, setTranslation)
import AppData.Parameter as Parameter exposing (Parameter)
import Array as Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)
import Context exposing (Context, translate)
import Element.Region exposing (description)
import Flate
import Inflate
import Json.Decode exposing (int)
import Tar exposing (defaultMetadata)



-- Parameter order should be reversed because of how Decode pipelining works


type alias MachineParameters =
    { globalPriceValue : Int

    --TODO: wrong order ^
    , priceVisualization : Int
    , priceDecimalDigits : Int
    , priceDigits : Int
    , paymentRequest : Int
    , pulseValue : Int
    , globalPrice : Int
    , coinAcceptor : Int
    , ioExpander : Int
    , machineBusy : Int
    , machineBusySignal : Int
    , drainRecycle : Int
    , detergentReducedLoad : Int
    , levelReducedLoad : Int
    , auxOut4Type : Int
    , auxOut3Type : Int
    , auxOut2Type : Int
    , auxOut1Type : Int
    , portHoleSwitchDelayTime : Int
    , drainAlarmTime : Int
    , temperatureAlarmTime : Int
    , levelAlarmTime : Int
    , uselessPar6 : Int
    , stopPressTime : Int
    , pausePressTime : Int
    , uselessPar5 : Int
    , uselessPar4 : Int
    , uselessPar3 : Int
    , uselessPar2 : Int
    , uselessPar1 : Int
    , showMachinePrice : Int
    , showKg : Int
    , scheduledProgram : Int
    , showTotalCycles : Int
    , soapMenuUi : Int
    , menuUi : Int
    , startUi : Int
    , stopUi : Int
    , accessLevel : Int
    , uselessPar0 : Int
    , maxUserPrograms : Int
    , maxPrograms : Int
    , revCounterCorrection : Int
    , trapHeight : Int
    , basketDepth : Int
    , basketDiameter : Int
    , nodeCode : Int
    , machineSubModel : Int
    , machineModel : Int
    , logo : Int
    , maxUiLanguages : Int
    , language : Int
    , name : String
    }


type alias MachineParameter =
    Parameter MachineParameters MachineParameters


type alias MachineConfiguration =
    { parmac : MachineParameters
    , parmacMetadata : List MachineParameter
    , cycles : Array WashingCycle
    }


parameterMetadataList : List MachineParameter
parameterMetadataList =
    let
        uiOptions =
            [ Intl.Self, Intl.Laboratorio ]

        boolOptions =
            [ Intl.Disabilitato, Intl.Abilitato ]

        formatOption : List IntlString -> MachineParameters -> Context -> Int -> String
        formatOption options _ context value =
            let
                option =
                    Maybe.withDefault Intl.Errore <| Array.get value <| Array.fromList options
            in
            translate option context

        formatNumber : MachineParameters -> Context -> Int -> String
        formatNumber _ _ value =
            String.fromInt value

        formatWithUM : String -> MachineParameters -> Context -> Int -> String
        formatWithUM um _ _ value =
            String.fromInt value ++ " " ++ um
    in
    [ { get = .language, set = \v p -> { p | language = v }, min = 0, max = 1, default = 0, description = Intl.Lingua, format = formatOption [ Intl.ItalianoCSV, Intl.Inglese ], ui = Parameter.Option }
    , { get = .maxUiLanguages, set = \v p -> { p | maxUiLanguages = v }, min = 0, max = 1, default = 1, description = Intl.LingueSelezionabiliDaFrontale, format = formatNumber, ui = Parameter.Number }
    , { get = .logo, set = \v p -> { p | logo = v }, min = 0, max = 5, default = 1, description = Intl.Logo, format = formatOption [ Intl.Nessuno, Intl.Ms, Intl.Lavenda, Intl.Rotondi, Intl.Schultess, Intl.Hoover ], ui = Parameter.Option }
    , { get = .nodeCode, set = \v p -> { p | nodeCode = v }, min = 0, max = 255, default = 0, description = Intl.CodiceNodoMacchina, format = formatNumber, ui = Parameter.Number }
    , { get = .machineModel, set = \v p -> { p | machineModel = v }, min = 0, max = 255, default = 255, description = Intl.ModelloMacchina, format = formatNumber, ui = Parameter.Number }
    , { get = .machineSubModel, set = \v p -> { p | machineSubModel = v }, min = 0, max = 255, default = 255, description = Intl.SottomodelloMacchina, format = formatNumber, ui = Parameter.Number }
    , { get = .accessLevel, set = \v p -> { p | accessLevel = v }, min = 0, max = 3, default = 0, description = Intl.LivelloDiAccesso, format = formatNumber, ui = Parameter.Number }
    , { get = .stopUi, set = \v p -> { p | stopUi = v }, min = 0, max = 1, default = 0, description = Intl.InterfacciaDaFermo, format = formatOption uiOptions, ui = Parameter.Option }
    , { get = .startUi, set = \v p -> { p | startUi = v }, min = 0, max = 1, default = 0, description = Intl.InterfacciaInFunzionamento, format = formatOption uiOptions, ui = Parameter.Option }
    , { get = .maxPrograms, set = \v p -> { p | maxPrograms = v }, min = 0, max = 100, default = 100, description = Intl.NumeroMassimoDiProgrammi, format = formatNumber, ui = Parameter.Number }
    , { get = .maxUserPrograms, set = \v p -> { p | maxUserPrograms = v }, min = 0, max = 100, default = 20, description = Intl.NumeroMassimoDiProgrammiUtente, format = formatNumber, ui = Parameter.Number }
    , { get = .showKg, set = \v p -> { p | showKg = v }, min = 0, max = 100, default = 0, description = Intl.VisualizzazioneKg, format = formatWithUM "kg", ui = Parameter.Number }

    -- TODO: il prezzo macchina deve essere visualizzato in base alle cifre configurate
    , { get = .showMachinePrice, set = \v p -> { p | showMachinePrice = v }, min = 0, max = 0xFFFF, default = 0, description = Intl.PrezzoMacchina, format = formatNumber, ui = Parameter.Number }
    , { get = .menuUi, set = \v p -> { p | menuUi = v }, min = 0, max = 1, default = 0, description = Intl.MenuParametri, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .soapMenuUi, set = \v p -> { p | soapMenuUi = v }, min = 0, max = 1, default = 0, description = Intl.MenuSaponi, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .showTotalCycles, set = \v p -> { p | showTotalCycles = v }, min = 0, max = 1, default = 0, description = Intl.MostraTotaleCicli, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .scheduledProgram, set = \v p -> { p | scheduledProgram = v }, min = 0, max = 1, default = 0, description = Intl.LavaggioProgrammato, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .coinAcceptor, set = \v p -> { p | coinAcceptor = v }, min = 0, max = 8, default = 0, description = Intl.AbilitazionePagamento, format = formatOption [ Intl.Nessuno, Intl.GettonieraNA, Intl.GettonieraNC, Intl.GettoniereNA, Intl.GettoniereNC, Intl.GettonieraDigitale, Intl.GettonieraLineaSingola, Intl.CassaNA, Intl.CassaNC ], ui = Parameter.Option }
    , { get = .pulseValue, set = \v p -> { p | pulseValue = v }, min = 1, max = 0xFFFF, default = 10, description = Intl.ValoreImpulso, format = formatNumber, ui = Parameter.Number }
    , { get = .globalPriceValue, set = \v p -> { p | globalPriceValue = v }, min = 1, max = 0xFFFF, default = 500, description = Intl.ValorePrezzoUnico, format = formatNumber, ui = Parameter.Number }
    , { get = .globalPrice, set = \v p -> { p | globalPrice = v }, min = 0, max = 1, default = 0, description = Intl.PrezzoUnico, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .priceDigits, set = \v p -> { p | priceDigits = v }, min = 1, max = 6, default = 4, description = Intl.CifrePrezzo, format = formatNumber, ui = Parameter.Number }
    , { get = .priceDecimalDigits, set = \v p -> { p | priceDecimalDigits = v }, min = 0, max = 6, default = 2, description = Intl.CifreDecimaliPrezzo, format = formatNumber, ui = Parameter.Number }
    ]


resetToDefaults : MachineConfiguration -> MachineConfiguration
resetToDefaults config =
    let
        foldfn : MachineParameter -> MachineConfiguration -> MachineConfiguration
        foldfn md c =
            { c | parmac = md.set md.default c.parmac }
    in
    List.foldl foldfn config config.parmacMetadata


changeName : String -> MachineConfiguration -> MachineConfiguration
changeName name ({ parmac } as config) =
    { config | parmac = { parmac | name = name } }


default : Context -> MachineConfiguration
default context =
    MachineConfiguration
        (MachineParameters 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 (translate Intl.NuovaConfigurazione context))
        parameterMetadataList
        (Array.fromList [])
        |> resetToDefaults


machineParametersDecoder : Decoder MachineParameters
machineParametersDecoder =
    Decode.succeed MachineParameters
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.unsignedInt16 BE)
        |> pipeline (Decode.string 33)


encodeMachineParameters : MachineParameters -> Bytes
encodeMachineParameters pars =
    let
        finalSize =
            279

        encodePar lens =
            Encode.unsignedInt16 BE (lens pars)
    in
    Encode.sequence
        ([ limitedStringEncoder pars.name
         , encodePar .language
         , encodePar .maxUiLanguages
         , encodePar .logo
         , encodePar .machineModel
         , encodePar .machineSubModel
         , encodePar .nodeCode
         , encodePar .basketDiameter
         , encodePar .basketDepth
         , encodePar .trapHeight
         , encodePar .revCounterCorrection
         , encodePar .maxPrograms
         , encodePar .maxUserPrograms
         , encodePar .uselessPar0
         , encodePar .accessLevel
         , encodePar .stopUi
         , encodePar .startUi
         , encodePar .menuUi
         , encodePar .soapMenuUi
         , encodePar .showTotalCycles
         , encodePar .scheduledProgram
         , encodePar .showKg
         , encodePar .showMachinePrice
         , encodePar .uselessPar1
         , encodePar .uselessPar2
         , encodePar .uselessPar3
         , encodePar .uselessPar4
         , encodePar .uselessPar5
         , encodePar .pausePressTime
         , encodePar .stopPressTime
         ]
            ++ List.map Encode.unsignedInt8 (List.repeat (finalSize - 37) 0)
        )
        |> Encode.encode



-- Wash Cycles


type alias WashingCycle =
    { washType : Int
    , price : Int
    , name : Translation
    }


newWashingCycle : Int -> WashingCycle
newWashingCycle index =
    WashingCycle 0 0 { italiano = "Nuovo programma " ++ String.fromInt index, english = "New program " ++ String.fromInt index }


changeWashCycleName : String -> Language -> WashingCycle -> WashingCycle
changeWashCycleName name lang cycle =
    { cycle | name = setTranslation cycle.name lang name }


washTypesStrings : Context -> List String
washTypesStrings context =
    [ Intl.MoltoSporchiConPrelavaggio
    , Intl.MedioSporchiConPrelavaggio
    , Intl.MoltoSporchi
    , Intl.MedioSporchi
    , Intl.Colorati
    , Intl.Sintetici
    , Intl.Piumoni
    , Intl.DelicatiAFreddo
    , Intl.Lana
    , Intl.LinoETendaggi
    , Intl.SoloCentrifuga1000Giri
    , Intl.SoloCentrifuga600Giri
    , Intl.Sanificazione
    , Intl.Ammollo
    , Intl.PrelavaggioConCentrifuga
    , Intl.RisciacquoConCentrifuga
    ]
        |> List.map (\s -> translate s context)


encodeIndexFile : Array WashingCycle -> Bytes
encodeIndexFile cycles =
    Array.indexedMap (\i _ -> stringEncoder <| String.fromInt i ++ ".bin\n") cycles
        |> Array.toList
        |> Encode.sequence
        |> Encode.encode


translationDecoder : Decoder Translation
translationDecoder =
    let
        toThrowAway =
            33 * 8
    in
    Decode.map3
        (\i e _ -> Translation i e)
        limitedStringDecoder
        limitedStringDecoder
        (Decode.bytes toThrowAway)


washCycleDecoder : Decoder WashingCycle
washCycleDecoder =
    pipeline translationDecoder <|
        pipeline (Decode.unsignedInt32 Bytes.BE) <|
            pipeline (Decode.unsignedInt16 Bytes.BE) <|
                Decode.succeed WashingCycle


encodeWashCycle : WashingCycle -> Bytes
encodeWashCycle cycle =
    let
        --finalSize = 338
        names =
            [ cycle.name.italiano, cycle.name.english ]
                ++ List.repeat 10 ""
                |> List.take 10
    in
    Encode.sequence
        (List.map limitedStringEncoder names
            ++ [ Encode.unsignedInt32 Bytes.BE cycle.price
               , Encode.unsignedInt16 Bytes.BE cycle.washType
               , Encode.unsignedInt16 Bytes.BE 0 -- TODO: num steps
               ]
        )
        |> Encode.encode



-- Archiving functions


extractArchive : Bytes -> Maybe MachineConfiguration
extractArchive archiveBytes =
    let
        binaryFileContents : String -> List ( Tar.Metadata, Tar.Data ) -> Maybe Bytes
        binaryFileContents name =
            List.filter (\( metadata, _ ) -> metadata.filename == name)
                >> Array.fromList
                >> Array.get 0
                >> Maybe.andThen
                    (\( _, fileData ) ->
                        case fileData of
                            Tar.BinaryData bytes ->
                                Just bytes

                            Tar.StringData _ ->
                                Nothing
                    )

        textFileContents : String -> List ( Tar.Metadata, Tar.Data ) -> Maybe String
        textFileContents name =
            List.filter (\( metadata, _ ) -> metadata.filename == name)
                >> Array.fromList
                >> Array.get 0
                >> Maybe.andThen
                    (\( _, fileData ) ->
                        case fileData of
                            Tar.BinaryData _ ->
                                Nothing

                            Tar.StringData string ->
                                Just string
                    )

        getMachineParameters : List ( Tar.Metadata, Tar.Data ) -> Maybe MachineParameters
        getMachineParameters =
            binaryFileContents parmacFileName >> Maybe.andThen (Decode.decode machineParametersDecoder)

        getIndex : List ( Tar.Metadata, Tar.Data ) -> Maybe (List String)
        getIndex =
            textFileContents indexFileName >> Maybe.map (String.split "\n")

        getWashCycle : String -> List ( Tar.Metadata, Tar.Data ) -> Maybe WashingCycle
        getWashCycle name =
            binaryFileContents (cycleFileName name) >> Maybe.andThen (Decode.decode washCycleDecoder)

        fileList =
            Inflate.inflateGZip archiveBytes
                |> Maybe.map Tar.extractArchive
                |> Maybe.withDefault []

        parmac =
            getMachineParameters fileList

        cycles =
            Maybe.withDefault []
                (getIndex fileList |> Maybe.map (List.map (\n -> getWashCycle n fileList)))
                |> List.filterMap identity
                |> Array.fromList
    in
    Maybe.map (\c -> MachineConfiguration c parameterMetadataList cycles) parmac


createArchive : MachineConfiguration -> Bytes
createArchive { parmac, cycles } =
    let
        binaryMetadata bytes name =
            ( { defaultMetadata
                | filename = name
                , mode = Tar.defaultMode
                , ownerID = -16
                , groupID = -16
                , fileSize = Bytes.width bytes
                , lastModificationTime = 0
                , linkedFileName = ""
                , userName = ""
                , groupName = ""
                , fileNamePrefix = ""
              }
            , Tar.BinaryData bytes
            )
    in
    ([ binaryMetadata (encodeMachineParameters parmac) parmacFileName
     , binaryMetadata dataVersion versionFileName
     , binaryMetadata (encodeIndexFile cycles) indexFileName
     ]
        ++ List.indexedMap (\i c -> binaryMetadata (encodeWashCycle c) (cycleFileNameFromIndex i)) (Array.toList cycles)
    )
        |> Tar.createArchive
        |> Flate.deflateGZip


pipeline : Decoder a -> Decoder (a -> b) -> Decoder b
pipeline =
    Decode.map2 (|>)


parmacFileName : String
parmacFileName =
    "parametri/parmac.bin"


indexFileName : String
indexFileName =
    "programmi/index.txt"


cycleFileName : String -> String
cycleFileName name =
    "programmi/" ++ name


cycleFileNameFromIndex : Int -> String
cycleFileNameFromIndex index =
    cycleFileName <| (String.fromInt index ++ ".bin")


versionFileName : String
versionFileName =
    "version.txt"


dataVersion : Bytes.Bytes
dataVersion =
    encodeString "3\n"


stringEncoder : String -> Encode.Encoder
stringEncoder string =
    Encode.sequence (List.map (\c -> Encode.unsignedInt8 <| Char.toCode c) (String.toList string))


encodeString : String -> Bytes.Bytes
encodeString =
    stringEncoder >> Encode.encode


limitedStringDecoder : Decode.Decoder String
limitedStringDecoder =
    Decode.string 33


limitedStringEncoder : String -> Encode.Encoder
limitedStringEncoder string =
    Encode.string <| String.left 33 <| String.padRight 33 (Char.fromCode 0) string

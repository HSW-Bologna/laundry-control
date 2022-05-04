module AppData.WashingMachineConfiguration exposing (..)

import AUTOGEN_FILE_translations as Intl exposing (IntlString(..), Language, Translation, setTranslation)
import AppData.Parameter as Parameter exposing (Parameter)
import Array as Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Decode.Extra as Decode
import Bytes.Encode as Encode exposing (Encoder)
import Bytes.Extra as Bytes
import Context exposing (Context, translate)
import Element.Region exposing (description)
import Flate
import Inflate
import Json.Decode exposing (int)
import Tar exposing (defaultMetadata)



-- Parameter order should be reversed because of how Decode pipelining works


type alias MachineParameters =
    { name : String
    , language : Int
    , maxUiLanguages : Int
    , logo : Int
    , machineModel : Int
    , machineSubModel : Int
    , nodeCode : Int
    , basketDiameter : Int
    , basketDepth : Int
    , trapHeight : Int
    , revCounterCorrection : Int
    , maxPrograms : Int
    , maxUserPrograms : Int
    , uselessPar0 : Int
    , accessLevel : Int
    , stopUi : Int
    , startUi : Int
    , menuUi : Int
    , soapMenuUi : Int
    , showTotalCycles : Int
    , scheduledProgram : Int
    , showKg : Int
    , showMachinePrice : Int
    , uselessPar1 : Int
    , uselessPar2 : Int
    , uselessPar3 : Int
    , uselessPar4 : Int
    , uselessPar5 : Int
    , pausePressTime : Int
    , stopPressTime : Int
    , uselessPar6 : Int
    , levelAlarmTime : Int
    , temperatureAlarmTime : Int
    , drainAlarmTime : Int
    , portHoleSwitchDelay : Int
    , auxOut1Type : Int
    , auxOut2Type : Int
    , auxOut3Type : Int
    , auxOut4Type : Int
    , levelReducedLoad : Int
    , detergentReducedLoad : Int
    , drainRecycle : Int
    , machineBusySignal : Int
    , machineBusy : Int
    , ioExpander : Int
    , coinAcceptor : Int
    , globalPrice : Int
    , pulseValue : Int
    , paymentRequest : Int
    , priceDigits : Int
    , priceDecimalDigits : Int
    , priceVisualization : Int
    , maxTemperature : Int
    , temperatureHysteresis : Int
    , safetyTemperature : Int
    , termodegratationTemperature : Int
    , levelType : Int
    , pulsesPerLiter : Int
    , levelHysteresisTime : Int
    , cmMaxLevel : Int
    , cmMinDrain : Int
    , cmMinHeating : Int
    , maxLiters : Int
    , surfaceLevel : Int
    , ltMinHeating : Int
    , drainMinTime : Int
    , serviceDrainTime : Int
    , drainPulseTime : Int
    , inverterType : Int
    , minimumWashSpeed : Int
    , maximumWashSpeed : Int
    , serviceSpeed : Int
    , preparationRotationEnable : Int
    , preparationRotationTime : Int
    , preparationPauseTime : Int
    , preparationMinimumSpeed : Int
    , preparationMaximumSpeed : Int
    , centrifuge1MinimumSpeed : Int
    , centrifuge1MaximumSpeed : Int
    , centrifuge2MinimumSpeed : Int
    , centrifuge2MaximumSpeed : Int
    , centrifuge3MinimumSpeed : Int
    , centrifuge3MaximumSpeed : Int
    , rampMinimumTime : Int
    , rampMaximumTime : Int
    , maxLurchAttempts : Int
    , proximity : Int
    , minimumBrakeTime : Int
    , beams : Int
    , accelerometer : Int
    , accelerometerScale : Int
    , accelerometerXThresh : Int
    , accelerometerYThresh : Int
    , accelerometerZThresh : Int
    , accelerometerXThreshH : Int
    , accelerometerYThreshH : Int
    , accelerometerZThreshH : Int
    , accelerometerRounds : Int
    , accelerometerRounds2 : Int
    , accelerometerDelta : Int
    , accelerometerWaitTime : Int
    , accelerometerDrainTime : Int
    , detergents : Int
    , detergentCleaningTime : Int
    , preloadTime : Int
    , detergentLoadTime : Int
    , minSecEnable : Int
    , coinAcceptorUnlocking : Int
    , alarmInhibition : Int
    , autoStart : Int
    , continousRun : Int
    , dateTime : Int
    , showDetergentExclusion : Int
    , pedantic : Int
    , rgbStop : Int
    , rgbWork : Int
    , rgbPause : Int
    , rgbWaiting : Int
    , rgbWarning : Int
    , rgbAlarm : Int
    , excludedDetergent : Int
    , globalPriceValue : Int
    , frontType : Int
    , lockType : Int
    , lockPulse : Int
    }


type alias MachineParameter =
    Parameter MachineParameters MachineParameters


type alias WashParameter =
    Parameter WashingStep MachineParameters


type alias MachineConfiguration =
    { parmac : MachineParameters
    , parmacMetadata : List MachineParameter
    , cycles : Array WashingCycle
    }


swapWashCycles : Int -> Int -> MachineConfiguration -> ( MachineConfiguration, Int )
swapWashCycles oldIndex newIndex config =
    let
        ( newCycles, result ) =
            swapElements oldIndex newIndex config.cycles
    in
    ( { config | cycles = newCycles }, result )


formatWithUM : String -> MachineParameters -> Context -> Int -> String
formatWithUM um _ _ value =
    String.fromInt value ++ " " ++ um


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


boolOptions : List Intl.IntlString
boolOptions =
    [ Intl.Disabilitato, Intl.Abilitato ]


celsius =
    "°"



-- "\u{00B0}"


parameterMetadataList : List MachineParameter
parameterMetadataList =
    let
        colorOptions =
            [ Intl.Spento, Intl.Blu, Intl.Verde, Intl.Azzurro, Intl.Rosso, Intl.Viola, Intl.Giallo, Intl.Bianco ]

        uiOptions =
            [ Intl.Self, Intl.Laboratorio ]

        formatPrice : MachineParameters -> Context -> Int -> String
        formatPrice { priceDecimalDigits } _ val =
            Parameter.formatPrice priceDecimalDigits val
    in
    [ { get = .language, set = \v p -> { p | language = v }, min = 0, max = 1, default = 0, description = Intl.Lingua, format = formatOption [ Intl.ItalianoCSV, Intl.Inglese ], ui = Parameter.Option }
    , { get = .maxUiLanguages, set = \v p -> { p | maxUiLanguages = v }, min = 0, max = 1, default = 1, description = Intl.LingueSelezionabiliDaFrontale, format = formatNumber, ui = Parameter.Number }
    , { get = .logo, set = \v p -> { p | logo = v }, min = 0, max = 5, default = 1, description = Intl.Logo, format = formatOption [ Intl.Nessuno, Intl.Ms, Intl.Lavenda, Intl.Rotondi, Intl.Schultess, Intl.Hoover ], ui = Parameter.Option }
    , { get = .nodeCode, set = \v p -> { p | nodeCode = v }, min = 0, max = 255, default = 0, description = Intl.CodiceNodoMacchina, format = formatNumber, ui = Parameter.Number }
    , { get = .machineModel, set = \v p -> { p | machineModel = v }, min = 0, max = 255, default = 255, description = Intl.ModelloMacchina, format = formatNumber, ui = Parameter.Number }
    , { get = .machineSubModel, set = \v p -> { p | machineSubModel = v }, min = 0, max = 255, default = 255, description = Intl.SottomodelloMacchina, format = formatNumber, ui = Parameter.Number }
    , { get = .accessLevel, set = \v p -> { p | accessLevel = v }, min = 0, max = 3, default = 0, description = Intl.LivelloDiAccesso, format = formatOption [ Intl.Utente, Intl.Tecnico, Intl.Distributore, Intl.Costruttore ], ui = Parameter.Option }
    , { get = .stopUi, set = \v p -> { p | stopUi = v }, min = 0, max = 1, default = 0, description = Intl.InterfacciaDaFermo, format = formatOption uiOptions, ui = Parameter.Option }
    , { get = .startUi, set = \v p -> { p | startUi = v }, min = 0, max = 1, default = 0, description = Intl.InterfacciaInFunzionamento, format = formatOption uiOptions, ui = Parameter.Option }
    , { get = .maxPrograms, set = \v p -> { p | maxPrograms = v }, min = 0, max = 100, default = 100, description = Intl.NumeroMassimoDiProgrammi, format = formatNumber, ui = Parameter.Number }
    , { get = .maxUserPrograms, set = \v p -> { p | maxUserPrograms = v }, min = 0, max = 100, default = 20, description = Intl.NumeroMassimoDiProgrammiUtente, format = formatNumber, ui = Parameter.Number }
    , { get = .showKg, set = \v p -> { p | showKg = v }, min = 0, max = 100, default = 0, description = Intl.VisualizzazioneKg, format = formatWithUM "kg", ui = Parameter.Number }
    , { get = .showMachinePrice, set = \v p -> { p | showMachinePrice = v }, min = 0, max = 0xFFFF, default = 0, description = Intl.PrezzoMacchina, format = formatPrice, ui = Parameter.Price }
    , { get = .menuUi, set = \v p -> { p | menuUi = v }, min = 0, max = 1, default = 0, description = Intl.MenuParametri, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .soapMenuUi, set = \v p -> { p | soapMenuUi = v }, min = 0, max = 1, default = 0, description = Intl.MenuSaponi, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .showTotalCycles, set = \v p -> { p | showTotalCycles = v }, min = 0, max = 1, default = 0, description = Intl.MostraTotaleCicli, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .scheduledProgram, set = \v p -> { p | scheduledProgram = v }, min = 0, max = 1, default = 0, description = Intl.LavaggioProgrammato, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .coinAcceptor, set = \v p -> { p | coinAcceptor = v }, min = 0, max = 8, default = 0, description = Intl.AbilitazionePagamento, format = formatOption [ Intl.Nessuno, Intl.GettonieraNA, Intl.GettonieraNC, Intl.GettoniereNA, Intl.GettoniereNC, Intl.GettonieraDigitale, Intl.GettonieraLineaSingola, Intl.CassaNA, Intl.CassaNC ], ui = Parameter.Option }
    , { get = .pulseValue, set = \v p -> { p | pulseValue = v }, min = 1, max = 0xFFFF, default = 10, description = Intl.ValoreImpulso, format = formatNumber, ui = Parameter.Number }
    , { get = .globalPriceValue, set = \v p -> { p | globalPriceValue = v }, min = 1, max = 0xFFFF, default = 500, description = Intl.ValorePrezzoUnico, format = formatPrice, ui = Parameter.Price }
    , { get = .globalPrice, set = \v p -> { p | globalPrice = v }, min = 0, max = 1, default = 0, description = Intl.PrezzoUnico, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .priceDigits, set = \v p -> { p | priceDigits = v }, min = 1, max = 6, default = 4, description = Intl.CifrePrezzo, format = formatNumber, ui = Parameter.Number }
    , { get = .priceDecimalDigits, set = \v p -> { p | priceDecimalDigits = v }, min = 0, max = 6, default = 2, description = Intl.CifreDecimaliPrezzo, format = formatNumber, ui = Parameter.Number }
    , { get = .priceVisualization, set = \v p -> { p | priceVisualization = v }, min = 0, max = 4, default = 0, description = Intl.ModoVisualizzazionePrezzo, format = formatOption [ Intl.Nessuno, Intl.Prezzo, Intl.PrezzoERimanente, Intl.PrezzoECredito, Intl.RimanenteECredito ], ui = Parameter.Option }
    , { get = .paymentRequest, set = \v p -> { p | paymentRequest = v }, min = 0, max = 2, default = 0, description = Intl.TipoDiPagamento, format = formatOption [ Intl.InserireGettone, Intl.InserireMoneta, Intl.PagamentoCassa, Intl.PagamentoImporto ], ui = Parameter.Option }
    , { get = .coinAcceptorUnlocking, set = \v p -> { p | coinAcceptorUnlocking = v }, min = 0, max = 3, default = 0, description = Intl.SbloccoGettoniera, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .pausePressTime, set = \v p -> { p | pausePressTime = v }, min = 0, max = 10, default = 1, description = Intl.TempoDiAccettazioneDelTastoPausa, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .stopPressTime, set = \v p -> { p | stopPressTime = v }, min = 0, max = 10, default = 1, description = Intl.TempoDiAccettazioneDelTastoStop, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .levelAlarmTime, set = \v p -> { p | levelAlarmTime = v }, min = 1, max = 100, default = 30, description = Intl.TempoDiAllarmeNoRiempimento, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .temperatureAlarmTime, set = \v p -> { p | temperatureAlarmTime = v }, min = 1, max = 100, default = 45, description = Intl.TempoDiAllarmeNoRiscaldamento, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .drainAlarmTime, set = \v p -> { p | drainAlarmTime = v }, min = 1, max = 100, default = 5, description = Intl.TempoDiAllarmeNoScarico, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .portHoleSwitchDelay, set = \v p -> { p | portHoleSwitchDelay = v }, min = 0, max = 240, default = 15, description = Intl.TempoDiAttesaDelMicroDelChiavistello, format = formatNumber, ui = Parameter.Number }
    , { get = .preloadTime, set = \v p -> { p | preloadTime = v }, min = 0, max = 240, default = 10, description = Intl.TempoDiPrecarica, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .detergentCleaningTime, set = \v p -> { p | detergentCleaningTime = v }, min = 0, max = 240, default = 10, description = Intl.TempoPuliziaSaponi, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .detergentLoadTime, set = \v p -> { p | detergentLoadTime = v }, min = 0, max = 240, default = 5, description = Intl.TempoTastoDiRabboccoSaponi, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .serviceDrainTime, set = \v p -> { p | serviceDrainTime = v }, min = 1, max = 240, default = 15, description = Intl.TempoDiScaricoServizio, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .drainPulseTime, set = \v p -> { p | drainPulseTime = v }, min = 1, max = 240, default = 24, description = Intl.TempoDellImpulsoDiAperturaDelloScarico, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .drainMinTime, set = \v p -> { p | drainMinTime = v }, min = 1, max = 240, default = 10, description = Intl.TempoMinimoDiScarico, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .minimumBrakeTime, set = \v p -> { p | minimumBrakeTime = v }, min = 1, max = 250, default = 45, description = Intl.TempoMinimoDiFrenata, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .basketDiameter, set = \v p -> { p | basketDiameter = v }, min = 0, max = 10000, default = 0, description = Intl.DiametroDelCesto, format = formatNumber, ui = Parameter.Number }
    , { get = .basketDepth, set = \v p -> { p | basketDepth = v }, min = 0, max = 10000, default = 0, description = Intl.ProfonditaDelCesto, format = formatNumber, ui = Parameter.Number }
    , { get = .trapHeight, set = \v p -> { p | trapHeight = v }, min = 0, max = 1000, default = 0, description = Intl.AltezzaTrappola, format = formatNumber, ui = Parameter.Number }
    , { get = .ioExpander, set = \v p -> { p | ioExpander = v }, min = 0, max = 1, default = 0, description = Intl.EspansioneIO, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .detergents, set = \v p -> { p | detergents = v }, min = 3, max = 10, default = 5, description = Intl.NumeroSaponiUtilizzabili, format = formatNumber, ui = Parameter.Number }
    , { get = .excludedDetergent, set = \v p -> { p | excludedDetergent = v }, min = 0, max = 10, default = 0, description = Intl.EsclusioneSapone, format = formatNumber, ui = Parameter.Number }
    , { get = .machineBusy, set = \v p -> { p | machineBusy = v }, min = 0, max = 2, default = 2, description = Intl.AbilitazioneMacchinaLiberaETipo, format = formatOption [ Intl.NonGestita, Intl.CommutaAlloStart, Intl.CommutaAlPagamento ], ui = Parameter.Option }
    , { get = .machineBusySignal, set = \v p -> { p | machineBusySignal = v }, min = 0, max = 1, default = 0, description = Intl.ContattoMacchinaLibera, format = formatOption [ Intl.NormalmenteChiuso, Intl.NormalmenteAperto ], ui = Parameter.Option }
    , { get = .auxOut1Type, set = \v p -> { p | auxOut1Type = v }, min = 0, max = 1, default = 0, description = Intl.TipologiaDellIngressoAusiliario1, format = formatOption [ Intl.SbloccoPagamento, Intl.StandbySaponi ], ui = Parameter.Option }
    , { get = .auxOut2Type, set = \v p -> { p | auxOut2Type = v }, min = 0, max = 1, default = 1, description = Intl.TipologiaDellUscitaAusiliaria2, format = formatOption [ Intl.MacchinaOccupata, Intl.Lampeggiante ], ui = Parameter.Option }
    , { get = .auxOut3Type, set = \v p -> { p | auxOut3Type = v }, min = 0, max = 1, default = 1, description = Intl.TipologiaDellUscitaAusiliaria3, format = formatOption [ Intl.H2ORecuperoDepurata, Intl.PompaRicircolo ], ui = Parameter.Option }
    , { get = .auxOut4Type, set = \v p -> { p | auxOut4Type = v }, min = 0, max = 1, default = 1, description = Intl.TipologiaDellUscitaAusiliaria4, format = formatOption [ Intl.ScaricoRecupero, Intl.RiscaldamentoIndiretto ], ui = Parameter.Option }
    , { get = .drainRecycle, set = \v p -> { p | drainRecycle = v }, min = 0, max = 1, default = 0, description = Intl.ValvolaDiScaricoRecupero, format = formatOption [ Intl.NormalmenteChiuso, Intl.NormalmenteAperto ], ui = Parameter.Option }
    , { get = .levelReducedLoad, set = \v p -> { p | levelReducedLoad = v }, min = 10, max = 190, default = 100, description = Intl.PercentualeLivelloConCaricoRidotto, format = formatNumber, ui = Parameter.Number }
    , { get = .detergentReducedLoad, set = \v p -> { p | detergentReducedLoad = v }, min = 10, max = 190, default = 100, description = Intl.PercentualeSaponeConCaricoRidotto, format = formatNumber, ui = Parameter.Number }
    , { get = .autoStart, set = \v p -> { p | autoStart = v }, min = 0, max = 1, default = 0, description = Intl.Autoavvio, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .proximity, set = \v p -> { p | proximity = v }, min = 0, max = 1, default = 0, description = Intl.SensoreDiProssimita, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .beams, set = \v p -> { p | beams = v }, min = 1, max = 12, default = 6, description = Intl.NumeroDiRaggi, format = formatNumber, ui = Parameter.Number }
    , { get = .revCounterCorrection, set = \v p -> { p | revCounterCorrection = v }, min = 0, max = 200, default = 111, description = Intl.FattoreDiCorrezioneAlContagiri, format = formatNumber, ui = Parameter.Number }
    , { get = .accelerometer, set = \v p -> { p | accelerometer = v }, min = 0, max = 3, default = 2, description = Intl.Accelerometro, format = formatOption [ Intl.Disabilitato, Intl.Velocita1, Intl.Velocita2SoglieDelta, Intl.Velocita3SoglieHi ], ui = Parameter.Option }
    , { get = .accelerometerScale, set = \v p -> { p | accelerometerScale = v }, min = 0, max = 5, default = 3, description = Intl.ScalaAccelerometro, format = formatNumber, ui = Parameter.Number }
    , { get = .accelerometerXThresh, set = \v p -> { p | accelerometerXThresh = v }, min = 0, max = 511, default = 100, description = Intl.SogliaXAccelerometro, format = formatNumber, ui = Parameter.Number }
    , { get = .accelerometerYThresh, set = \v p -> { p | accelerometerYThresh = v }, min = 0, max = 511, default = 90, description = Intl.SogliaYAccelerometro, format = formatNumber, ui = Parameter.Number }
    , { get = .accelerometerZThresh, set = \v p -> { p | accelerometerZThresh = v }, min = 0, max = 511, default = 110, description = Intl.SogliaZAccelerometro, format = formatNumber, ui = Parameter.Number }
    , { get = .accelerometerXThreshH, set = \v p -> { p | accelerometerXThreshH = v }, min = 0, max = 511, default = 155, description = Intl.SogliaXAccelerometroH, format = formatNumber, ui = Parameter.Number }
    , { get = .accelerometerYThreshH, set = \v p -> { p | accelerometerYThreshH = v }, min = 0, max = 511, default = 135, description = Intl.SogliaYAccelerometroH, format = formatNumber, ui = Parameter.Number }
    , { get = .accelerometerZThreshH, set = \v p -> { p | accelerometerZThreshH = v }, min = 0, max = 511, default = 110, description = Intl.SogliaZAccelerometroH, format = formatNumber, ui = Parameter.Number }
    , { get = .accelerometerRounds, set = \v p -> { p | accelerometerRounds = v }, min = 0, max = 1000, default = 200, description = Intl.GiriAccelerometro, format = formatNumber, ui = Parameter.Number }
    , { get = .accelerometerRounds2, set = \v p -> { p | accelerometerRounds2 = v }, min = 0, max = 1000, default = 300, description = Intl.GiriAccelerometro2, format = formatNumber, ui = Parameter.Number }
    , { get = .accelerometerDelta, set = \v p -> { p | accelerometerDelta = v }, min = 0, max = 100, default = 50, description = Intl.DeltaAccelerometro, format = formatNumber, ui = Parameter.Number }
    , { get = .accelerometerWaitTime, set = \v p -> { p | accelerometerWaitTime = v }, min = 0, max = 60, default = 20, description = Intl.TempoDiAttesaDellAccelerometro, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .accelerometerDrainTime, set = \v p -> { p | accelerometerDrainTime = v }, min = 0, max = 1000, default = 20, description = Intl.TempoDiScaricoDellAccelerometro, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .maxTemperature, set = \v p -> { p | maxTemperature = v }, min = 0, max = 100, default = 90, description = Intl.TemperaturaMassima, format = formatWithUM celsius, ui = Parameter.Number }
    , { get = .temperatureHysteresis, set = \v p -> { p | temperatureHysteresis = v }, min = 0, max = 60, default = 2, description = Intl.IsteresiDellaTemperatura, format = formatWithUM celsius, ui = Parameter.Number }
    , { get = .safetyTemperature, set = \v p -> { p | safetyTemperature = v }, min = 0, max = 99, default = 95, description = Intl.TemperaturaDiSicurezza, format = formatWithUM celsius, ui = Parameter.Number }
    , { get = .termodegratationTemperature, set = \v p -> { p | termodegratationTemperature = v }, min = 0, max = 60, default = 45, description = Intl.TemperaturaDiTermodegradazione, format = formatWithUM celsius, ui = Parameter.Number }
    , { get = .levelType, set = \v p -> { p | levelType = v }, min = 0, max = 2, default = 0, description = Intl.TipoLivello, format = formatOption [ Intl.Centimetri, Intl.UnContalitri, Intl.DueContalitri ], ui = Parameter.Option }
    , { get = .levelHysteresisTime, set = \v p -> { p | levelHysteresisTime = v }, min = 1, max = 60, default = 3, description = Intl.TempoDiIsteresiLivello, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .cmMaxLevel, set = \v p -> { p | cmMaxLevel = v }, min = 2, max = 100, default = 48, description = Intl.CentimetriLivelloMassimo, format = formatWithUM "cm", ui = Parameter.Number }
    , { get = .surfaceLevel, set = \v p -> { p | surfaceLevel = v }, min = 15, max = 10000, default = 50, description = Intl.LivelloDiSfioro, format = formatNumber, ui = Parameter.Number }
    , { get = .cmMinDrain, set = \v p -> { p | cmMinDrain = v }, min = 0, max = 30, default = 1, description = Intl.LivelloMinimoScarico, format = formatNumber, ui = Parameter.Number }
    , { get = .cmMinHeating, set = \v p -> { p | cmMinHeating = v }, min = 2, max = 30, default = 4, description = Intl.LivelloMinimoRiscaldamento, format = formatNumber, ui = Parameter.Number }
    , { get = .maxLiters, set = \v p -> { p | maxLiters = v }, min = 15, max = 10000, default = 50, description = Intl.LitriMassimiDiRiempimento, format = formatNumber, ui = Parameter.Number }
    , { get = .ltMinHeating, set = \v p -> { p | ltMinHeating = v }, min = 1, max = 1000, default = 20, description = Intl.LitriMinimiInRiscaldamento, format = formatNumber, ui = Parameter.Number }
    , { get = .pulsesPerLiter, set = \v p -> { p | pulsesPerLiter = v }, min = 0, max = 10000, default = 328, description = Intl.ImpulsiPerLitro, format = formatNumber, ui = Parameter.Number }
    , { get = .inverterType, set = \v p -> { p | inverterType = v }, min = 0, max = 1, default = 328, description = Intl.ModelloDiInverter, format = formatOption [ Intl.AvantiIndietro, Intl.MarciaDirezione ], ui = Parameter.Number }
    , { get = .serviceSpeed, set = \v p -> { p | serviceSpeed = v }, min = 1, max = 100, default = 36, description = Intl.VelocitaDiServizio, format = formatWithUM "rpm", ui = Parameter.Number }
    , { get = .minimumWashSpeed, set = \v p -> { p | minimumWashSpeed = v }, min = 0, max = 150, default = 20, description = Intl.VelocitaMinimaDiLavaggio, format = formatWithUM "rpm", ui = Parameter.Number }
    , { get = .maximumWashSpeed, set = \v p -> { p | maximumWashSpeed = v }, min = 0, max = 150, default = 60, description = Intl.VelocitaMassimaDiLavaggio, format = formatWithUM "rpm", ui = Parameter.Number }
    , { get = .preparationRotationEnable, set = \v p -> { p | preparationRotationEnable = v }, min = 0, max = 9, default = 3, description = Intl.NumeroDiCicliDiPreparazione, format = formatNumber, ui = Parameter.Number }
    , { get = .preparationRotationTime, set = \v p -> { p | preparationRotationTime = v }, min = 1, max = 200, default = 20, description = Intl.TempoDiMarciaInPreparazione, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .preparationPauseTime, set = \v p -> { p | preparationPauseTime = v }, min = 2, max = 240, default = 5, description = Intl.TempoDiSostaInPreparazione, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .preparationMinimumSpeed, set = \v p -> { p | preparationMinimumSpeed = v }, min = 1, max = 200, default = 20, description = Intl.VelocitaMinimaDiPreparazione, format = formatWithUM "rpm", ui = Parameter.Number }
    , { get = .preparationMaximumSpeed, set = \v p -> { p | preparationMaximumSpeed = v }, min = 1, max = 200, default = 50, description = Intl.VelocitaMassimaDiPreparazione, format = formatWithUM "rpm", ui = Parameter.Number }
    , { get = .centrifuge1MinimumSpeed, set = \v p -> { p | centrifuge1MinimumSpeed = v }, min = 0, max = 1200, default = 1, description = Intl.VelocitaMinimaDellaCentrifuga1, format = formatWithUM "rpm", ui = Parameter.Number }
    , { get = .centrifuge1MaximumSpeed, set = \v p -> { p | centrifuge1MaximumSpeed = v }, min = 0, max = 1200, default = 1000, description = Intl.VelocitaMassimaDellaCentrifuga1, format = formatWithUM "rpm", ui = Parameter.Number }
    , { get = .centrifuge2MinimumSpeed, set = \v p -> { p | centrifuge2MinimumSpeed = v }, min = 0, max = 1200, default = 1, description = Intl.VelocitaMinimaDellaCentrifuga2, format = formatWithUM "rpm", ui = Parameter.Number }
    , { get = .centrifuge2MaximumSpeed, set = \v p -> { p | centrifuge2MaximumSpeed = v }, min = 0, max = 1200, default = 1000, description = Intl.VelocitaMassimaDellaCentrifuga2, format = formatWithUM "rpm", ui = Parameter.Number }
    , { get = .centrifuge3MinimumSpeed, set = \v p -> { p | centrifuge3MinimumSpeed = v }, min = 0, max = 1200, default = 1, description = Intl.VelocitaMinimaDellaCentrifuga3, format = formatWithUM "rpm", ui = Parameter.Number }
    , { get = .centrifuge3MaximumSpeed, set = \v p -> { p | centrifuge3MaximumSpeed = v }, min = 0, max = 1200, default = 1000, description = Intl.VelocitaMassimaDellaCentrifuga3, format = formatWithUM "rpm", ui = Parameter.Number }
    , { get = .rampMinimumTime, set = \v p -> { p | rampMinimumTime = v }, min = 3, max = 1000, default = 15, description = Intl.TempoMinimoDellaRampa, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .rampMaximumTime, set = \v p -> { p | rampMaximumTime = v }, min = 3, max = 1000, default = 90, description = Intl.TempoMassimoDellaRampa, format = formatWithUM "s", ui = Parameter.Number }
    , { get = .maxLurchAttempts, set = \v p -> { p | maxLurchAttempts = v }, min = 1, max = 60, default = 35, description = Intl.NumeroMassimoDiSbilanciamenti, format = formatNumber, ui = Parameter.Number }
    , { get = .minSecEnable, set = \v p -> { p | minSecEnable = v }, min = 0, max = 1, default = 0, description = Intl.AbilitazioneDelCambioMinutiSecondi, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .lockType, set = \v p -> { p | lockType = v }, min = 0, max = 3, default = 0, description = Intl.TipoDiSerratura, format = formatOption [ Intl.NuovaTreMicroLivello, Intl.BobinaUnMicroLivello, Intl.NuovaTreMicroNoLivello, Intl.BobinaUnMicroNoLivello ], ui = Parameter.Option }
    , { get = .maxLurchAttempts, set = \v p -> { p | maxLurchAttempts = v }, min = 1, max = 60, default = 35, description = Intl.NumeroMassimoDiSbilanciamenti, format = formatNumber, ui = Parameter.Number }
    , { get = .lockPulse, set = \v p -> { p | lockPulse = v }, min = 5, max = 30, default = 8, description = Intl.DurataDellImpulsoDellaSerratura, format = formatNumber, ui = Parameter.Number }
    , { get = .alarmInhibition, set = \v p -> { p | alarmInhibition = v }, min = 0, max = 1, default = 0, description = Intl.InibizioneAllarmi, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .continousRun, set = \v p -> { p | continousRun = v }, min = 0, max = 1, default = 0, description = Intl.CicloContinuo, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .dateTime, set = \v p -> { p | dateTime = v }, min = 0, max = 1, default = 0, description = Intl.VisualizzazioneDataEOra, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .pedantic, set = \v p -> { p | dateTime = v }, min = 0, max = 1, default = 0, description = Intl.VisualizzazionePedante, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .showDetergentExclusion, set = \v p -> { p | showDetergentExclusion = v }, min = 0, max = 1, default = 0, description = Intl.VisualizzazioneEsclusioneSapone, format = formatOption boolOptions, ui = Parameter.Option }
    , { get = .rgbStop, set = \v p -> { p | rgbStop = v }, min = 0, max = 7, default = 7, description = Intl.ColoreLEDDaFermo, format = formatOption colorOptions, ui = Parameter.Option }
    , { get = .rgbWork, set = \v p -> { p | rgbWork = v }, min = 0, max = 7, default = 1, description = Intl.ColoreLEDAlLavoro, format = formatOption colorOptions, ui = Parameter.Option }
    , { get = .rgbPause, set = \v p -> { p | rgbPause = v }, min = 0, max = 7, default = 6, description = Intl.ColoreLEDInPausa, format = formatOption colorOptions, ui = Parameter.Option }
    , { get = .rgbWaiting, set = \v p -> { p | rgbWaiting = v }, min = 0, max = 7, default = 2, description = Intl.ColoreLEDInAttesa, format = formatOption colorOptions, ui = Parameter.Option }
    , { get = .rgbWarning, set = \v p -> { p | rgbWarning = v }, min = 0, max = 7, default = 3, description = Intl.ColoreLEDConAvviso, format = formatOption colorOptions, ui = Parameter.Option }
    , { get = .rgbAlarm, set = \v p -> { p | rgbAlarm = v }, min = 0, max = 7, default = 4, description = Intl.ColoreLEDConAllarme, format = formatOption colorOptions, ui = Parameter.Option }
    ]


stepParameterMetadataList : Int -> MachineParameters -> List WashParameter
stepParameterMetadataList stepType { maxTemperature, levelType, cmMinHeating, cmMaxLevel, ltMinHeating, maxLiters, preparationMinimumSpeed, preparationMaximumSpeed, centrifuge1MinimumSpeed, centrifuge1MaximumSpeed, centrifuge2MinimumSpeed, centrifuge2MaximumSpeed, centrifuge3MinimumSpeed, centrifuge3MaximumSpeed, rampMinimumTime, rampMaximumTime, minimumBrakeTime } =
    let
        ( minLevel, maxLevel ) =
            if levelType == 0 then
                ( cmMinHeating, cmMaxLevel )

            else
                ( ltMinHeating, maxLiters )

        activeTimeStrings =
            [ Intl.Subito, Intl.DopoLivello, Intl.DopoTemperatura, Intl.DopoLivelloETemperatura, Intl.ConVelocitaRiempimento ]

        duration =
            { get = .duration, set = \v p -> { p | duration = v }, min = 0, max = 3600, default = 120, description = Intl.Durata, format = formatWithUM "s", ui = Parameter.Number }

        activeTime max =
            { get = .activeTime, set = \v p -> { p | activeTime = v }, min = 0, max = max, default = 0, description = Intl.TempoAttivo, format = formatOption activeTimeStrings, ui = Parameter.Option }

        movingWhileFilling =
            { get = .movingWhileFilling, set = \v p -> { p | movingWhileFilling = v }, min = 0, max = 1, default = 0, description = Intl.MotoCestoInRiempimento, format = formatOption boolOptions, ui = Parameter.Option }

        fillingSpeed =
            { get = .fillingSpeed, set = \v p -> { p | fillingSpeed = v }, min = 0, max = 150, default = 40, description = Intl.VelocitaInRiempimento, format = formatNumber, ui = Parameter.Number }

        fillingInversion =
            { get = .fillingInversion, set = \v p -> { p | fillingInversion = v }, min = 0, max = 1, default = 0, description = Intl.InversioneInRiempimento, format = formatOption boolOptions, ui = Parameter.Option }

        fillingMotion =
            { get = .fillingMotion, set = \v p -> { p | fillingMotion = v }, min = 0, max = 900, default = 0, description = Intl.TempoDiMotoInRiempimento, format = formatWithUM "s", ui = Parameter.Number }

        fillingPause =
            { get = .fillingPause, set = \v p -> { p | fillingPause = v }, min = 0, max = 900, default = 0, description = Intl.TempoDiPausaInRiempimento, format = formatWithUM "s", ui = Parameter.Number }

        washSpeed =
            { get = .washSpeed, set = \v p -> { p | washSpeed = v }, min = 0, max = 140, default = 0, description = Intl.VelocitaInLavaggio, format = formatWithUM "rpm", ui = Parameter.Number }

        washInversion =
            { get = .washInversion, set = \v p -> { p | washInversion = v }, min = 0, max = 1, default = 0, description = Intl.InversioneInLavaggio, format = formatOption boolOptions, ui = Parameter.Option }

        washMotion =
            { get = .washMotion, set = \v p -> { p | washMotion = v }, min = 0, max = 900, default = 0, description = Intl.TempoDiMotoAlLavoro, format = formatWithUM "s", ui = Parameter.Number }

        washPause =
            { get = .washPause, set = \v p -> { p | washPause = v }, min = 0, max = 900, default = 0, description = Intl.TempoDiPausaAlLavoro, format = formatWithUM "s", ui = Parameter.Number }

        heating =
            { get = .heating, set = \v p -> { p | heating = v }, min = 0, max = 1, default = 0, description = Intl.Riscaldamento, format = formatOption boolOptions, ui = Parameter.Option }

        temperature =
            { get = .temperature, set = \v p -> { p | temperature = v }, min = 0, max = maxTemperature, default = 0, description = Intl.Temperatura, format = formatWithUM celsius, ui = Parameter.Number }

        heatingType =
            { get = .heatingType, set = \v p -> { p | heatingType = v }, min = 0, max = 1, default = 1, description = Intl.TipoDiRiscaldamento, format = formatOption [ Intl.Diretto, Intl.Indiretto ], ui = Parameter.Option }

        continousTemperatureControl =
            { get = .continousTemperatureControl, set = \v p -> { p | continousTemperatureControl = v }, min = 0, max = 1, default = 0, description = Intl.ControlloDellaTemperaturaContinuo, format = formatOption boolOptions, ui = Parameter.Option }

        level =
            { get = .level, set = \v p -> { p | level = v }, min = minLevel, max = maxLevel, default = 0, description = Intl.Livello, format = formatNumber, ui = Parameter.Number }

        recycling =
            { get = .recycling, set = \v p -> { p | recycling = v }, min = 0, max = 1, default = 0, description = Intl.Ricircolo, format = formatOption boolOptions, ui = Parameter.Option }

        coldWater =
            { get = .coldWater, set = \v p -> { p | coldWater = v }, min = 0, max = 1, default = 1, description = Intl.AcquaFredda, format = formatOption boolOptions, ui = Parameter.Option }

        warmWater =
            { get = .warmWater, set = \v p -> { p | warmWater = v }, min = 0, max = 1, default = 1, description = Intl.AcquaCalda, format = formatOption boolOptions, ui = Parameter.Option }

        purifiedWater =
            { get = .purifiedWater, set = \v p -> { p | purifiedWater = v }, min = 0, max = 1, default = 1, description = Intl.AcquaDepurata, format = formatOption boolOptions, ui = Parameter.Option }

        detergentActiveTime max =
            { get = .activeTime, set = \v p -> { p | activeTime = v }, min = 0, max = max, default = 0, description = Intl.TempoAttivoSapone, format = formatOption activeTimeStrings, ui = Parameter.Option }

        detergent1Time =
            { get = .detergent1Time, set = \v p -> { p | detergent1Time = v }, min = 0, max = 240, default = 0, description = Intl.TempoSapone1, format = formatWithUM "s", ui = Parameter.Number }

        detergent1Delay =
            { get = .detergent1Delay, set = \v p -> { p | detergent1Delay = v }, min = 0, max = 3600, default = 0, description = Intl.RitardoSapone1, format = formatWithUM "s", ui = Parameter.Number }

        detergent2Time =
            { get = .detergent2Time, set = \v p -> { p | detergent2Time = v }, min = 0, max = 240, default = 0, description = Intl.TempoSapone2, format = formatWithUM "s", ui = Parameter.Number }

        detergent2Delay =
            { get = .detergent2Delay, set = \v p -> { p | detergent2Delay = v }, min = 0, max = 3600, default = 0, description = Intl.RitardoSapone2, format = formatWithUM "s", ui = Parameter.Number }

        detergent3Time =
            { get = .detergent3Time, set = \v p -> { p | detergent3Time = v }, min = 0, max = 240, default = 0, description = Intl.TempoSapone3, format = formatWithUM "s", ui = Parameter.Number }

        detergent3Delay =
            { get = .detergent3Delay, set = \v p -> { p | detergent3Delay = v }, min = 0, max = 3600, default = 0, description = Intl.RitardoSapone3, format = formatWithUM "s", ui = Parameter.Number }

        detergent4Time =
            { get = .detergent4Time, set = \v p -> { p | detergent4Time = v }, min = 0, max = 240, default = 0, description = Intl.TempoSapone4, format = formatWithUM "s", ui = Parameter.Number }

        detergent4Delay =
            { get = .detergent4Delay, set = \v p -> { p | detergent4Delay = v }, min = 0, max = 3600, default = 0, description = Intl.RitardoSapone4, format = formatWithUM "s", ui = Parameter.Number }

        detergent5Time =
            { get = .detergent5Time, set = \v p -> { p | detergent5Time = v }, min = 0, max = 240, default = 0, description = Intl.TempoSapone5, format = formatWithUM "s", ui = Parameter.Number }

        detergent5Delay =
            { get = .detergent5Delay, set = \v p -> { p | detergent5Delay = v }, min = 0, max = 3600, default = 0, description = Intl.RitardoSapone5, format = formatWithUM "s", ui = Parameter.Number }

        detergent6Time =
            { get = .detergent6Time, set = \v p -> { p | detergent6Time = v }, min = 0, max = 240, default = 0, description = Intl.TempoSapone6, format = formatWithUM "s", ui = Parameter.Number }

        detergent6Delay =
            { get = .detergent6Delay, set = \v p -> { p | detergent6Delay = v }, min = 0, max = 3600, default = 0, description = Intl.RitardoSapone6, format = formatWithUM "s", ui = Parameter.Number }

        detergent7Time =
            { get = .detergent7Time, set = \v p -> { p | detergent7Time = v }, min = 0, max = 240, default = 0, description = Intl.TempoSapone7, format = formatWithUM "s", ui = Parameter.Number }

        detergent7Delay =
            { get = .detergent7Delay, set = \v p -> { p | detergent7Delay = v }, min = 0, max = 3600, default = 0, description = Intl.RitardoSapone7, format = formatWithUM "s", ui = Parameter.Number }

        detergent8Time =
            { get = .detergent8Time, set = \v p -> { p | detergent8Time = v }, min = 0, max = 240, default = 0, description = Intl.TempoSapone8, format = formatWithUM "s", ui = Parameter.Number }

        detergent8Delay =
            { get = .detergent8Delay, set = \v p -> { p | detergent8Delay = v }, min = 0, max = 3600, default = 0, description = Intl.RitardoSapone8, format = formatWithUM "s", ui = Parameter.Number }

        detergent9Time =
            { get = .detergent9Time, set = \v p -> { p | detergent9Time = v }, min = 0, max = 240, default = 0, description = Intl.TempoSapone9, format = formatWithUM "s", ui = Parameter.Number }

        detergent9Delay =
            { get = .detergent9Delay, set = \v p -> { p | detergent9Delay = v }, min = 0, max = 3600, default = 0, description = Intl.RitardoSapone9, format = formatWithUM "s", ui = Parameter.Number }

        detergent10Time =
            { get = .detergent10Time, set = \v p -> { p | detergent10Time = v }, min = 0, max = 240, default = 0, description = Intl.TempoSapone10, format = formatWithUM "s", ui = Parameter.Number }

        detergent10Delay =
            { get = .detergent10Delay, set = \v p -> { p | detergent10Delay = v }, min = 0, max = 3600, default = 0, description = Intl.RitardoSapone10, format = formatWithUM "s", ui = Parameter.Number }

        detergents =
            [ detergent1Time, detergent1Delay, detergent2Time, detergent2Delay, detergent3Time, detergent3Delay, detergent4Time, detergent4Delay, detergent5Time, detergent5Delay, detergent6Time, detergent6Delay, detergent7Time, detergent7Delay, detergent8Time, detergent8Delay, detergent9Time, detergent9Delay, detergent10Time, detergent10Delay ]

        movingWhileWashing =
            { get = .movingWhileWashing, set = \v p -> { p | movingWhileWashing = v }, min = 0, max = 1, default = 0, description = Intl.MotoCesto, format = formatOption boolOptions, ui = Parameter.Option }

        recovery =
            { get = .recovery, set = \v p -> { p | recovery = v }, min = 0, max = 1, default = 0, description = Intl.Recupero, format = formatOption boolOptions, ui = Parameter.Option }

        preparationTime =
            { get = .preparationTime, set = \v p -> { p | preparationTime = v }, min = 0, max = 240, default = 5, description = Intl.TempoDiPreparazione, format = formatWithUM "s", ui = Parameter.Number }

        preparationSpeed =
            { get = .preparationSpeed, set = \v p -> { p | preparationSpeed = v }, min = preparationMinimumSpeed, max = preparationMaximumSpeed, default = 40, description = Intl.VelocitaDiPreparazione, format = formatWithUM "rpm", ui = Parameter.Number }

        drainTime =
            { get = .drainTime, set = \v p -> { p | drainTime = v }, min = 0, max = 2, default = 0, description = Intl.TempoDiScarico, format = formatNumber, ui = Parameter.Number }

        centrifuge1Speed =
            { get = .centrifuge1Speed, set = \v p -> { p | centrifuge1Speed = v }, min = centrifuge1MinimumSpeed, max = centrifuge1MaximumSpeed, default = 40, description = Intl.VelocitaDellaCentrifuga1, format = formatWithUM "rpm", ui = Parameter.Number }

        ramp1Time =
            { get = .ramp1Time, set = \v p -> { p | ramp1Time = v }, min = rampMinimumTime, max = rampMaximumTime, default = 0, description = Intl.TempoDellaRampa1, format = formatWithUM "s", ui = Parameter.Number }

        centrifuge1WaitTime =
            { get = .centrifuge1WaitTime, set = \v p -> { p | centrifuge1WaitTime = v }, min = 0, max = 240, default = 5, description = Intl.TempoDiAttesaDellaCentrifuga1, format = formatWithUM "s", ui = Parameter.Number }

        centrifuge2Speed =
            { get = .centrifuge2Speed, set = \v p -> { p | centrifuge2Speed = v }, min = centrifuge2MinimumSpeed, max = centrifuge2MaximumSpeed, default = 40, description = Intl.VelocitaDellaCentrifuga2, format = formatWithUM "rpm", ui = Parameter.Number }

        ramp2Time =
            { get = .ramp2Time, set = \v p -> { p | ramp2Time = v }, min = rampMinimumTime, max = rampMaximumTime, default = 0, description = Intl.TempoDellaRampa2, format = formatWithUM "s", ui = Parameter.Number }

        centrifuge2WaitTime =
            { get = .centrifuge2WaitTime, set = \v p -> { p | centrifuge2WaitTime = v }, min = 0, max = 240, default = 5, description = Intl.TempoDiAttesaDellaCentrifuga2, format = formatWithUM "s", ui = Parameter.Number }

        centrifuge3Speed =
            { get = .centrifuge3Speed, set = \v p -> { p | centrifuge3Speed = v }, min = centrifuge3MinimumSpeed, max = centrifuge3MaximumSpeed, default = 40, description = Intl.VelocitaDellaCentrifuga3, format = formatWithUM "rpm", ui = Parameter.Number }

        ramp3Time =
            { get = .ramp3Time, set = \v p -> { p | ramp3Time = v }, min = rampMinimumTime, max = rampMaximumTime, default = 0, description = Intl.TempoDellaRampa3, format = formatWithUM "s", ui = Parameter.Number }

        brakeTime =
            { get = .brakeTime, set = \v p -> { p | brakeTime = v }, min = minimumBrakeTime, max = 240, default = 35, description = Intl.TempoDiFrenata, format = formatWithUM "s", ui = Parameter.Number }

        waitTime =
            { get = .waitTime, set = \v p -> { p | waitTime = v }, min = 0, max = 9, default = 0, description = Intl.TempoDiAttesa, format = formatNumber, ui = Parameter.Number }

        ramps =
            { get = .ramps, set = \v p -> { p | ramps = v }, min = 1, max = 3, default = 3, description = Intl.NumeroDiRampe, format = formatNumber, ui = Parameter.Number }

        commonWashParameters =
            [ duration, activeTime 4, movingWhileFilling, fillingSpeed, fillingInversion, fillingMotion, fillingPause, washSpeed, washInversion, washMotion, washPause, heating, temperature, heatingType, continousTemperatureControl, level, recycling, coldWater, warmWater, purifiedWater, detergentActiveTime 3 ] ++ detergents
    in
    case stepType of
        1 ->
            commonWashParameters

        2 ->
            commonWashParameters

        3 ->
            commonWashParameters

        4 ->
            [ duration, activeTime 1, movingWhileFilling, fillingSpeed, fillingInversion, fillingMotion, fillingPause, washSpeed, washInversion, washMotion, washPause, level, recycling, coldWater, warmWater, purifiedWater, detergentActiveTime 1 ] ++ detergents

        5 ->
            [ duration, movingWhileWashing, washSpeed, washInversion, washMotion, washPause, recovery ]

        6 ->
            [ duration, preparationTime, preparationSpeed, drainTime, ramps, centrifuge1Speed, ramp1Time, centrifuge1WaitTime, centrifuge2Speed, ramp2Time, centrifuge2WaitTime, centrifuge3Speed, ramp3Time, brakeTime ]

        7 ->
            [ duration, washSpeed, washMotion, washPause ]

        8 ->
            [ duration, waitTime, movingWhileWashing, washSpeed, washInversion, washMotion, washPause, level, recycling, coldWater, warmWater ]

        _ ->
            []


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
    { config | parmac = { parmac | name = String.slice 0 32 name } }


default : Context -> MachineConfiguration
default context =
    MachineConfiguration
        (MachineParameters (translate Intl.NuovaConfigurazione context) 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        parameterMetadataList
        (Array.fromList [])
        |> resetToDefaults


machineParametersDecoder : Decoder MachineParameters
machineParametersDecoder =
    Decode.succeed MachineParameters
        |> Decode.andMap (Decode.string 33)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt32 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap Decode.unsignedInt8
        |> Decode.andMap Decode.unsignedInt8
        |> Decode.andMap Decode.unsignedInt8
        |> Decode.andMap Decode.unsignedInt8
        |> Decode.andMap Decode.unsignedInt8
        |> Decode.andMap Decode.unsignedInt8
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt32 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)


encodeMachineParameters : MachineParameters -> Bytes
encodeMachineParameters pars =
    let
        encodePar8 lens =
            Encode.unsignedInt8 (lens pars)

        encodePar16 lens =
            Encode.unsignedInt16 BE (lens pars)

        encodePar32 lens =
            Encode.unsignedInt32 BE (lens pars)
    in
    Encode.sequence
        [ limitedStringEncoder pars.name
        , encodePar16 .language
        , encodePar16 .maxUiLanguages
        , encodePar16 .logo
        , encodePar16 .machineModel
        , encodePar16 .machineSubModel
        , encodePar16 .nodeCode
        , encodePar16 .basketDiameter
        , encodePar16 .basketDepth
        , encodePar16 .trapHeight
        , encodePar16 .revCounterCorrection
        , encodePar16 .maxPrograms
        , encodePar16 .maxUserPrograms
        , encodePar16 .uselessPar0
        , encodePar16 .accessLevel
        , encodePar16 .stopUi
        , encodePar16 .startUi
        , encodePar16 .menuUi
        , encodePar16 .soapMenuUi
        , encodePar16 .showTotalCycles
        , encodePar16 .scheduledProgram
        , encodePar16 .showKg
        , encodePar32 .showMachinePrice
        , encodePar16 .uselessPar1
        , encodePar16 .uselessPar2
        , encodePar16 .uselessPar3
        , encodePar16 .uselessPar4
        , encodePar16 .uselessPar5
        , encodePar16 .pausePressTime
        , encodePar16 .stopPressTime
        , encodePar16 .uselessPar6
        , encodePar16 .levelAlarmTime
        , encodePar16 .temperatureAlarmTime
        , encodePar16 .drainAlarmTime
        , encodePar16 .portHoleSwitchDelay
        , encodePar16 .auxOut1Type
        , encodePar16 .auxOut2Type
        , encodePar16 .auxOut3Type
        , encodePar16 .auxOut4Type
        , encodePar16 .levelReducedLoad
        , encodePar16 .detergentReducedLoad
        , encodePar16 .drainRecycle
        , encodePar16 .machineBusySignal
        , encodePar16 .machineBusy
        , encodePar16 .ioExpander
        , encodePar16 .coinAcceptor
        , encodePar16 .globalPrice
        , encodePar16 .pulseValue
        , encodePar16 .paymentRequest
        , encodePar16 .priceDigits
        , encodePar16 .priceDecimalDigits
        , encodePar16 .priceVisualization
        , encodePar16 .maxTemperature
        , encodePar16 .temperatureHysteresis
        , encodePar16 .safetyTemperature
        , encodePar16 .termodegratationTemperature
        , encodePar16 .levelType
        , encodePar16 .pulsesPerLiter
        , encodePar16 .levelHysteresisTime
        , encodePar16 .cmMaxLevel
        , encodePar16 .cmMinDrain
        , encodePar16 .cmMinHeating
        , encodePar16 .maxLiters
        , encodePar16 .surfaceLevel
        , encodePar16 .ltMinHeating
        , encodePar16 .drainMinTime
        , encodePar16 .serviceDrainTime
        , encodePar16 .drainPulseTime
        , encodePar16 .inverterType
        , encodePar16 .minimumWashSpeed
        , encodePar16 .maximumWashSpeed
        , encodePar16 .serviceSpeed
        , encodePar16 .preparationRotationEnable
        , encodePar16 .preparationRotationTime
        , encodePar16 .preparationPauseTime
        , encodePar16 .preparationMinimumSpeed
        , encodePar16 .preparationMaximumSpeed
        , encodePar16 .centrifuge1MinimumSpeed
        , encodePar16 .centrifuge1MaximumSpeed
        , encodePar16 .centrifuge2MinimumSpeed
        , encodePar16 .centrifuge2MaximumSpeed
        , encodePar16 .centrifuge3MinimumSpeed
        , encodePar16 .centrifuge3MaximumSpeed
        , encodePar16 .rampMinimumTime
        , encodePar16 .rampMaximumTime
        , encodePar16 .maxLurchAttempts
        , encodePar16 .proximity
        , encodePar16 .minimumBrakeTime
        , encodePar16 .beams
        , encodePar16 .accelerometer
        , encodePar16 .accelerometerScale
        , encodePar16 .accelerometerXThresh
        , encodePar16 .accelerometerYThresh
        , encodePar16 .accelerometerZThresh
        , encodePar16 .accelerometerXThreshH
        , encodePar16 .accelerometerYThreshH
        , encodePar16 .accelerometerZThreshH
        , encodePar16 .accelerometerRounds
        , encodePar16 .accelerometerRounds2
        , encodePar16 .accelerometerDelta
        , encodePar16 .accelerometerWaitTime
        , encodePar16 .accelerometerDrainTime
        , encodePar16 .detergents
        , encodePar16 .detergentCleaningTime
        , encodePar16 .preloadTime
        , encodePar16 .detergentLoadTime
        , encodePar16 .minSecEnable
        , encodePar16 .coinAcceptorUnlocking
        , encodePar16 .alarmInhibition
        , encodePar16 .autoStart
        , encodePar16 .continousRun
        , encodePar16 .dateTime
        , encodePar16 .showDetergentExclusion
        , encodePar16 .pedantic
        , encodePar8 .rgbStop
        , encodePar8 .rgbWork
        , encodePar8 .rgbPause
        , encodePar8 .rgbWaiting
        , encodePar8 .rgbWarning
        , encodePar8 .rgbAlarm
        , encodePar16 .excludedDetergent
        , encodePar32 .globalPriceValue
        , encodePar16 .frontType
        , encodePar16 .lockType
        , encodePar16 .lockPulse
        ]
        |> Encode.encode



-- Wash Cycles


type alias WashingStep =
    { stepType : Int
    , duration : Int
    , activeTime : Int
    , fillingSpeed : Int
    , fillingMotion : Int
    , fillingPause : Int
    , washMotion : Int
    , washPause : Int
    , temperature : Int
    , level : Int
    , soapActiveTime : Int
    , washSpeed : Int
    , uselessPar0 : Int
    , uselessPar1 : Int
    , preparationSpeed : Int
    , preparationTime : Int
    , drainTime : Int
    , ramps : Int
    , centrifuge1Speed : Int
    , centrifuge2Speed : Int
    , centrifuge3Speed : Int
    , ramp1Time : Int
    , ramp2Time : Int
    , ramp3Time : Int
    , centrifuge1WaitTime : Int
    , centrifuge2WaitTime : Int
    , brakeTime : Int
    , detergent1Time : Int
    , detergent2Time : Int
    , detergent3Time : Int
    , detergent4Time : Int
    , detergent5Time : Int
    , detergent6Time : Int
    , detergent7Time : Int
    , detergent8Time : Int
    , detergent9Time : Int
    , detergent10Time : Int
    , detergent1Delay : Int
    , detergent2Delay : Int
    , detergent3Delay : Int
    , detergent4Delay : Int
    , detergent5Delay : Int
    , detergent6Delay : Int
    , detergent7Delay : Int
    , detergent8Delay : Int
    , detergent9Delay : Int
    , detergent10Delay : Int
    , waitTime : Int
    , heatingType : Int
    , continousTemperatureControl : Int
    , heating : Int
    , washInversion : Int
    , fillingInversion : Int
    , movingWhileFilling : Int
    , movingWhileWashing : Int
    , recycling : Int
    , coldWater : Int
    , warmWater : Int
    , purifiedWater : Int
    , recovery : Int
    , finalParameter : Int
    }


type alias WashingCycle =
    { name : Translation
    , price : Int
    , washType : Int
    , steps : Array WashingStep
    }


swapWashSteps : Int -> Int -> WashingCycle -> WashingCycle
swapWashSteps oldIndex newIndex cycle =
    let
        ( newSteps, _ ) =
            swapElements oldIndex newIndex cycle.steps
    in
    { cycle | steps = newSteps }


newWashingCycle : Int -> WashingCycle
newWashingCycle index =
    WashingCycle { italiano = "Nuovo programma " ++ String.fromInt index, english = "New program " ++ String.fromInt index } 0 0 Array.empty


changeWashCycleName : String -> Language -> WashingCycle -> WashingCycle
changeWashCycleName name lang cycle =
    { cycle | name = setTranslation cycle.name lang (String.slice 0 32 name) }


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


washStepStrings : Context -> List String
washStepStrings context =
    [ Intl.Ammollo
    , Intl.Prelavaggio
    , Intl.Lavaggio
    , Intl.Risciacquo
    , Intl.Scarico
    , Intl.Centrifuga
    , Intl.Srotolamento
    , Intl.AttesaOperatore
    ]
        |> List.map (\s -> translate s context)


washStepTypeToString : Context -> Int -> String
washStepTypeToString context stepType =
    washStepStrings context
        |> Array.fromList
        |> Array.get (stepType - 1)
        |> Maybe.withDefault ""


defaultWashStep : Int -> Bool -> WashingStep
defaultWashStep stepType energetic =
    let
        de d e =
            if energetic then
                e

            else
                d

        empty =
            { stepType = stepType, duration = 0, activeTime = 0, fillingSpeed = 0, fillingMotion = 0, fillingPause = 0, washMotion = 0, washPause = 0, temperature = 0, level = 0, soapActiveTime = 0, washSpeed = 0, uselessPar0 = 0, uselessPar1 = 0, preparationSpeed = 0, preparationTime = 0, drainTime = 0, ramps = 0, centrifuge1Speed = 0, centrifuge2Speed = 0, centrifuge3Speed = 0, ramp1Time = 0, ramp2Time = 0, ramp3Time = 0, centrifuge1WaitTime = 0, centrifuge2WaitTime = 0, brakeTime = 0, detergent1Time = 0, detergent2Time = 0, detergent3Time = 0, detergent4Time = 0, detergent5Time = 0, detergent6Time = 0, detergent7Time = 0, detergent8Time = 0, detergent9Time = 0, detergent10Time = 0, detergent1Delay = 0, detergent2Delay = 0, detergent3Delay = 0, detergent4Delay = 0, detergent5Delay = 0, detergent6Delay = 0, detergent7Delay = 0, detergent8Delay = 0, detergent9Delay = 0, detergent10Delay = 0, waitTime = 0, heatingType = 0, continousTemperatureControl = 0, heating = 0, washInversion = 0, fillingInversion = 0, movingWhileFilling = 0, movingWhileWashing = 0, recycling = 0, coldWater = 0, warmWater = 0, purifiedWater = 0, recovery = 0, finalParameter = 0 }

        soakingPrewash =
            { empty | duration = 180, activeTime = 3, fillingSpeed = de 25 45, fillingMotion = de 8 26, fillingPause = de 10 5, washMotion = de 8 26, washPause = de 10 5, temperature = de 30 40, level = 8, soapActiveTime = 1, washSpeed = de 25 45, continousTemperatureControl = 0, heating = 1, washInversion = 1, fillingInversion = 1, movingWhileFilling = de 0 1, recycling = 0, coldWater = 1, warmWater = 0, purifiedWater = 0 }
    in
    case stepType of
        1 ->
            soakingPrewash

        2 ->
            soakingPrewash

        3 ->
            { soakingPrewash | duration = 600, temperature = 30 }

        4 ->
            { empty | duration = 240, activeTime = 1, movingWhileFilling = 0, fillingSpeed = 25, fillingInversion = 1, fillingMotion = 8, fillingPause = 10, washSpeed = 25, washInversion = 1, washMotion = 8, washPause = 10, level = 12, recycling = 0, coldWater = 1, purifiedWater = 0, soapActiveTime = 1 }

        5 ->
            { empty | duration = 30, movingWhileWashing = de 0 1, washSpeed = de 25 45, washInversion = 1, washMotion = de 8 10, washPause = de 10 5 }

        6 ->
            { empty | duration = 60, preparationTime = 20, preparationSpeed = 60, drainTime = 0, recovery = 0, ramps = de 1 3, centrifuge1Speed = 250, ramp1Time = 40, centrifuge1WaitTime = 30, centrifuge2Speed = 400, ramp2Time = 50, centrifuge2WaitTime = 30, centrifuge3Speed = de 500 600, ramp3Time = 60, brakeTime = 75 }

        7 ->
            { empty | duration = 45, washSpeed = 45, washMotion = 5, washPause = 5 }

        8 ->
            { empty | washSpeed = de 25 45, washInversion = 1, washMotion = de 8 10, washPause = de 10 5 }

        _ ->
            empty


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
    Decode.map4 WashingCycle
        translationDecoder
        (Decode.unsignedInt32 Bytes.BE)
        (Decode.unsignedInt16 Bytes.BE)
        (Decode.map Array.fromList <|
            Decode.andThen
                (\s -> Decode.list s washStepDecoder)
                (Decode.unsignedInt16 Bytes.BE)
        )


washStepDecoder : Decoder WashingStep
washStepDecoder =
    Decode.succeed WashingStep
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.unsignedInt16 BE)
        |> Decode.andMap (Decode.withOffset 134 <| Decode.unsignedInt16 BE)


washStepEncoder : WashingStep -> Encode.Encoder
washStepEncoder step =
    let
        encodePar lens =
            Encode.unsignedInt16 BE (lens step)
    in
    Encode.sequence
        ([ encodePar .stepType
         , encodePar .duration
         , encodePar .activeTime
         , encodePar .fillingSpeed
         , encodePar .fillingMotion
         , encodePar .fillingPause
         , encodePar .washMotion
         , encodePar .washPause
         , encodePar .temperature
         , encodePar .level
         , encodePar .soapActiveTime
         , encodePar .washSpeed
         , encodePar .uselessPar0
         , encodePar .uselessPar1
         , encodePar .preparationSpeed
         , encodePar .preparationTime
         , encodePar .drainTime
         , encodePar .ramps
         , encodePar .centrifuge1Speed
         , encodePar .centrifuge2Speed
         , encodePar .centrifuge3Speed
         , encodePar .ramp1Time
         , encodePar .ramp2Time
         , encodePar .ramp3Time
         , encodePar .centrifuge1WaitTime
         , encodePar .centrifuge2WaitTime
         , encodePar .brakeTime
         , encodePar .detergent1Time
         , encodePar .detergent2Time
         , encodePar .detergent3Time
         , encodePar .detergent4Time
         , encodePar .detergent5Time
         , encodePar .detergent6Time
         , encodePar .detergent7Time
         , encodePar .detergent8Time
         , encodePar .detergent9Time
         , encodePar .detergent10Time
         , encodePar .detergent1Delay
         , encodePar .detergent2Delay
         , encodePar .detergent3Delay
         , encodePar .detergent4Delay
         , encodePar .detergent5Delay
         , encodePar .detergent6Delay
         , encodePar .detergent7Delay
         , encodePar .detergent8Delay
         , encodePar .detergent9Delay
         , encodePar .detergent10Delay
         , encodePar .waitTime
         , encodePar .heatingType
         , encodePar .continousTemperatureControl
         , encodePar .heating
         , encodePar .washInversion
         , encodePar .fillingInversion
         , encodePar .movingWhileFilling
         , encodePar .movingWhileWashing
         , encodePar .recycling
         , encodePar .coldWater
         , encodePar .warmWater
         , encodePar .purifiedWater
         , encodePar .recovery
         ]
            ++ List.map Encode.unsignedInt8 (List.repeat 136 0)
        )


encodeWashCycle : WashingCycle -> Bytes
encodeWashCycle cycle =
    let
        names =
            [ cycle.name.italiano, cycle.name.english ]
                ++ List.repeat 10 ""
                |> List.take 10
    in
    Encode.sequence
        (List.map limitedStringEncoder names
            ++ [ Encode.unsignedInt32 Bytes.BE cycle.price
               , Encode.unsignedInt16 Bytes.BE cycle.washType
               , Encode.unsignedInt16 Bytes.BE (Array.length cycle.steps)
               ]
            ++ List.map washStepEncoder (Array.toList cycle.steps)
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
        getMachineParameters list =
            let
                parmacFile : Maybe Bytes
                parmacFile =
                    binaryFileContents parmacFileName list

                padOp : Bytes -> Bytes
                padOp bytes =
                    let
                        length =
                            Bytes.width bytes

                        missing =
                            if length < parmacSize then
                                parmacSize - length

                            else
                                0
                    in
                    Bytes.fromByteValues <| (Bytes.toByteValues bytes ++ List.repeat missing 0)
            in
            parmacFile
                |> Maybe.map padOp
                |> Maybe.andThen (Decode.decode machineParametersDecoder)

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



--pipeline : Decoder a -> Decoder (a -> b) -> Decoder b
--pipeline = Decode.map2 (|>)


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


parmacSize : Int
parmacSize =
    279


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


swapElements : Int -> Int -> Array a -> ( Array a, Int )
swapElements oldIndex newIndex array =
    Maybe.map2 (\old new -> Array.set newIndex old array |> Array.set oldIndex new)
        (Array.get oldIndex array)
        (Array.get newIndex array)
        |> Maybe.map (\na -> ( na, newIndex ))
        |> Maybe.withDefault ( array, oldIndex )

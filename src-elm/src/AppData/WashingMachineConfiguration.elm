module AppData.WashingMachineConfiguration exposing (..)

import Array
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)
import Context exposing (Context, translate)
import Flate
import Inflate
import Tar exposing (defaultMetadata)


type alias MachineParameters =
    { name : String
    }


type alias MachineConfiguration =
    { parmac : MachineParameters
    }


default : Context -> MachineConfiguration
default context =
    MachineParameters (translate "nuova_configurazione" context)
        |> MachineConfiguration


machineParametersDecoder : Decoder MachineParameters
machineParametersDecoder =
    Decode.succeed MachineParameters
        |> pipeline (Decode.string 33)


encodeMachineParameters : MachineParameters -> Bytes
encodeMachineParameters pars =
    let
        finalSize =
            279
    in
    Encode.sequence
        ((Encode.string <| String.padRight 33 (Char.fromCode 0) pars.name)
            :: List.map Encode.unsignedInt8 (List.repeat (finalSize - 33) 0)
        )
        |> Encode.encode


extractArchive : Bytes -> Maybe MachineConfiguration
extractArchive b =
    let
        getFileContents : String -> List ( Tar.Metadata, Tar.Data ) -> Maybe Bytes
        getFileContents name =
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

        fileListToMachineParameters : List ( Tar.Metadata, Tar.Data ) -> Maybe MachineParameters
        fileListToMachineParameters =
            getFileContents parmacFileName >> Maybe.andThen (Decode.decode machineParametersDecoder)

        _ =
            Debug.log "tar" (Inflate.inflateGZip b |> Maybe.map Tar.extractArchive)
    in
    (Inflate.inflateGZip
        >> Maybe.map Tar.extractArchive
        >> Maybe.andThen fileListToMachineParameters
        >> Maybe.map MachineConfiguration
    )
        b


createArchive : MachineConfiguration -> Bytes
createArchive { parmac } =
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

        stringMetadata string name =
            ( { defaultMetadata
                | filename = name
                , mode = Tar.defaultMode
                , ownerID = -16
                , groupID = -16
                , fileSize = String.length string
                , lastModificationTime = 0
                , linkedFileName = ""
                , userName = ""
                , groupName = ""
                , fileNamePrefix = ""
              }
            , Tar.StringData string
            )
    in
    [ binaryMetadata (encodeMachineParameters parmac) parmacFileName
    , stringMetadata dataVersion versionFileName
    ]
        |> Tar.createArchive
        |> Flate.deflateGZip


pipeline : Decoder a -> Decoder (a -> b) -> Decoder b
pipeline =
    Decode.map2 (|>)


parmacFileName : String
parmacFileName =
    "parametri/parmac.bin"


versionFileName : String
versionFileName =
    "version.txt"


dataVersion : String
dataVersion =
    "3"

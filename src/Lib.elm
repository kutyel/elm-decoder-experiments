module Lib exposing
    ( FailureReason(..)
    , TransactionResult(..)
    , transactionDecoder
    )

import Json.Decode as Decode exposing (Decoder)


type TransactionResult
    = AuthCompleted
    | AuthFailed FailureReason
    | AuthFailedAndMaxAttemptsReached FailureReason


type FailureReason
    = Unsuccessful
    | DataMismatch


failureReasonDecoder : Decoder FailureReason
failureReasonDecoder =
    Decode.field "reason"
        (Decode.string
            |> Decode.andThen
                (\reason ->
                    case reason of
                        "unsuccessful" ->
                            Decode.succeed Unsuccessful

                        "data_mismatch" ->
                            Decode.succeed DataMismatch

                        _ ->
                            Decode.fail <| "Unknown transaction failure reason received: " ++ reason
                )
        )


transactionDecoder : Decoder TransactionResult
transactionDecoder =
    Decode.andThen
        (\result ->
            case result of
                "completed" ->
                    Decode.succeed AuthCompleted

                "failed" ->
                    Decode.map AuthFailed failureReasonDecoder

                "failed_and_max_attempts_reached" ->
                    Decode.map AuthFailedAndMaxAttemptsReached failureReasonDecoder

                _ ->
                    Decode.fail <| "Unknown transaction result received: " ++ result
        )
        (Decode.field "result" Decode.string)

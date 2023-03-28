module Tests exposing (all)

import Expect
import Json.Decode exposing (decodeString)
import Lib exposing (FailureReason(..), TransactionResult(..), transactionDecoder)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "TransactionResult Decoder"
        [ test "Just `result` is found" <|
            \_ ->
                let
                    json =
                        """
                    {
                    "result": "completed"
                    }
                """
                in
                decodeString transactionDecoder json
                    |> Expect.equal (Ok AuthCompleted)
        , test "Failed with reason" <|
            \_ ->
                let
                    json =
                        """
                    {
                    "result": "failed",
                    "reason": "unsuccessful"
                    }
                """
                in
                decodeString transactionDecoder json
                    |> Expect.equal (Ok <| AuthFailed Unsuccessful)
        , test "Failed with data mismatch" <|
            \_ ->
                let
                    json =
                        """
                    {
                    "result": "failed_and_max_attempts_reached",
                    "reason": "data_mismatch"
                    }
                """
                in
                decodeString transactionDecoder json
                    |> Expect.equal (Ok <| AuthFailedAndMaxAttemptsReached DataMismatch)
        ]

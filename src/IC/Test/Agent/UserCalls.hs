{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds #-}

module IC.Test.Agent.UserCalls
    (
      ic_canister_status'',
      ic_delete_canister'',
      ic_deposit_cycles'',
      ic_ecdsa_public_key'',
      ic_http_get_request'',
      ic_install'',
      ic_raw_rand'',
      ic_set_controllers'',
      ic_sign_with_ecdsa'',
      ic_start_canister'',
      ic_stop_canister'',
      ic_top_up''',
      ic_uninstall'',
    ) where

import qualified Data.Vector as Vec
import qualified Data.Text as T
import Numeric.Natural
import Test.Tasty.HUnit
import Codec.Candid (Principal(..))
import Data.Row

import IC.Management
import IC.Id.Forms
import IC.Test.Agent
import IC.Test.Agent.Calls

ic_install'' :: (HasCallStack, HasAgentConfig) => Blob -> InstallMode -> Blob -> Blob -> Blob -> IO (HTTPErrOr ReqResponse)
ic_install'' user mode canister_id wasm_module arg =
  callIC'' user canister_id #install_code $ empty
    .+ #mode .== mode
    .+ #canister_id .== Principal canister_id
    .+ #wasm_module .== wasm_module
    .+ #arg .== arg

ic_uninstall'' :: HasAgentConfig => Blob -> Blob -> IO (HTTPErrOr ReqResponse)
ic_uninstall'' user canister_id =
  callIC'' user canister_id #uninstall_code $ empty
    .+ #canister_id .== Principal canister_id

ic_set_controllers'' :: HasAgentConfig => Blob -> Blob -> [Blob] -> IO (HTTPErrOr ReqResponse)
ic_set_controllers'' user canister_id new_controllers = do
  callIC'' user canister_id #update_settings $ empty
    .+ #canister_id .== Principal canister_id
    .+ #settings .== fromPartialSettings (#controllers .== Vec.fromList (map Principal new_controllers))

ic_start_canister'' :: HasAgentConfig => Blob -> Blob -> IO (HTTPErrOr ReqResponse)
ic_start_canister'' user canister_id = do
  callIC'' user canister_id #start_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_stop_canister'' :: HasAgentConfig => Blob -> Blob -> IO (HTTPErrOr ReqResponse)
ic_stop_canister'' user canister_id = do
  callIC'' user canister_id #stop_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_canister_status'' :: HasAgentConfig => Blob -> Blob -> IO (HTTPErrOr ReqResponse)
ic_canister_status'' user canister_id = do
  callIC'' user canister_id #canister_status $ empty
    .+ #canister_id .== Principal canister_id

ic_delete_canister'' :: HasAgentConfig => Blob -> Blob -> IO (HTTPErrOr ReqResponse)
ic_delete_canister'' user canister_id = do
  callIC'' user canister_id #delete_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_deposit_cycles'' :: HasAgentConfig => Blob -> Blob -> IO (HTTPErrOr ReqResponse)
ic_deposit_cycles'' user canister_id = do
  callIC'' user canister_id #deposit_cycles $ empty
    .+ #canister_id .== Principal canister_id

ic_raw_rand'' :: HasAgentConfig => Blob -> Blob -> IO (HTTPErrOr ReqResponse)
ic_raw_rand'' user ecid = do
  callIC'' user ecid #raw_rand ()

ic_http_get_request'' :: HasAgentConfig => Blob -> IO (HTTPErrOr ReqResponse)
ic_http_get_request'' user =
  callIC'' user "" #http_request $ empty
    .+ #url .== (T.pack $ "https://" ++ httpbin)
    .+ #max_response_bytes .== Nothing
    .+ #method .== enum #get
    .+ #headers .== Vec.empty
    .+ #body .== Nothing
    .+ #transform .== Nothing

ic_ecdsa_public_key'' :: HasAgentConfig => Blob -> Blob -> IO (HTTPErrOr ReqResponse)
ic_ecdsa_public_key'' user ecid =
  callIC'' user ecid #ecdsa_public_key $ empty
    .+ #derivation_path .== Vec.empty
    .+ #canister_id .== Nothing
    .+ #key_id .== (empty
       .+ #curve .== enum #secp256k1
       .+ #name .== (T.pack "0")
    )

ic_sign_with_ecdsa'' :: HasAgentConfig => Blob -> Blob -> Blob -> IO (HTTPErrOr ReqResponse)
ic_sign_with_ecdsa'' user ecid msg =
  callIC'' user ecid #sign_with_ecdsa $ empty
    .+ #derivation_path .== Vec.empty
    .+ #message_hash .== msg
    .+ #key_id .== (empty
       .+ #curve .== enum #secp256k1
       .+ #name .== (T.pack "0")
    )

ic_top_up''' :: HasAgentConfig => IC00' -> Blob -> Natural -> IO (HTTPErrOr ReqResponse)
ic_top_up''' ic00' canister_id amount = do
  callIC''' ic00' canister_id #provisional_top_up_canister $ empty
    .+ #canister_id .== Principal canister_id
    .+ #amount .== amount

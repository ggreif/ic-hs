{-# LANGUAGE ScopedTypeVariables #-}
{-|

This module provides a thin wrapper around the winter Wasm engine, exposing just
the bits needed by the IC ref.

This is the interface at which one might plug in a different Wasm engine.
-}
module IC.Wasm.Winter
  ( Module
  , parseModule
  , exportedFunctions
  , ModuleInstance
  , W.Value(..)
  , WS.FuncType(..)
  , W.HostItem(..)
  , W.Store
  , WS.ValueType(..)
  , Import
  , getBytes
  , setBytes
  , initialize
  , invokeExport
  , invokeTable
  )
where

import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as T
import Data.MemoUgly

{-
import qualified Wasm.Binary.Decode as W
import qualified Wasm.Exec.Eval as W
import qualified Wasm.Runtime.Func as W
import qualified Wasm.Runtime.Instance as W
import qualified Wasm.Runtime.Memory as W
import qualified Wasm.Syntax.AST as W
import qualified Wasm.Syntax.Types as W
import qualified Wasm.Syntax.Values as W
import qualified Wasm.Syntax.Memory as W
import qualified Wasm.Util.Source as W
-}

import qualified Language.Wasm.Binary as W
import qualified Language.Wasm.Interpreter as W
import qualified Language.Wasm.Structure as WS
import qualified Language.Wasm.Validate as W

{-
type Instance s = (IM.IntMap (W.ModuleInst W.Phrase (ST s)), Int)

type HostM s = ExceptT String (ST s)

type HostFunc s = HostM s [W.Value]

type ModName = String
type Import s = (ModName, FuncName, W.StackType, W.StackType, [W.Value] -> HostFunc s)
type Imports s = [Import s]
-}

type Address = Int
type Module = WS.Module
type ModuleInstance = W.ModuleInstance
type Imports = W.Imports
type Store = W.Store
type FuncName = String
type Import = (T.Text, W.HostItem)
-- This function is memoized using Data.MemoUgly. This optimizes for workloads
-- where the same module is parsed many times (ic-ref-test). It is wasteful when
-- a module is parsed, eventually dropped (i.e. canister deleted), and never installed
-- again.
parseModule :: BS.ByteString -> Either String Module
parseModule = memo W.decodeModule

initialize :: Module -> Imports -> IO (Store, ModuleInstance)
initialize mod imps = do
  valid_mod <- case W.validate mod of Right w -> return w
                                      Left e -> error $ show e
  (res, st) <- W.instantiate W.emptyStore imps valid_mod
  case res of Right w -> return (st, w)
              Left e -> error $ show e
  {-
  let by_mod :: [(T.Text, [(T.Text, W.StackType, W.StackType, [W.Value] -> HostFunc s)])]
      by_mod = M.toList $ M.fromListWith (<>)
        [ (T.pack m, [(T.pack n,t1,t2,f)]) | (m,n,t1,t2,f) <- imps ]

      names :: M.Map T.Text Int
      names = M.fromList (zip (map fst by_mod) [1..])

      mods :: IM.IntMap (W.ModuleInst W.Phrase (ST s))
      mods  = IM.fromList $ zip [1..]
        [ (W.emptyModuleInst def)
          { W._miGlobals  = mempty
          , W._miTables   = mempty
          , W._miMemories = mempty
          , W._miFuncs    = mempty
          , W._miExports  = M.fromList
            [ (,) fname $ W.ExternFunc $
              W.allocHostEff (W.FuncType arg_ty ret_ty)
                  (\ args -> runExceptT $ f args)
            | (fname, arg_ty, ret_ty, f) <- funcs
            ]
          }
        | (_name, funcs) <- by_mod
        ]
  (ref, inst, start_err) <- W.initialize mod names mods
  for_ start_err throwError
  let mods' = IM.insert ref inst mods
  return (mods', ref)
  -}

exportName :: WS.Export -> String
exportName (WS.Export name _) = T.unpack name

exportedFunctions :: Module -> [FuncName]
exportedFunctions wasm_mod = map exportName $ WS.exports wasm_mod

invokeExport :: Store -> ModuleInstance -> FuncName -> [W.Value] -> IO (Maybe [W.Value])
invokeExport st mod method args = W.invokeExport st mod (T.pack method) args

invokeTable :: Store -> ModuleInstance -> Address -> [W.Value] -> IO (Maybe [W.Value])
invokeTable st _mod idx args = W.invoke st idx args

getBytes :: Store -> ModuleInstance -> Address -> Int -> IO BS.ByteString
getBytes _st _mod _ptr _len = do
  return BS.empty

setBytes :: Store -> ModuleInstance -> Address -> BS.ByteString -> IO ()
setBytes _st _mod _ptr _blob = do
  return ()


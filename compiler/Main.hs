{-# LANGUAGE PatternGuards #-}
module Main where

import Lambdachine.Utils
import Lambdachine.Ghc.Pipeline
import Lambdachine.Ghc.CoreToBC
--import Lambdachine.Grin.Eval
import Lambdachine.Grin.Bytecode
import Lambdachine.Grin.Analyse
import Lambdachine.Grin.RegAlloc
--import Lambdachine.Interp.Exec
--import Lambdachine.Interp.Trace
import Lambdachine.Serialise
import qualified Lambdachine.Options as Cli

import GHC
import DynFlags ( setPackageName )
import GHC.Paths ( libdir )
import Outputable
import MonadUtils ( liftIO )
import qualified Data.Map as M

import Control.Exception ( onException )
import Control.Monad ( when )
import System.Environment ( getArgs )
import System.Directory ( getTemporaryDirectory, renameFile, removeFile )
import System.IO ( openTempFile, hPutStr, hFlush, hClose )
import System.Cmd ( rawSystem )
import System.FilePath ( replaceExtension )
import System.IO.Temp

main :: IO ()
main = do
  opts <- Cli.getOptions
  runGhc (Just libdir) $ do
    dflags0 <- getSessionDynFlags
    let dflags1 = dflags0{ ghcLink = NoLink }
        dflags2 | Cli.package_name opts /= ""
                = setPackageName (Cli.package_name opts) dflags1
                | otherwise = dflags1
        dflags = dflags2
    setSessionDynFlags dflags
    let file = Cli.inputFile opts
    (this_mod, core_binds, data_tycons, imports)
      <- compileToCore file
    liftIO $ do
      print (moduleNameString this_mod,
            map moduleNameString imports)
      s <- newUniqueSupply 'g'
      when (Cli.dumpCoreBinds opts) $ do
        putStrLn "================================================="
        putStrLn $ showPpr core_binds

      let bcos = generateBytecode s this_mod core_binds data_tycons
      putStrLn $ pretty bcos
      let !bco_mdl =
            allocRegs (moduleNameString this_mod)
                      (map moduleNameString imports)
                      bcos

      when (Cli.dumpBytecode opts) $ do
        pprint $ bco_mdl

      let ofile = file `replaceExtension` ".lcbc"
--      putStrLn $ "Writing output to: " ++ show ofile

      tmpdir <- getTemporaryDirectory
      (tmpfile, hdl) <- openBinaryTempFile tmpdir "lcc.lcbc"
      (`onException` (hClose hdl >> removeFile tmpfile)) $ do
        hWriteModule hdl bco_mdl
        hFlush hdl  -- just to be sure
        hClose hdl
        renameFile tmpfile ofile

{-
--     tmp <- getTemporaryDirectory
--     (file, hdl) <- openTempFile tmp "trace.html"
--     hPutStr hdl (showHtml (defaultWrapper (toHtml (FLoad (TVar 3) 2))))
--     hFlush hdl
--     hClose hdl
--     _ <- rawSystem "open" [file]
--     return ()
-}
--    test_insts2 bcos'
   
    --let entry:_ = filter ((=="test") . show) (M.keys bcos')
    --pprint $ fst $ interp entry bcos'
    --test_record1 bcos'

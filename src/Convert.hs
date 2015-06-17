{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import qualified Problem.DAG.Convertor.Pegasus as CP
import           Problem.DAG.TinyDAG           (encodeDAG)

import qualified Data.ByteString.Lazy.Char8    as BL
import           System.Console.CmdArgs        (Data, Typeable, argPos, cmdArgs,
                                                def, help, name, summary, typ,
                                                typFile, (&=))


data Arg = Arg { orig :: String
               , file :: [FilePath]} deriving (Data, Typeable, Show, Eq)

arg::Arg
arg = Arg { orig = "pegasus" &= name "t" &= typ "Original"
                   &= help "Original Format"
          , file = def &= argPos 0 &= typFile
          } &= summary "Convert workflow file to a simple json format."

convert::Arg->IO ()
convert (Arg _orig _path) = do
  let cvt = case _orig of
        "pegasus" -> CP.parseFile
  jDag <- cvt $ head _path
  BL.putStrLn $ encodeDAG jDag

main = convert =<< cmdArgs arg

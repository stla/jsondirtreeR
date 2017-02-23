-- compilation
-- first: stack install
-- second (ignore the error 'ghc can't apply -o to multiple source files'):
-- Windows:
--   64 bit:
--     stack build --ghc-options "-shared -fPIC StartEnd.c -o Rpackage/inst/libs/x64/JsonDirTree.dll"
--   32 bit:
--     stack install jsondirtree --stack-yaml stack32jsondirtree.yaml
--     stack setup --stack-yaml stack32.yaml
--     stack build --stack-yaml stack32.yaml --ghc-options "-shared -fPIC StartEnd.c -o Rpackage/inst/libs/i386/JsonDirTree.dll"
-- Linux:
--

{-# LANGUAGE ForeignFunctionInterface #-}
-- the two pragmas below are possibly useless (not tested)
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonDirTreeR
  where
import Foreign
import Foreign.C
import JsonDirTree (dirToJSONtree)
import Data.ByteString.Lazy.Internal (unpackChars)

main = putStrLn "hello"

foreign export ccall dirToJSONtreeR :: Ptr CInt -> Ptr CString -> Ptr CString -> IO ()
dirToJSONtreeR :: Ptr CInt -> Ptr CString -> Ptr CString -> IO ()
dirToJSONtreeR depth dir result = do
  depth <- peek depth
  dir <- (>>=) (peek dir) peekCString
  json <- dirToJSONtree
            (if depth<0 then Nothing else Just (fromIntegral depth :: Int))
              dir
  jsonC <- newCString $ unpackChars json
  poke result $ jsonC

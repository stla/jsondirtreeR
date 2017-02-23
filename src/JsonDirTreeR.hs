-- compilation
-- Windows:
--   64 bit:
--     stack setup
--     stack exec -- ghc -c StartEnd.c
--     stack build --ghc-options "-shared -fPIC -o JsonDirTreeR.dll C:\HaskellProjects\jsondirtreeR\StartEnd.o"
--     mv JsonDirTreeR.dll Rpackage/inst/libs/x64/JsonDirTreeR.dll
--     rm JsonDirTreeR.dll.a
--   32 bit:
--     stack clean
--     stack install jsondirtree --stack-yaml stack32jsondirtree.yaml
--     stack setup --stack-yaml stack32.yaml
--     stack exec -- ghc -c StartEnd.c
--     stack build --stack-yaml stack32.yaml --ghc-options "-shared -fPIC -o JsonDirTreeR.dll C:\HaskellProjects\jsondirtreeR\StartEnd.o"
--     mv JsonDirTreeR.dll Rpackage/inst/libs/i386/JsonDirTreeR.dll
--     rm JsonDirTreeR.dll.a
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

foreign export ccall dirToJSONtreeR :: Ptr CInt -> Ptr CString -> Ptr CString -> IO ()
dirToJSONtreeR :: Ptr CInt -> Ptr CString -> Ptr CString -> IO ()
dirToJSONtreeR depth dir result = do
  depth <- peek depth
  let d = fromIntegral depth :: Int
  dir <- (>>=) (peek dir) peekCString
  json <- dirToJSONtree
            (if d<0 then Nothing else Just d)
              dir
  jsonC <- newCString $ unpackChars json
  poke result $ jsonC

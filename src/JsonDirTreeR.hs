-- compilation
-- Windows:
-- NEW (with ghc-options in cabal file)
--   64 bit:
--     stack setup --arch x86_64
--     stack build --arch x86_64
--     mv HSdll.dll Rpackage/inst/libs/x64/JsonDirTreeR.dll
--     rm HSdll.dll.a
--   32 bit:
--     stack clean
--     stack setup --arch i386
--     stack build --arch i386
--     mv HSdll.dll Rpackage/inst/libs/i386/JsonDirTreeR.dll
--     rm HSdll.dll.a
-- OLD
--   64 bit:
--     stack setup --arch x86_64
--     stack exec --arch x86_64 -- ghc -c StartEnd.c
--     stack build --arch x86_64 --ghc-options "-shared -fPIC -o JsonDirTreeR.dll %cd%\StartEnd.o"
--     mv JsonDirTreeR.dll Rpackage/inst/libs/x64/JsonDirTreeR.dll
--     rm JsonDirTreeR.dll.a
--   32 bit:
--     stack clean
--     stack setup --arch i386
--     stack exec --arch i386 -- ghc -c StartEnd.c
--     stack build --arch i386 --ghc-options "-shared -fPIC -o JsonDirTreeR.dll %cd%\StartEnd.o"
--     mv JsonDirTreeR.dll Rpackage/inst/libs/i386/JsonDirTreeR.dll
--     rm JsonDirTreeR.dll.a
-- Linux:
--     stack setup
--     stack exec -- ghc -c StartEnd.c
--     stack build --ghc-options "-shared -fPIC -dynamic -lHSrts-ghc8.0.2 -o Main.so $(pwd)/StartEnd.o"
--     mv Main.so ./Rpackage/inst/libs/JsonDirTreeR.so

{-# LANGUAGE ForeignFunctionInterface #-}

module JsonDirTreeR
  where
import Foreign
import Foreign.C
import JsonDirTree (dirToJSONtree, drawDir)
import Data.ByteString.Lazy.Internal (unpackChars)

foreign export ccall dirToJSON :: Ptr CInt -> Ptr CString -> Ptr CString -> IO ()
dirToJSON :: Ptr CInt -> Ptr CString -> Ptr CString -> IO ()
dirToJSON depth dir result = do
  depth <- peek depth
  let d = fromIntegral depth :: Int
  dir <- (>>=) (peek dir) peekCString
  json <- dirToJSONtree
            (if d<0 then Nothing else Just d)
              dir
  jsonC <- newCString $ unpackChars json
  poke result jsonC

foreign export ccall dirToTree :: Ptr CInt -> Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
dirToTree :: Ptr CInt -> Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
dirToTree depth dir vertical result = do
  depth <- peek depth
  let d = fromIntegral depth :: Int
  dir <- (>>=) (peek dir) peekCString
  vertical <- peek vertical
  let v = vertical > 0
  tree <- drawDir
            (if d<0 then Nothing else Just d)
              dir v
  treeC <- newCString tree
  poke result treeC

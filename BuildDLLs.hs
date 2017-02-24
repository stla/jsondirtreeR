import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import System.Process
import System.Directory
import Data.List


main = defaultMainWithHooks (simpleUserHooks { postBuild = buildDll })
  where
  buildDll _ _ pkg info = do putStrLn "Building Dll..."
                             setCurrentDirectory (buildDir info)
                             let buildCmd = cmd pkg info
                             putStrLn buildCmd
--                             system buildCmd
                             let dll = dllFile pkg
                             let cpDllCmd = "cp " ++ dll ++ " " ++ (name pkg) ++ "\\" ++ dll
                             putStrLn cpDllCmd
--                             system cpDllCmd
  ghcExe :: LocalBuildInfo ->  String
  -- ghcExe info = "\"" ++ (compilerPath (compiler info)) ++ "\""
  ghcExe info = "XXX"
  mainOFile :: PackageDescription ->  String
  mainOFile pd = "HS" ++ (name pd) ++ "-" ++ (show (pkgVersion (package pd))) ++ ".o"
  cmd :: PackageDescription ->  LocalBuildInfo ->  String
  cmd pd i = (ghcExe i) ++ " --mk-dll -o " ++ (dllFile pd) ++ " " ++ (mainOFile pd) ++ " " ++ (packages i)
  packages :: LocalBuildInfo ->  String
  packages i = foldl1 (\x y ->  x ++ " " ++ y) (map (showPackage . snd) (externalPackageDeps i))
  showPackage :: PackageId ->  String
  showPackage pi = "-package " ++ show (packageId pi)
  name :: PackageDescription ->  String
  name = show . pkgName . package
  dllFile :: PackageDescription ->  String
  dllFile pd = (name pd) ++ ".dll"

{ mkDerivation, base, blaze-html, directory, filepath, lib }:
mkDerivation {
  pname = "test2web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base blaze-html directory filepath ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}

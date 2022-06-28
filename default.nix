{ mkDerivation, base, blaze-html, directory, filepath, lib
, unordered-containers
}:
mkDerivation {
  pname = "test2web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-html directory filepath unordered-containers
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}

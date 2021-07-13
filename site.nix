{ mkDerivation, aeson, base, hsass, lens, lens-aeson, lib, shake
, slick, text, time
}:
mkDerivation {
  pname = "site";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base hsass lens lens-aeson shake slick text time
  ];
  description = "A personal site";
  license = lib.licenses.gpl3Only;
}

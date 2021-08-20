{ mkDerivation, aeson, base, data-default, hsass, lens, lens-aeson
, lib, pandoc, shake, slick, text, time
}:
mkDerivation {
  pname = "site";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base data-default hsass lens lens-aeson pandoc shake slick
    text time
  ];
  description = "A personal site";
  license = lib.licenses.gpl3Only;
}

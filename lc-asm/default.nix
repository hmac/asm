let sources = import ./nix/sources.nix { };
in { pkgs ? import sources.nixpkgs { }, ... }:
with pkgs.haskellPackages;
mkDerivation {
  pname = "lc-asm";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    containers
    megaparsec
    transformers
    pretty-simple
    Stream
    http-types
    wai
    warp
    aeson
    prettyprinter
  ];
  buildTools = [ cabal-install hlint brittany ghcid cabal-edit ];
  license = "unknown";
}

{ pkgs ? (import <nixpkgs> { }), ... }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithHoogle (pkgs: [ pkgs.containers pkgs.megaparsec pkgs.transformers ]))
    haskellPackages.hlint
    haskellPackages.ormolu
    haskellPackages.ghcid
    nixfmt
  ];
}

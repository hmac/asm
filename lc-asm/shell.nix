{ pkgs ? (import <nixpkgs> { }), ... }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithHoogle (pkgs: [
      pkgs.containers
      pkgs.megaparsec
      pkgs.transformers
      pkgs.pretty-simple
      pkgs.warp
      pkgs.wai
      pkgs.bytestring
      pkgs.http-types
    ]))
    haskellPackages.hlint
    haskellPackages.brittany
    haskellPackages.ghcid
    nixfmt
  ];
}

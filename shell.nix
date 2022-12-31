{ pkgs ? (import <nixpkgs> { }), ... }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    nasm
    llvm
    # nodejs 16.x is required for copilot
    nodejs-16_x
  ];
}

{pkgs ? (import <nixpkgs> {}), ...}:
  pkgs.mkShell {
    buildInputs = with pkgs; [nasm llvm];
  }

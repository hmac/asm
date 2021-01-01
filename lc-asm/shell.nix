let sources = import ./nix/sources.nix { };
in { pkgs ? import sources.nixpkgs { }, ... }:
(import ./default.nix { inherit pkgs; }).env

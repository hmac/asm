# This defines a NixOS module which runs the app as a systemd service, exposing a HTTP interface
# on the given port.
let sources = import ./nix/sources.nix { };
in { config, lib, pkgs ? import sources.nixpkgs { }, ... }:
let
  cfg = config.services.hmac.lc-asm;
  lc-asm = import ./default.nix { inherit config pkgs; };

in with lib; {
  imports = [ ];
  options = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "If enabled, the lc-asm application will run.";
      port = mkOption {
        type = types.int;
        default = 8001;
        description = "TCP port which the application will listen on.";
      };
      config = {
        systemd.services.lc-asm = mkIf cfg.enable {
          description =
            "Compiles a simple lambda calculus to assembly, over HTTP";
          wantedBy = [ "multi-user.target" ];
          path = [ lc-asm ];
          script = ''
            exec ${lc-asm}/bin/lc-asm web ${cfg.port}
          '';
        };
      };
    };
  };
}

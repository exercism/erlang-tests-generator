{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-20.03";
  inputs.flake-utils.url = "github:numtide/flake-utils";

    outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        erlang = pkgs.beam.interpreters.erlangR22;
        beampkgs = pkgs.beam.packagesWith erlang;
      in rec {
        devShell = pkgs.mkShell {
          buildInputs = [
            beampkgs.rebar3
          ];
        };
      }
    );
}

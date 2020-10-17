{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-20.03";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        version =
          if self ? shortRev
          then "${self.lastModifiedDate}-${self.shortRev}"
          else "${self.lastModifiedDate}-dirty";

        selfpkgs = self.outputs.packages.${system};

        pkgs = nixpkgs.legacyPackages.${system};
        erlang = pkgs.beam.interpreters.erlangR22;
        beampkgs = pkgs.beam.packagesWith erlang;
      in
      rec {
        defaultApp = {
          type = "app";
          program = "${selfpkgs.erlangTestGenerator}/bin/testgen";
        };

        packages.erlangTestGenerator = beampkgs.buildRebar3 rec {
          name = "erlang-test-generator";
          inherit version;

          src = self;

          checkouts = beampkgs.fetchRebar3Deps {
            name = "${name}-deps";
            inherit version;
            src = "${self}/rebar.lock";
            sha256 = "0h3y1d05n1apg8wbdgh5v2mpxla26i3h46qcqj3m8kvcrqp9n180";
          };

          buildInputs = [ ];

          preConfigure = ''
            cp --no-preserve=all -R ${checkouts}/_checkouts _checkouts
          '';

          buildPhase = ''
            HOME=. rebar3 escriptize
          '';

          installPhase = ''
            mkdir -p $out/bin
            install _build/default/bin/testgen $out/bin/testgen
          '';
        };

        devShell = pkgs.mkShell {
          buildInputs = [
            # build tools
            beampkgs.rebar3

            # nix helpers
            pkgs.nixpkgs-fmt
          ];
        };
      }
    );
}

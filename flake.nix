{
  inputs = {
    nixpkgs.follows = "creusot/nixpkgs";
    flake-utils.follows = "creusot/flake-utils";

    creusot.url = "github:creusot-rs/creusot/74d94fef94387d7706abcc53bc490020cd6c75c3";
  };

  outputs = {
    self,
    creusot,
    flake-utils,
    nixpkgs,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};
      pkgsCreusot = creusot.lib.${system}.pkgs;

      version = "0.1.99";
    in {
      packages = {
        code = pkgs.buildNpmPackage {
          inherit version;

          pname = "creusot-ide";
          src = ./.;
          npmDepsHash = "sha256-bkZ5pHTZsa/5vMbRUqazTRVSKhoCnI0LZHyowAwLxEQ=";

          buildInputs = with pkgs; [libsecret];
          nativeBuildInputs = with pkgs; [pkg-config];

          prePatch = ''
            sed -i -e 's/"0.1.1"/"${version}"/g' package.json package-lock.json
          '';

          installPhase = ''
            mkdir $out
            npx vsce package -o $out/creusot-ide.vsix
          '';
        };

        lsp = pkgs.ocamlPackages.buildDunePackage {
          inherit version;

          pname = "creusot-lsp";
          src = ./.;

          nativeBuildInputs = [pkgs.ocamlPackages.menhir];
          buildInputs =
            (with pkgsCreusot; [why3 why3find])
            ++ (with pkgs.ocamlPackages; [
              dune-build-info
              dune-site
              jsonm
              linol-lwt
              logs
              lwt
              menhirLib
              ppx_deriving
              ppx_expect
              ppx_yojson_conv
              terminal_size
              toml
              uri
              xmlm
            ]);
        };
      };

      formatter = pkgs.alejandra;
    });
}

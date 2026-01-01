{
  inputs = {
    nixpkgs.follows = "creusot/nixpkgs";
    flake-utils.follows = "creusot/flake-utils";

    creusot.url = "github:creusot-rs/creusot/021fd93d593addbf4016f326a8f07c37e633df83";
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
    in {
      packages.default = pkgs.ocamlPackages.buildDunePackage {
        pname = "creusot-lsp";
        version = "0.1.0";

        src = ./.;

        nativeBuildInputs = [pkgs.ocamlPackages.menhir];
        buildInputs =
          (with pkgsCreusot; [why3 (why3find.passthru.why3find)])
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

      formatter = pkgs.alejandra;
    });
}

{
  inputs = {
    nixpkgs.follows = "creusot/nixpkgs";
    flake-parts.follows = "creusot/flake-parts";

    creusot.url = "github:creusot-rs/creusot";
  };

  outputs =
    inputs@{
      creusot,
      flake-parts,
      nixpkgs,
      self,
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "x86_64-linux"
      ];

      perSystem =
        {
          pkgs,
          system,
          ...
        }:
        let
          version = pkgs.creusot.creusot.version;
        in
        {
          _module.args.pkgs = import nixpkgs {
            inherit system;
            overlays = [ creusot.overlays.default ];
          };

          formatter = pkgs.nixfmt-tree;

          packages = {
            code = pkgs.buildNpmPackage {
              inherit version;

              pname = "creusot-ide";
              src = ./.;
              npmDepsHash = "sha256-8tB57OR3vBLdYk+cXRMwk/tQSDBurgH4uYwzz/j1Ncs=";

              buildInputs = with pkgs; [ libsecret ];
              nativeBuildInputs = with pkgs; [ pkg-config ];

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

              nativeBuildInputs = [ pkgs.ocamlPackages.menhir ];
              buildInputs =
                (with pkgs.creusot; [
                  why3
                  why3find
                ])
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
        };
    };
}

{
  description = "A flake for tracy Haskell bindings development";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
  let
    forAllSystems = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
  in
  {
    packages = forAllSystems (system: let pkgs = import nixpkgs { inherit system; }; in {
      tracy-profiler = pkgs.haskellPackages.callCabal2nix "tracy-profiler" ./. {
        buildInputs = [ self.packages.${system}.tracy-lib ];
      };
      tracy-lib = pkgs.stdenv.mkDerivation {
        pname = "tracy";
        version = "v0.12.2";

        src = pkgs.fetchFromGitHub {
          owner = "KovalevDima";
          repo = "tracy";
          rev = "denerate-pc-for-tracy";
          hash = "sha256-laqt1ORB2XryU3uuqgdGfDLu1BGK4ekboT8gLjRr7+U=";
        };

        nativeBuildInputs = with pkgs; [ pkg-config meson ninja cmake ];
        buildInputs = with pkgs; [ zlib libunwind ];
      };
    });
    devShells = forAllSystems (system: let pkgs = import nixpkgs { inherit system; }; in {
      default = pkgs.mkShell {
        inputsFrom = with pkgs; [  ];
        packages = with pkgs; [ pkg-config self.packages.${system}.tracy-lib ];
      };
    });
  };
}

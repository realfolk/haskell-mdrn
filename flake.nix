{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=23.05";
    flakeUtils.url = "github:numtide/flake-utils";
    realfolkNix.url = "github:realfolk/nix";

    # Haskell Dependencies
    pouch.url = "github:realfolk/haskell-pouch";
    logger.url = "github:realfolk/haskell-logger";
  };

  outputs =
    { self
    , nixpkgs
    , flakeUtils
    , realfolkNix
    , pouch
    , logger
    , ...
    }:
    flakeUtils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      realfolkNixPkgs = realfolkNix.packages.${system};

      neovim = realfolkNixPkgs.neovim;
      ranger = realfolkNixPkgs.ranger;
      rnixLsp = realfolkNixPkgs.rnixLsp;

      realfolkNixLib = realfolkNix.lib.${system};

      haskellPackages = realfolkNixLib.haskellPackages;

      haskellPkgs = haskellPackages.extend (self: super: {
        pouch = pouch.packages.${system}.default;
        real-folk-logger = logger.packages.${system}.default;
      });

      ghc = haskellPkgs.ghcWithPackages (p: [
        p.pouch
        p.real-folk-logger
      ]);

      haskellProject = realfolkNixLib.haskellProject;
    in
    {
      packages.default = haskellPkgs.callCabal2nix "mdrn" "${self}" { };

      devShells.default = pkgs.mkShell {
        buildInputs = [
          neovim
          pkgs.fzf
          pkgs.inotifyTools
          pkgs.openssl
          pkgs.silver-searcher
          pkgs.zlib
          ranger
          rnixLsp

          # Haskell
          ghc
          haskellPkgs.cabal-install
          haskellPkgs.haskell-language-server
          haskellPkgs.hspec-discover
        ];

        shellHook = pkgs.lib.concatStrings [
          (
            ''
              # Load ~/.bashrc if it exists
              test -f ~/.bashrc && source ~/.bashrc

              # Initialize $PROJECT environment variable
              export PROJECT="$PWD"

              # Source .env file if present
              test -f "$PROJECT/.env" && source .env

              # Ignore files specified in .gitignore when using fzf
              # -t only searches text files and includes empty files
              export FZF_DEFAULT_COMMAND="ag -tl"
            ''
          )
          (haskellProject.shellHook ghc)
        ];
      };
    });
}

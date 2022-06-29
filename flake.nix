{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flakeUtils.url = "github:numtide/flake-utils";
    neovim.url = "github:realfolk/nix?dir=lib/packages/neovim";
    ranger.url = "github:realfolk/nix?dir=lib/packages/ranger";
    rnixLsp.url = "github:nix-community/rnix-lsp";
    haskellPackages.url = "github:realfolk/nix?dir=lib/projects/haskell/packages/ghc-9.2";
    haskellProject.url = "github:realfolk/nix?dir=lib/projects/haskell";
    commonProject.url = "github:realfolk/nix?dir=lib/projects/common";
    projectLib.url = "github:realfolk/nix?dir=lib/projects/lib";
  };

  outputs =
    { self
    , nixpkgs
    , flakeUtils
    , neovim
    , ranger
    , rnixLsp
    , haskellPackages
    , haskellProject
    , commonProject
    , projectLib
    , ...
    }:
    flakeUtils.lib.eachDefaultSystem (system:
    let
      # HELPERS

      config = {
        srcDir = "$PROJECT/src";
        buildDir = "$PROJECT/.build";
        buildArtifactsDir = "$PROJECT/.build.artifacts";
      };

      haskellDependencies = p: [
        p.aeson
        p.async
        p.base58-bytestring
        p.base64-bytestring
        p.basement
        p.bcrypt
        p.criterion
        p.cryptonite
        p.dotenv
        p.envy
        p.haddock
        p.hspec
        p.http-client
        p.http-client-tls
        p.http-types
        p.HUnit
        p.lmdb
        p.microlens
        p.microstache
        p.mime-types
        p.network-uri
        p.optparse-applicative
        p.shakespeare
        p.smtp-mail
        p.strict-concurrency
        p.tasty
        p.tasty-hunit
        p.tasty-hunit-adapter
        p.tasty-quickcheck
        p.tasty-smallcheck
        p.temporary
        p.text-trie
        p.tls
        p.tls-debug
        p.turtle
        p.uuid
        p.wai
        p.warp
        p.warp-tls
      ];

      defineProject = args:
        projectLib.lib.defineProject (config // args);

      defineHaskellProject = args:
        haskellProject.lib.defineProject (config // { inherit haskellDependencies; } // args);

      pkgs = nixpkgs.legacyPackages.${system};

      haskellPkgs = haskellPackages.packages.${system};

      ghc = haskellPkgs.ghcWithPackages haskellDependencies;

      # PROJECTS

      mdrnLibDefinition = {
        groupName = "mdrn";
        projectName = "lib";
        localDependencies = map defineHaskellProject [
          #TODO
        ];
      };

      mdrnLibHaskell = haskellProject.lib.make {
        inherit system;
        haskellPackages = haskellPkgs;
        project = defineHaskellProject mdrnLibDefinition;
      };

      mdrnLibCommon = commonProject.lib.make {
        inherit system;
        project = defineProject mdrnLibDefinition;
      };

      mdrnREPLDefinition = {
        groupName = "mdrn";
        projectName = "repl";
        localDependencies = map defineHaskellProject [
          #TODO
          mdrnLibDefinition
        ];
        executables = {
          list-parser = "ListParser.hs";
        };
      };

      mdrnREPLHaskell = haskellProject.lib.make {
        inherit system;
        haskellPackages = haskellPkgs;
        project = defineHaskellProject mdrnREPLDefinition;
      };

      mdrnREPLCommon = commonProject.lib.make {
        inherit system;
        project = defineProject mdrnREPLDefinition;
      };

      mdrnBenchmarksDefinition = {
        groupName = "mdrn";
        projectName = "benchmarks";
        localDependencies = map defineHaskellProject [
          #TODO
          mdrnLibDefinition
        ];
        executables = {
          list-parser = "ListParser.hs";
        };
      };

      mdrnBenchmarksHaskell = haskellProject.lib.make {
        inherit system;
        haskellPackages = haskellPkgs;
        project = defineHaskellProject mdrnBenchmarksDefinition;
      };

      mdrnBenchmarksCommon = commonProject.lib.make {
        inherit system;
        project = defineProject mdrnBenchmarksDefinition;
      };

      mdrnTestsDefinition = {
        groupName = "mdrn";
        projectName = "tests";
        localDependencies = map defineHaskellProject [
          #TODO
          mdrnLibDefinition
        ];
        executables = {
          test = "Spec.hs";
        };
      };

      mdrnTestsHaskell = haskellProject.lib.make {
        inherit system;
        haskellPackages = haskellPkgs;
        project = defineHaskellProject mdrnTestsDefinition;
      };

      mdrnTestsCommon = commonProject.lib.make {
        inherit system;
        project = defineProject mdrnTestsDefinition;
      };
    in
    {
      packages = {
        inherit ghc;
        neovim = neovim.packages.${system}.default;
        ranger = ranger.packages.${system}.default;
        rnixLsp = rnixLsp.defaultPackage.${system};
        haskellLanguageServer = haskellPkgs.haskell-language-server;
        hspecDiscover = haskellPkgs.hspec-discover;
      };

      devShells.default = pkgs.mkShell {
        buildInputs = builtins.concatLists [
          (builtins.attrValues self.packages.${system})
          [
            pkgs.silver-searcher # ag
            pkgs.fzf
            pkgs.openssl
            pkgs.inotifyTools
            # Projects
            mdrnLibHaskell.combinedCommandsPackage
            mdrnLibCommon.combinedCommandsPackage
            mdrnREPLHaskell.combinedCommandsPackage
            mdrnREPLCommon.combinedCommandsPackage
            mdrnBenchmarksHaskell.combinedCommandsPackage
            mdrnBenchmarksCommon.combinedCommandsPackage
            mdrnTestsHaskell.combinedCommandsPackage
            mdrnTestsCommon.combinedCommandsPackage
          ]
        ];
        shellHook = pkgs.lib.concatStrings [
          (
            ''
              # Load ~/.bashrc if it exists
              test -f ~/.bashrc && source ~/.bashrc

              # Source .env file if present
              test -f "$PROJECT/.env" && source .env

              # Ignore files specified in .gitignore when using fzf
              # -t only searches text files and includes empty files
              export FZF_DEFAULT_COMMAND="ag -tl"

              # Initialize $PROJECT environment variable
              export PROJECT="$PWD"

              # Create project src directories
              ${mdrnLibCommon.commands.mkdirSrc.bin}
              ${mdrnREPLCommon.commands.mkdirSrc.bin}
              ${mdrnBenchmarksCommon.commands.mkdirSrc.bin}
              ${mdrnTestsCommon.commands.mkdirSrc.bin}

              # Create hie.yaml files
              ${mdrnLibHaskell.commands.hieYaml.bin}
              ${mdrnREPLHaskell.commands.hieYaml.bin}
              ${mdrnBenchmarksHaskell.commands.hieYaml.bin}
              ${mdrnTestsHaskell.commands.hieYaml.bin}
            ''
          )
          (haskellProject.lib.shellHook ghc)
        ];
      };
    });
}

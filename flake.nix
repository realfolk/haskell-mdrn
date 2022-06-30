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
    haskellLib.url = "github:realfolk/haskell-lib/cd08238d2dc7739d50c19bbdd72aef956a6fcc05";
    haskellLogger.url = "github:realfolk/haskell-logger/2ffa3d1e58cd52b1e3d59f3487acbab20fb6fcc2";
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
    , haskellLib
    , haskellLogger
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

      # UPSTREAM LIBRARIES

      haskellLibLibrary = haskellLib.lib.${system}.defineLibProject {
        buildDir = config.buildDir;
        buildArtifactsDir = config.buildArtifactsDir;
      };

      haskellLoggerLibrary = haskellLogger.lib.${system}.defineLoggerProject {
        buildDir = config.buildDir;
        buildArtifactsDir = config.buildArtifactsDir;
      };

      # PROJECTS

      mdrnLibDefinition = {
        groupName = "mdrn";
        projectName = "lib";
        localDependencies = builtins.concatLists [
          ([ haskellLibLibrary haskellLoggerLibrary ])
          #TODO remove once automatically supported by realfolk/nix upstream
          haskellLibLibrary.localDependencies
          haskellLoggerLibrary.localDependencies
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
        localDependencies = builtins.concatLists [
          [ (defineHaskellProject mdrnLibDefinition) ]
          #TODO remove once automatically supported by realfolk/nix upstream
          mdrnLibDefinition.localDependencies
        ];
        executables = {
          main = "Main.hs";
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
        localDependencies = builtins.concatLists [
          [ (defineHaskellProject mdrnLibDefinition) ]
          #TODO remove once automatically supported by realfolk/nix upstream
          mdrnLibDefinition.localDependencies
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
        localDependencies = builtins.concatLists [
          [ (defineHaskellProject mdrnLibDefinition) ]
          #TODO remove once automatically supported by realfolk/nix upstream
          mdrnLibDefinition.localDependencies
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

      # LIBRARIES

      defineLibraryProject =
        { groupName
        , projectName
        , buildDir
        , buildArtifactsDir
        , srcPath
        , haskellDependencies ? (availableDependencies: [ ])
        , localDependencies ? [ ]
        , languageExtensions ? [ ]
        , ...
        }:
        haskellProject.lib.defineProject
          {
            inherit groupName projectName buildDir buildArtifactsDir haskellDependencies localDependencies languageExtensions;
            srcDir = "";
          } // {
          inherit srcPath;
        };

      defineMDRNProject =
        { buildDir
        , buildArtifactsDir
        , groupName ? "realfolk"
        , projectName ? "haskell-mdrn"
        , ...
        }:
        defineLibraryProject
          {
            inherit groupName projectName buildDir buildArtifactsDir haskellDependencies;
            srcPath = "${self}/src/mdrn/lib";
            localDependencies = mdrnLibDefinition.localDependencies;
          };
    in
    {
      lib = {
        inherit defineMDRNProject;
      };

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

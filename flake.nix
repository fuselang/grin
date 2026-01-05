{
  description = "GRIN - Graph Reduction Intermediate Notation";

  inputs = {
    # Follow haskellNix's nixpkgs for cache compatibility
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    # nixos-24.11 has LLVM 15 (removed from unstable)
    nixpkgs-llvm.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, nixpkgs, nixpkgs-llvm, flake-utils, haskellNix }:
  flake-utils.lib.eachDefaultSystem (system:
    let
      # Import LLVM 15 from nixos-23.11
      pkgs-llvm = import nixpkgs-llvm { inherit system; };
      # LLVM overlay - import LLVM 15 from older nixpkgs
      llvm-overlay = final: prev: {
        llvm_15 = pkgs-llvm.llvm_15.overrideAttrs (old: { doCheck = false; });
        clang_15 = pkgs-llvm.clang_15;
        llvm-config = pkgs-llvm.llvm_15.overrideAttrs (old: { doCheck = false; });
      };
      # Project overlay
      project-overlay = final: prev: {
        grinProject =
          final.haskell-nix.stackProject' {
            name = "grin";
            src = ./.;
            compiler-nix-name = "ghc967";
            modules = [{
              # Configure llvm-hs with LLVM 15 and its dependencies
              packages.llvm-hs.components.library.libs = final.lib.mkForce [
                final.llvm_15
                # LLVM's transitive dependencies (needed for linking)
                pkgs-llvm.libxml2
                pkgs-llvm.zlib
                pkgs-llvm.ncurses
              ];
              packages.llvm-hs.components.library.build-tools = [ final.llvm_15 ];
              # Fix macOS build: prevent llvm-hs from setting DYLD_LIBRARY_PATH
              # which causes clang to load the wrong LLVM version's libLLVM.dylib
              # See: https://discourse.nixos.org/t/how-to-get-llvm-hs-building-on-macosx-in-nixpkgs/4780
              packages.llvm-hs.postPatch = ''
                substituteInPlace Setup.hs --replace "addToLdLibraryPath libDir" "pure ()"
              '';

              # Enable LLVM backend in grin via cabal flag (sets WITH_LLVM_HS CPP flag)
              packages.grin.flags.with_llvm_hs = true;
            }];
          };
      };
      # Use haskellNix.overlay and config (standard flake pattern)
      pkgs = import nixpkgs {
        inherit system;
        inherit (haskellNix) config;
        overlays = [ haskellNix.overlay llvm-overlay project-overlay ];
      };
      flake = pkgs.grinProject.flake {};
      executable = "grin:exe:grin";
      app = flake-utils.lib.mkApp {
        name = "grin";
        exePath = "/bin/grin";
        drv = self.packages.${system}.${executable};
      };
    in flake // {
      # Built by `nix build .`
      packages = flake.packages // {
        default = flake.packages.${executable};
      };

      # `nix run`
      apps = (flake.apps or {}) // {
        grin = app;
        default = app;
      };

      # This is used by `nix develop .` to open a shell for use with
      # `cabal`, `hlint` and `haskell-language-server`
      devShells = {
        default = pkgs.grinProject.shellFor {
          tools = {
            cabal = "latest";
            hlint = "latest";
            haskell-language-server = "latest";
          };

          # Environment variables must be set via shellHook
          shellHook = ''
            export GRIN_CC="${pkgs.clang_15}/bin/clang"
            export GRIN_OPT="${pkgs.llvm_15}/bin/opt"
            export GRIN_LLC="${pkgs.llvm_15}/bin/llc"
          '';
        };
      };
    }
  );
}

let
  sources = import ./nix/sources.nix {};
  nixpkgs = import sources.nixpkgs {};
in
  nixpkgs.mkShell {
    buildInputs = with nixpkgs; [
      # Haskell tooling
      haskell.compiler.ghc96
      cabal-install
      haskellPackages.hlint
      haskellPackages.ghcid
      haskellPackages.hspec-discover

      # LLVM 15 toolchain
      clang_15
      llvm_15

      # Build dependencies
      pkg-config
      zlib
      ncurses
      libxml2
    ];

    shellHook = ''
      export GRIN_CC="${nixpkgs.clang_15}/bin/clang"
      export GRIN_OPT="${nixpkgs.llvm_15}/bin/opt"
      export GRIN_LLC="${nixpkgs.llvm_15}/bin/llc"
    '';
  }

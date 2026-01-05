let
  sources = import ./sources.nix {};
  haskellNix = import sources.haskellNix {};
  llvm-overlay = self: super: {
    llvm-config = self.llvm_15;
  };
  extra-overlays = [ llvm-overlay ];
  # Use our pinned nixpkgs (release-23.11) which has llvm_15
  pkgs = import
    sources.nixpkgs
    (haskellNix.nixpkgsArgs // { overlays = haskellNix.nixpkgsArgs.overlays ++ extra-overlays; });
in
  pkgs

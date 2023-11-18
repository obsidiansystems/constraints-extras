let pkgs = import ./nixpkgs.nix;
in
  pkgs.mkShell {
    name = "constraints-extras";
    buildInputs = with pkgs; [
      cabal-install
      ghcid
    ];
    inputsFrom = [
      (import ./release.nix).env
    ];
  }

{ pkgs ? import <nixpkgs> {}, unstable ? import <nixos-unstable> {} }:

with pkgs;

# I need to make a proper switch to Nix to take advantage of the cached
# binaries, but this gets it running at least on NixOS.
mkShell {
  buildInputs = [
    bashInteractive

    # Haskell toolchain
    stack
    ghc
    ormolu
    unstable.haskell-language-server # Stable has a bug with hlint

    # For OpenGL support
    mesa
    freeglut
    libGL
    libGLU
  ];

  # See https://stackoverflow.com/a/53093412
  LD_LIBRARY_PATH = with pkgs; "${freeglut}/lib";
}

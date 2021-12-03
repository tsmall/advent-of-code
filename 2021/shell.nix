# Latest nixpkgs-unstable build as of 11/30/2021
with import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/af21d41260846fb9c9840a75e310e56dfe97d6a3.tar.gz") {};

let
  gems = bundlerEnv {
    name = "gems-for-advent-of-code";
    ruby = ruby_3_0;
    gemdir = ./.;
  };
in mkShell {
  packages = [ gems gems.wrappedRuby bundix ];
}

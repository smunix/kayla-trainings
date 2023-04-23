{ pkgs, inputs, ... }:
with pkgs.lib;
with inputs.nix-utils.lib;
with inputs.nix-filter.lib;
with pkgs.haskell.lib;
let
  hpkgs = slow pkgs.haskellPackages [{
    modifiers = [ ];
    extension = hfinal: hprevious:
      with hfinal; {
        tree = callCabal2nix "tree" (filter { root = inputs.self; }) { };
      };
  }];
in {
  env.GREET = "devenv";
  packages = with pkgs; [
    git
    hpack
    (with hpkgs;
      ghcWithPackages
      (p: with p; [ cabal-install haskell-language-server tree ]))
  ];
  scripts.hello.exec = "echo hello from $GREET";
  scripts.repl.exec = with pkgs;
    "${ghcid}/bin/ghcid -W -a -T Lib.Top.main -c cabal repl 'lib:tree'";

  enterShell = ''
    hello
    git --version

  '';
} e

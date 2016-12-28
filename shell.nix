{ghc ? abort "ghc argument missing. Use Stack >= 1.2."}:

with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "hyperion";
  buildInputs =
    [ ncurses5 # For Intero
    ];
  inherit ghc;
  LANG = "en_US.utf8";
}

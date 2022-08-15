let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/22.05.tar.gz";
  }) {};

  # 2022-06-02 nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix
  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "5dca2f0f3b9ec0bceabb23fa1fd2b5f8ec30fa53";
    sha256 = "1vsc08ik9rs7vhnv8bg6bqf6gyqvywjfr5502rw1wpird74whhcs";
  }) { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "halogen-hooks";
  buildInputs = with pursPkgs; [
    pursPkgs.purs
    pursPkgs.spago
    pkgs.nodejs-16_x
    pkgs.esbuild
  ];
}

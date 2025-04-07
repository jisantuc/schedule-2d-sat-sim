{
  inputs = {
    interval-index.url = "github:jisantuc/interval-index";
    nixpkgs.follows = "interval-index/nixpkgs";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { interval-index, nixpkgs, utils, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          compiler = "ghc98";
          haskellPackages = pkgs.haskell.packages.${compiler}.extend (final: prev: {
            interval-index = interval-index.packages.${system}.default;
          });
          devDependencies = with haskellPackages; [
            cabal-fmt
            cabal-gild
            cabal-install
            haskell-language-server
            hlint
            ormolu
          ] ++ (with pkgs; [
            httpie
            kafkactl
            redis
          ]);
        in
        {
          devShells.default = haskellPackages.shellFor {
            packages = ps: [ (ps.callCabal2nix "schedule-2d-sat-sim" ./. { }) ];
            nativeBuildInputs = devDependencies;
            withHoogle = true;
          };
          devShells.ci = haskellPackages.shellFor {
            packages = ps: [ (ps.callCabal2nix "schedule-2d-sat-sim" ./. { }) ];
            nativeBuildInputs = with haskellPackages; [
              cabal-install
            ];
          };

          packages.default = haskellPackages.callCabal2nix "schedule-2d-sat-sim" ./. { };
        }
      );
}


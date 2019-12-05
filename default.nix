with import <nixpkgs> {}; {
    kgbotkaEnv = stdenv.mkDerivation {
        name = "kgbotkaEnv";
        buildInputs = [ ghc
                        stack
                        cabal-install
                        openssl
                        zlib
                        haskellPackages.ghcid
                        haskellPackages.hindent
                        haskellPackages.hlint
                      ];
        LD_LIBRARY_PATH="${openssl.out}/lib;${zlib}/lib";
    };
}

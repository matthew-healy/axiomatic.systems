final: prev:
  let
    inherit (prev.stdenv) mkDerivation;
    inherit (prev.lib.trivial) flip pipe;
    inherit (prev.haskell.lib)
      appendPatch
      appendConfigureFlags
      dontCheck
      doJailbreak;
    
    withPatch = flip appendPatch;
    withFlags = flip appendConfigureFlags;

    haskellCompiler = "ghc8107";
  in {
    axiomaticSystems.haskellPackages = prev.haskell.packages.${haskellCompiler}.override {
      overrides = hpFinal: hpPrev:
        let
          hakyll-src = hpPrev.callHackage "hakyll" "4.15.1.1" {};
          pandoc-src = hpPrev.callHackage "pandoc" "2.17.1.1" {};
        in rec {
          hakyll = pipe hakyll-src [
            doJailbreak
            dontCheck
            (withFlags [ "-f" "watchServer" "-f" "previewServer" ])
          ];

          pandoc = pipe pandoc-src [
            doJailbreak
            dontCheck
          ];

          tailwindcss = prev.nodePackages.tailwindcss;

          generator =
            let
              tailwindcss = prev.nodePackages.tailwindcss;
            in
              prev.symlinkJoin {
                name = "generator";
                paths = [ (hpPrev.callCabal2nix "generator" ../generator {}) ];
                buildInputs = [ prev.makeWrapper ];
                postBuild = ''
                  wrapProgram $out/bin/generator \
                    --set PATH ${prev.lib.getBin tailwindcss}/bin
                '';
              };

          website = prev.stdenv.mkDerivation {
            name = "axiomatic.systems";
            buildInputs = [ generator ];
            src = prev.nix-gitignore.gitignoreSourcePure [
              ../.gitignore
              ".git"
              ".github"
            ] ../.;

            LANG = "en_GB.UTF-8";
            LOCALE_ARCHIVE = prev.lib.optionalString
              (prev.buildPlatform.libc == "glibc")
              "${prev.glibcLocales}/lib/locale/locale-archive";

            buildPhase = ''
              generator build --verbose
            '';

            installPhase = ''
              mkdir -p "$out/dist"
              cp -r dist/* "$out/dist"
            '';
          };
        };
    };
  }
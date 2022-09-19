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

          generator = hpPrev.callCabal2nix "generator" ../generator {};

          website = prev.stdenv.mkDerivation {
            name = "axiomatic.systems";
            buildInputs = [ generator ] ++ (with prev.nodePackages; [ tailwindcss ]);
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
              site build --verbose
            '';

            installPhase = ''
              mkdir -p "$out/dist"
              cp -r dist/* "$out/dist"
            '';
          };
        };
    };
  }
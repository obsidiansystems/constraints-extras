let
  pkgs = import ./nixpkgs.nix;
  lib = pkgs.haskell.lib;
  ghc901 = pkgs.haskell.packages.ghc901.override {
    overrides = self: super: {
      type-equality = lib.doJailbreak super.type-equality;

      bifunctors = self.callHackageDirect {
        pkg = "bifunctors";
        ver = "5.5.10";
        sha256 = "0qc134fwnq3fg6h7q4maw2mi87a9zrjdwm4k0sqbcsiak3pgcmrf";
      } {};

      th-abstraction = self.callHackageDirect {
        pkg = "th-abstraction";
        ver = "0.4.2.0";
        sha256 = "0jy11vkpzkvkydn99pdasa8ywdcnqv2fdkvqmbybwm24kb8km2ks";
      } {};

      base-orphans = self.callHackageDirect {
        pkg = "base-orphans";
        ver = "0.8.4";
        sha256 = "0bg9b0fcllyfd8pc18aw0fffns71dsc8x66sglflf0y6qcjk17y7";
      } {};

      comonad = self.callHackageDirect {
        pkg = "comonad";
        ver = "5.0.8";
        sha256 = "1wwn8z9f3flqlka2k51wqw8wsjcxbp8mwg6yp3vbn6akyjrn36gc";
      } {};

      indexed-traversable = self.callHackageDirect {
        pkg = "indexed-traversable";
        ver = "0.1.1";
        sha256 = "1r5hvz6c90qcjc6r79r1vdv38l898saiv0027xzknlp48hcx8292";
      } {};

      tagged = self.callHackageDirect {
        pkg = "tagged";
        ver = "0.8.6.1";
        sha256 = "0mpx7zc7sxi9651gbakmd6jplfj7x624pipx3mi9yz7np7hh9jg8";
      } {};

      assoc = lib.doJailbreak (self.callHackageDirect {
        pkg = "assoc";
        ver = "1.0.2";
        sha256 = "1sfc21z18sf8rpsbcr77kgw7qjpm5cm1d24n5ifsm2zid88v8fs9";
      } {});

      these = lib.doJailbreak (self.callHackageDirect {
        pkg = "these";
        ver = "1.1.1.1";
        sha256 = "1i1nfh41vflvqxi8w8n2s35ymx2z9119dg5zmd2r23ya7vwvaka1";
      } {});

      quickcheck-instances = lib.doJailbreak super.quickcheck-instances;
      attoparsec = lib.dontCheck super.attoparsec;

      aeson = lib.dontCheck (self.callHackageDirect {
        pkg = "aeson";
        ver = "1.5.6.0";
        sha256 = "18yb8j0jvvzp275ylj16hskgxwdy55hljch9bjvpxl25vvslnk1n";
      } {});

      data-fix = self.callHackageDirect {
        pkg = "data-fix";
        ver = "0.3.1";
        sha256 = "0d9rj6kbaqyr9waj4yih2pp3qaswcip51jn9zdvs83gg8m29pqkj";
      } {};

      strict = self.callHackageDirect {
        pkg = "strict";
        ver = "0.4.0.1";
        sha256 = "0xhr98m2632k2pic8q9bpnm3mp9098mmg4s66ds052b92494k49f";
      } {};

    };
  };
in ghc901.callCabal2nix "constraints-extras" ./. {}

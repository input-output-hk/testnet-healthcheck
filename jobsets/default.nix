{ nixpkgs ? <nixpkgs>
, declInput ? {
    uri = "https://github.com/input-output-hk/testnet-healthcheck.git";
    rev = "refs/heads/master";
  }
, testnetHealthcheckPrsJSON ? ./simple-pr-dummy.json
}:
let pkgs = import nixpkgs {};

    testnetHealthcheckPrs = builtins.fromJSON (builtins.readFile testnetHealthcheckPrsJSON );

    mkGitSrc = { repo, branch ? "refs/heads/master", deepClone ? false }: {
      type = "git";
      value = repo + " " + branch + (if deepClone then " deepClone" else "");
      emailresponsible = false;
    };

    mkJob = { name, description, nixexprinput ? "jobsetSrc", nixexprpath, extraInputs }: {
      inherit name;
      value = {
        inherit description nixexprinput nixexprpath;

        inputs = {
          jobsetSrc = mkGitSrc {
            repo = declInput.uri;
            branch = declInput.rev;
          };

          nixpkgs = mkGitSrc {
            repo = "https://github.com/NixOS/nixpkgs-channels";
            branch = "refs/heads/nixos-18.03";
          };
        } // extraInputs;

        enabled = 1;
        hidden = false;
        checkinterval = 90;
        schedulingshares = 100;
        emailoverride = "";
        enableemail = false;
        keepnr = 3;
      };
    };

    mkTestnetHealthcheckJob = { name, description, testnetHealthcheckBranch }:
      mkJob {
        inherit name description;
        nixexprpath = "jobsets/release-testnet-healthcheck.nix";
        extraInputs = {
          testnetHealthcheckSrc = mkGitSrc {
            repo = "https://github.com/input-output-hk/testnet-healthcheck.git";
            branch = testnetHealthcheckBranch;
            deepClone = true;
          };
        };
      };

    testnetHealthcheckJobsetDefinition = pkgs.lib.listToAttrs (
      [
        (mkTestnetHealthcheckJob {
          name = "master";
          description = "master";
          testnetHealthcheckBranch = "refs/heads/master";
        })
      ]
      ++
      (pkgs.lib.mapAttrsToList
        (
          num:
          info: mkTestnetHealthcheckJob {
            name = "testnetHealthcheck-PR-${num}";
            description = info.title;
            testnetHealthcheckBranch = info.head.sha;
          }
        )
        testnetHealthcheckPrs
      )
    );

    jobsetDefinition = testnetHealthcheckJobsetDefinition;
in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF

    tee $out <<EOF
    ${builtins.toJSON jobsetDefinition}
    EOF
  '';
}

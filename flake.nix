{
  description = "Analysing the metre of Kristijonas Donelaitis's epic";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    lib = nixpkgs.lib;
    fetchMetai = name: hash: pkgs.fetchurl {
      url = "https://titus.fkidg1.uni-frankfurt.de/cordon/data/tei/${name}.tei.xml";
      inherit hash;
    };
    pythonInstallation = pkgs.python3.withPackages (p: [
      p.pandas
      p.jupyter
      p.matplotlib
      p.seaborn
      p.tabulate
      p.papermill
    ]);
    metaiXML = {
      PL = fetchMetai "DM_PL_1765_1775" "sha256-Fx4dZ+FQYegHRqX5jAc7b+GPCB0Sh7aLt6ZpDeayRsw=";
      WD = fetchMetai "DM_WD_1765_1775" "sha256-G4TzWZbZ3V1/xHqCsEBoHgFrLP+GA+KYaljpJBD6NxA=";
      RG = fetchMetai "DMN_RG_1869" "sha256-359tbpddJh6Us1mj6JcJgtwqhy+DcZt+nQ0xRF81ZMg=";
      ZR = fetchMetai "DMN_ZR_1869" "sha256-whh3/BPMmhpCxvIU9R/nKknsw6+1VKNph4uPn+PCnD8=";
    };
  in {
    apps.${system} = {
      jupyter = {
        type = "app";
        program = toString (pkgs.writers.writeDash "jupyter" ''
          PATH=${nixpkgs.lib.makeBinPath [pythonInstallation]} \
          EPIC_CSV=${self.packages.${system}.metai-scansion-csv} \
          jupyter notebook
        '');
      };
    };

    packages.${system} = rec {
      metai-csv = pkgs.runCommand "metai.csv" {} ''
        ${pkgs.writers.writePython3 "extract-cordon" {
          libraries = [pkgs.python3Packages.lxml];
          flakeIgnore = ["E501"];
        } (builtins.readFile ./extract_cordon.py)} ${toString (map lib.escapeShellArg (builtins.attrValues metaiXML))} > $out
      '';
      insert-cordon = pkgs.writers.writePython3Bin "insert-cordon" {
        libraries = [pkgs.python3Packages.pandas pkgs.python3Packages.numpy];
        flakeIgnore = ["E501" "W503"];
      } (builtins.readFile ./insert_cordon.py);
      metai-scansion-csv = pkgs.runCommand "metai-scansion.csv" {} ''
        cat ${metai-csv} | ${self.packages.${system}.metai}/bin/metai > $out
      '';
      metai-scanned-tei = pkgs.runCommand "metai-scanned" {} ''
        mkdir $out
        ${insert-cordon}/bin/insert-cordon ${metai-scansion-csv} < ${metaiXML.PL} > $out/PL_scanned.xml
        ${insert-cordon}/bin/insert-cordon ${metai-scansion-csv} < ${metaiXML.WD} > $out/WD_scanned.xml
        ${insert-cordon}/bin/insert-cordon ${metai-scansion-csv} < ${metaiXML.RG} > $out/RG_scanned.xml
        ${insert-cordon}/bin/insert-cordon ${metai-scansion-csv} < ${metaiXML.ZR} > $out/ZR_scanned.xml
      '';
      metai-assets = pkgs.runCommand "assets" {} ''
        PATH=$PATH:${nixpkgs.lib.makeBinPath [pythonInstallation]} \
        EPIC_CSV=${self.packages.${system}.metai-scansion-csv} \
        papermill ${./statistics.ipynb} /dev/null

        for figure in assets/*.svg; do
          ${pkgs.inkscape}/bin/inkscape -D --export-latex --export-filename="assets/$(basename "$figure" .svg).pdf" "$figure"
        done

        # make table page break footer empty. original one is ugly and in english
        ${pkgs.gnused}/bin/sed -i '/endhead/,/endfoot/{//!d}' assets/table-*.tex

        mkdir -p $out
        cp assets/*{md,html,svg,tex,pdf} $out/
      '';
      metai = pkgs.haskellPackages.callCabal2nix "metai" ./scansion {};
      inherit metaiXML;
    };

    devShells.${system}.default = with pkgs; haskellPackages.developPackage {
      root = ./scansion;
      modifier = drv: haskell.lib.addBuildTools drv (with haskellPackages; [
        cabal-install
        ghcid
        (hoogleWithPackages (_: drv.propagatedBuildInputs))
        fourmolu
        pythonInstallation
        pkg-config
        icu
      ]);
    };
  };
}

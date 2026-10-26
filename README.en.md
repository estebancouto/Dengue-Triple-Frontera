# Dengue in the Triple Border region: thesis analysis scripts

[Español](README.md) · [Reproducibility review](docs/REPRODUCIBILITY.md) · [Script inventory](docs/script_inventory.tsv)

R code from Esteban Manuel Couto’s thesis on dengue surveillance and transmission dynamics in the Argentina–Brazil–Paraguay Triple Border region. The retrospective observational study covers suspected dengue notifications from October 2009 to September 2023 in Puerto Iguazú, Foz do Iguaçu, Ciudad del Este, Hernandarias, Minga Guazú and Presidente Franco, using SNVS, SINAN and DGVS data.

## Reference doctoral thesis

**This README documents the analysis scripts for the doctoral thesis cited below.** The repository covers multiple analytical components of the thesis; it is not dedicated exclusively to a journal article. Publications derived from the thesis should identify the specific modules and outputs used in each manuscript.

Couto, Esteban Manuel. *Dengue en la triple frontera entre Argentina, Brasil y Paraguay: dinámica temporal y comparación entre municipios según caracterización del sistema de salud y vigilancia*. Rio de Janeiro: Fundação Oswaldo Cruz, Escola Nacional de Saúde Pública Sergio Arouca; 2026. Doctoral thesis in Public Health Epidemiology. 275 leaves: colour illustrations and maps. Bibliography: leaves 187–229. The original Spanish title is retained for citation.

- **Supervisor:** Luiz Antônio Bastos Camacho.
- **Co-supervisors:** Leonardo Soares Bastos and Zoraida del Carmen Fernandez Grillo.
- **Original catalogue subject headings:** Dengue; Saúde na Fronteira; Vigilância Epidemiológica; Sistemas de Saúde; Saúde Pública.

This reference was transcribed and normalized from the bibliographic record supplied by the author. Use it to cite the thesis; add the repository URL and exact commit or release to identify the code executed.

## External code for Rt estimation

Rt estimation uses functions associated with the following publication:

Codeço CT, Villela DAM, Coelho FC. Estimating the effective reproduction number of dengue considering temperature-dependent generation intervals. *Epidemics*. 2018;25:101–111. [doi:10.1016/j.epidem.2018.05.011](https://doi.org/10.1016/j.epidem.2018.05.011).

The method accounts for temperature-dependent generation intervals and was applied to Foz do Iguaçu and Rio de Janeiro. The [original repository](https://github.com/AlertaDengue/publication-data/tree/master/Codeco-et-al-2017) retains the directory name `Codeco-et-al-2017`, whereas the final paper was published in **2018**.

Required files and example:

- [EstRtGT_v4.R](https://github.com/AlertaDengue/publication-data/blob/master/Codeco-et-al-2017/EstRtGT_v4.R): includes `est.R.Temp()` and `evalGenTimeDist()`.
- [sumgamma_v2.R](https://github.com/AlertaDengue/publication-data/blob/master/Codeco-et-al-2017/sumgamma_v2.R): functions for distributions of sums of gamma random variables.
- [Rt_calc-example.rmd](https://github.com/AlertaDengue/publication-data/blob/master/Codeco-et-al-2017/Rt_calc-example.rmd): the authors’ usage example.

File headers credit **Daniel Villela**. These are external contributions and must not be attributed to the thesis author. Both files were located and inspected in their public source but **are not bundled in this package**. The prepared analysis scripts expect them under `R/external/`; confirm reuse terms before redistribution.

**Outstanding dependency:** `EstRtGT_v4.R` executes `read.csv("./SE.csv")` when sourced. `SE.csv` is absent from the inspected upstream directory. Resolve this read and check its use before execution. Identifying the source establishes provenance, but does not establish successful end-to-end execution or agreement with thesis results.

## Status and scope

This version organizes 18 author-supplied scripts for review and use as supporting code for publications. **It is not yet a validated reproduction of thesis or manuscript results.** Individual-level datasets and verified numerical results are not included. Analysis populations differ between modules. Several `CP` series exclude discarded cases; their correspondence with the manuscript’s probable-case definition must be checked.

Calculations, filters and models are preserved. Changes cover script organization, `source()` references and machine-specific paths. Previous versions remain in Git history; the latest author uploads are the source for this reorganization. Older `2.armonizacion.R` and `3.Validacion_armo.R` are removed from the active tree to avoid confusion with versions `2.1` and `3.1`.

## Organization and dependencies

| Module | Files under `scripts/` | Purpose and dependencies |
|---|---|---|
| Data | `01_data/01_import.R` | Original import; Argentina section incomplete |
| Harmonization | `01_data/02_harmonize.R` | National database recoding; exports to `source/csv/armo/` |
| Validation | `01_data/03_validate.R` | Harmonization tables and figures; requires harmonized data |
| Transmission | `02_transmission/01_build_weekly_rt.R` | Weekly series, population, climate and Rt; sources six `rt/` scripts; Brazil is calculated in the parent script |
| Features | `02_transmission/02_epidemic_features.R`, `03_feature_tables.R` | Epidemic descriptors; the latter reruns the former |
| Surveillance | `03_surveillance/01_quality.R`, `02_quality_figures.R`, `03_quality_tables.R` | Indicators and comparisons; figures and tables source the first script |
| Time series | `04_time_series/01_monthly.R`, `02_weekly.R` | Monthly and weekly analyses; require harmonized data |
| Integration | `04_time_series/03_integrated_figures.R` | Requires weekly series, Rt, descriptors and shading input |

`R/external/` is reserved for external Rt functions; `config/` lists packages and local directories; `docs/` records inputs, provenance and outstanding issues. Original local `source/` and `salida/` conventions are retained and excluded from Git.

## Software and preparation

R supporting `|>` (R ≥ 4.1) is required for syntax; this is not a validated runtime version. RStudio is optional. Original R/package versions are unavailable. `config/packages.txt` is a static inventory, not a dependency lockfile. The entry `colorspaces` requires clarification against `colorspace`, also used in the code. `webshot`/`webshot2` exports may require a browser and additional components. Review `parallel::mcmapply` portability for the intended operating system.

From the repository root:

```r
source("scripts/setup.R")        # create directories and report missing packages
source("scripts/check_syntax.R") # parse scripts without running analyses
```

Original scripts use `pacman::p_load()`, which may install packages automatically. Review the inventory first. No `renv.lock` is available, so `renv::restore()` cannot restore the original environment. After completing and validating the workflow in a clean environment, record dependencies using `renv::snapshot()` and retain `sessionInfo()`.

## Data and execution

Datasets are not distributed. Access to SNVS, SINAN and DGVS must follow the relevant institutional channels and authorizations. This repository grants neither data access nor redistribution rights; restrictions may differ by source.

The [input inventory](docs/input_inventory.tsv) lists literal paths expected by each script. Place authorized SINAN DBC files in `source/SINAN/`, Paraguay spreadsheets and the hospital catalogue in `source/Paraguay/`, Argentine inputs in `source/TAD/`, and intermediate CSV files in `source/csv/`. Annual population, weekly climate and both `R/external/` functions are also needed. This inventory is not a data dictionary or a guarantee of input compatibility.

**There is no validated end-to-end execution command.** After obtaining authorized inputs and resolving blockers, invoke modules from the repository root. For example:

```r
source("scripts/01_data/02_harmonize.R", encoding = "UTF-8")
```

Dependency order: import → harmonization → validation and surveillance/time-series/transmission branches. Integrated figures require weekly series, Rt and epidemic descriptors first. The six Rt scripts rely on parent-script objects and must not be run as independent sessions. Outstanding issues prevent claiming unattended execution.

## Outputs and manuscript use

Expected outputs include harmonization tables/figures (`salida/articulo_1/`), surveillance quality (`salida/articulo_2/`), and Rt, epidemic descriptors and time series (`salida/articulo_4/`). These are original output conventions, not verified manuscript table/figure mappings. Complete the [traceability register](docs/manuscript_traceability.tsv) with manuscript items, input versions and validation evidence.

Before journal submission: resolve blockers, run with authorized data, compare every output against the manuscript, freeze dependencies and archive a tagged version and commit. No DOI or final release has been assigned.

## Authorship, citation and license

Analysis author: **Esteban Manuel Couto**. Contact: [GitHub profile](https://github.com/estebancouto). Cite the thesis using the reference above and identify the code by repository URL, access date and exact commit. No repository DOI or citation for a derived journal article has been supplied.

No license has been selected. Public availability does not constitute a reuse license. Confirm rights separately for project code, external functions and data.

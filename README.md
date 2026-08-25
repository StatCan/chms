

<details open>
<summary>
English
</summary>

## Overview

**chms** provides tools for cleaning and summarizing accelerometer data
consistent with methods applied to cycle 7 of the
<a href="https://www.statcan.gc.ca/en/survey/household/5071" target="_blank">Canadian
Health Measures Survey</a> (CHMS):

- `agd$new()` initializes an R6 class with fields (data) and methods
  (functions) for processing
  <a href="https://ametris.com" target="_blank">Ametris</a> (formerly
  ActiGraph) accelerometer data.
- `agd$run()` executes a pipeline that calls several methods: `$load()`,
  `$clean()`, `$classify()` and `$summarize()`.
- `agd$sanity_check()` renders an html-formatted report of summary
  statistics per participant.

**chms** requires:

- two ActiGraph (.agd) files per participant, one processed with the
  normal filter (for step counts) and the other processed with the low
  frequency extension filter (for all other movement behaviour
  variables).
- accelerometer data at the 15-second epoch level for participants ≤ 17
  years.
- accelerometer data at the 60-second epoch level or lower for
  participants ≥ 18 years.
- integer age of each participant.

By default, **chms**:

- removes incomplete days of data (\< 24 hours).
- identifies sleep time, wear time and non-wear time by applying the
  <a href="https://www.pbrc.edu/pdf/PBRCSleepEpisodeTimeMacroCode.pdf" target="_blank">Barreira
  algorithm</a> to data at the 60-second epoch level.
- looks for sleep bouts beginning at 6pm (3-5 years) or 7pm (6+ years).
- identifies movement behaviours for eligible epochs (epochs previously
  classified as wear time and awake time) by applying the following
  cut-points to axis1 accelerometer counts at age-specific epoch levels:

| Age (years) | Epoch level (seconds) | SB cut-point (counts) | LPA cut-point (counts) | MPA cut-point (counts) | VPA cut-point (counts) |
|:--:|:--:|:--:|:--:|:--:|:--:|
| 3-4 | 15 | 0-24<sup><a href='https://doi.org/10.1080/02640410802334196' target='_blank'>Evenson</a></sup> | 25-419<sup><a href='https://doi.org/10.1038/oby.2006.234' target='_blank'>Pate</a></sup> | 420+<sup><a href='https://doi.org/10.1038/oby.2006.234' target='_blank'>Pate</a></sup> | – |
| 5-17 | 15 | 0-24<sup><a href='https://doi.org/10.1080/02640410802334196' target='_blank'>Evenson</a></sup> | 25-573<sup><a href='https://doi.org/10.1080/02640410802334196' target='_blank'>Evenson</a></sup> | 574-1,002<sup><a href='https://doi.org/10.1080/02640410802334196' target='_blank'>Evenson</a></sup> | 1,003+<sup><a href='https://doi.org/10.1080/02640410802334196' target='_blank'>Evenson</a></sup> |
| 18-64 | 60 | 0-99<sup><a href='https://doi.org/10.1249/mss.0b013e31815a51b3' target='_blank'>Troiano</a></sup> | 100-2,019<sup><a href='https://doi.org/10.1249/mss.0b013e31815a51b3' target='_blank'>Troiano</a></sup> | 2,020-5,998<sup><a href='https://doi.org/10.1249/mss.0b013e31815a51b3' target='_blank'>Troiano</a></sup> | 5,999+<sup><a href='https://doi.org/10.1249/mss.0b013e31815a51b3' target='_blank'>Troiano</a></sup> |
| 65+ | 60 | 0-99<sup><a href='https://doi.org/10.1249/mss.0b013e31815a51b3' target='_blank'>Troiano</a></sup> | 100-2,019<sup><a href='https://doi.org/10.1249/mss.0b013e31815a51b3' target='_blank'>Troiano</a></sup> | 2,020-5,998<sup><a href='https://doi.org/10.1249/mss.0b013e31815a51b3' target='_blank'>Troiano</a></sup> | 5,999+<sup><a href='https://doi.org/10.1249/mss.0b013e31815a51b3' target='_blank'>Troiano</a></sup> |

> **SB:** sedentary behaviour; **LPA:** light-intensity physical
> activity; **MPA:** moderate-intensity physical activity; **VPA:**
> vigorous-intensity physical activity.

- validates days (≥ 10 hours of wear time and ≥ 100 steps). Invalid days
  are excluded from waking hours results.
- validates nights of sleep (≥ 160 minutes of sleep). Invalid nights of
  sleep are excluded from sleeping hours results.

For more details on the methods used in the **chms** R package, see
Clarke J, Gribbon A, St-Laurent M, Ferrao T, Barnes J, Kuzik N, Colley
R.
<a href="https://www150.statcan.gc.ca/n1/pub/82-003-x/2026002/article/00001-eng.htm" target="_blank">Comparison
of physical activity and sedentary time measured with the ActiGraph
GT3X-BT and Actical accelerometers</a>. Health Rep. 2026 Feb
18;37(2):3-15. doi: 10.25318/82-003-x202600200001-eng. PMID: 41730515.

## Installation

``` r
remotes::install_git(
  url = "https://github.com/statcan/chms",
  force = TRUE,
  upgrade = "never"
)
```

## Usage

``` r
# Load dependencies into current R session
library(chms)
library(dplyr)
```

``` r
# Create participant meta (external/non-statcan users)
meta <- tibble(
  id = c("jane-canuck", "john-canuck"),
  age = c(10, 40),
  agd_lfe = c(
    system.file("extdata", "jane-canuck-lfe.agd", package = "chms"),
    system.file("extdata", "john-canuck-lfe.agd", package = "chms")
  ),
  agd_nml = c(
    system.file("extdata", "jane-canuck-nml.agd", package = "chms"),
    system.file("extdata", "john-canuck-nml.agd", package = "chms")
  ),
  start_date = c("2021-05-30", "2021-05-27"),
  epoch_length = c(15, 60)
)

# Print/examine
glimpse(meta)
#> Rows: 2
#> Columns: 6
#> $ id           <chr> "jane-canuck", "john-canuck"
#> $ age          <dbl> 10, 40
#> $ agd_lfe      <chr> "C:/Users/Clippy/AppData/Local/R/win-library/4.4/chms/ex…
#> $ agd_nml      <chr> "C:/Users/Clippy/AppData/Local/R/win-library/4.4/chms/ex…
#> $ start_date   <chr> "2021-05-30", "2021-05-27"
#> $ epoch_length <dbl> 15, 60
```

``` r
# Create participant meta (statcan users)
meta <- get_chms_meta(
  clinic_file = "path/to/clinic/file.sas7bdat",
  agd_dir = "path/to/agd/files/site",
  clinic_id = "CLINICID",
  site = "SITE",
  age = "CLC_AGE",
  day = "V2_DAY",
  month = "V2_MTH",
  year = "V2_YEAR"
)
```

``` r
# Initialize agd R6 class
agd_data <- agd$new(
  id = meta$id,
  age = meta$age,
  agd_lfe = meta$agd_lfe,
  agd_nml = meta$agd_nml,
  epoch_length = meta$epoch_length,
  day_max = 7,
  sleep_algo = "barreira",
  non_wear_algo = "barreira",
  start_date = meta$start_date,
  cpu_max = 2
)

# Print/examine
agd_data
#> 
#> ── 🍁chms::agd$print() method ──
#> 
#> Settings
#> 
#> # A tibble: 2 × 9
#>   id         age   agd_lfe agd_nml epoch_length day_max sleep_algo non_wear_algo
#>   <chr>      <chr> <chr>   <chr>   <chr>        <chr>   <chr>      <chr>        
#> 1 jane-canu… 10    C:/Use… C:/Use… 15           7       barreira   barreira     
#> 2 john-canu… 40    C:/Use… C:/Use… 60           7       barreira   barreira     
#> # ℹ 1 more variable: start_date <chr>
#> 
#> Log
#> 
#> # A tibble: 1 × 4
#>   method timestamp           status  message
#>   <chr>  <dttm>              <chr>   <chr>  
#> 1 new()  2026-08-25 19:22:40 success ""
```

``` r
# Run processing pipeline (load, clean, classify and summarize data)
agd_data$run()
#> 
#> ── 🍁chms::agd$run() method ──
#> 
#> ℹ Crunching data for 2 participants across 2 CPUs.
#> 
#> ■■■■■■■■■■■■■■■■                  50% | ETA:  7s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ✔ Done!
```

``` r
# Export results
agd_data$export(dir = tempdir())
#> 
#> ── 🍁chms::agd$export() method ──
#> 
#> ℹ Exporting results to 'C:\Users\Clippy\AppData\Local\Temp\Rtmp0kKTmq/agd-run-2026-08-25-19-22-48-815256'.
#> 
#> ✔ Done!
```

``` r
# Export statcan-formatted results
agd_data$export(dir = tempdir(), stc = TRUE)
#> 
#> ── 🍁chms::agd$export() method ──
#> 
#> ℹ Exporting `self$results$summary_full_stc` and `self$results$summary_run` to 'C:\Users\Clippy\AppData\Local\Temp\Rtmp0kKTmq'.
#> 
#> ✔ Done!
```

``` r
# Get settings and pipeline run log
agd_data
#> 
#> ── 🍁chms::agd$print() method ──
#> 
#> Settings
#> 
#> # A tibble: 2 × 9
#>   id         age   agd_lfe agd_nml epoch_length day_max sleep_algo non_wear_algo
#>   <chr>      <chr> <chr>   <chr>   <chr>        <chr>   <chr>      <chr>        
#> 1 jane-canu… 10    C:/Use… C:/Use… 15           7       barreira   barreira     
#> 2 john-canu… 40    C:/Use… C:/Use… 60           7       barreira   barreira     
#> # ℹ 1 more variable: start_date <chr>
#> 
#> Log
#> 
#> # A tibble: 2 × 4
#>   method timestamp           status  message
#>   <chr>  <dttm>              <chr>   <chr>  
#> 1 new()  2026-08-25 19:22:40 success ""     
#> 2 run()  2026-08-25 19:22:48 success ""
```

``` r
# Plot data
plot(agd_data, id = "jane-canuck")
#> 
#> ── 🍁chms::plot(agd) method ──
#> 
#> ℹ Rendering scatter plot for participant `jane-canuck`
```

<img src="man/figures/unnamed-chunk-13-1.png"
style="width: 75%;" />


    #> ✔ Done!

``` r
# Summarize data
summary(agd_data)
#> 
#> ── 🍁chms::summary(agd) method ──
#> 
#> Waking hours summary
#> Participant count: 2
#> 
#> # A tibble: 2 × 14
#>   participant_id device_serial_number wear_time  steps   lpa   mpa   vpa  mvpa
#>   <chr>          <chr>                    <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 jane-canuck    MOS2E26200432             14.7 10659.  251.  35.5  25.8  61.3
#> 2 john-canuck    MOS2E26200637             16.4 11166.  309.  32.4  17.9  50.3
#> # ℹ 6 more variables: lmvpa <dbl>, mpa_bouts <dbl>, vpa_bouts <dbl>,
#> #   mvpa_bouts <dbl>, sb <dbl>, valid_day <dbl>
#> 
#> Sleeping hours summary
#> Participant count: 2
#> 
#> # A tibble: 2 × 16
#>   participant_id device_serial_number wear_time sleep_period_time sleep_episodes
#>   <chr>          <chr>                    <dbl>             <dbl>          <dbl>
#> 1 jane-canuck    MOS2E26200432             9.32              9.63              1
#> 2 john-canuck    MOS2E26200637             7.28              7.28              1
#> # ℹ 11 more variables: nocturnal_sleep_midpoint <chr>, wake_episodes <dbl>,
#> #   total_wake_episode_time <dbl>, total_sleep_episode_time <dbl>,
#> #   sleep_episode_efficiency <dbl>, total_restful_sleep_time <dbl>,
#> #   sleep_episode_movements <dbl>, total_disrupted_sleep <dbl>,
#> #   restful_sleep_efficiency <dbl>, valid_day <dbl>, sleep_episode_log <chr>
```

``` r
# View all results in tab
agd_data$view()

# View specific results in tab
agd_data$view("summary_full")
agd_data$view("summary_full_stc")
agd_data$view("summary_run")
agd_data$view("summary_sleeping_hours")
agd_data$view("summary_waking_hours")

# View issues and run log
agd_data$view("issues")
agd_data$view("log")
```

``` r
# Store results in stand-alone data frames
summary_full <- agd_data$results$summary_full
summary_full_stc <- agd_data$results$summary_full_stc
summary_run <- agd_data$results$summary_run
summary_sleeping_hours <- agd_data$results$summary_sleeping_hours
summary_waking_hours <- agd_data$results$summary_waking_hours
```

``` r
# Render sanity check report
agd_data$sanity_check(dir = tempdir(), name = "My sanity check report")
```

## Documentation

``` r
?agd
```

## How to cite

``` r
citation("chms")
#> To cite chms in publications, please use:
#> 
#>   Clarke J, Gribbon A, St-Laurent M, Ferrao T, Barnes J, Kuzik N,
#>   Colley R (2026). "Comparison of physical activity and sedentary time
#>   measured with the ActiGraph GT3X-BT and Actical accelerometers."
#>   _Health Rep_, *18*(37(2)), 3-15.
#>   doi:10.25318/82-003-x202600200001-eng
#>   <https://doi.org/10.25318/82-003-x202600200001-eng>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {Comparison of physical activity and sedentary time measured with the ActiGraph GT3X-BT and Actical accelerometers},
#>     author = {J Clarke and A Gribbon and M St-Laurent and T Ferrao and J Barnes and N Kuzik and R Colley},
#>     journal = {Health Rep},
#>     year = {2026},
#>     volume = {18},
#>     number = {37(2)},
#>     pages = {3-15},
#>     doi = {10.25318/82-003-x202600200001-eng},
#>   }
```

</details>
<hr style="border: 3px solid grey; height: 4px">
<details open>
<summary>
Français
</summary>

## Vue d’ensemble

**L’ECMS** fournit des outils pour nettoyer et résumer les données de
l’accéléromètre conforme aux méthodes appliquées au cycle 7 du
<a href="https://www.statcan.gc.ca/fr/enquete/menages/5071" target="_blank">Enquête
canadienne sur les mesures de la santé</a> (ECMS) :

- `agd$new()` initialise une classe R6 avec des champs (données) et des
  méthodes (fonctions) pour le traitement
  <a href="https://ametris.com" target="_blank">Ametris</a>
  (anciennement ActiGraph).
- `agd$run()` exécute un pipeline qui appelle plusieurs méthodes :
  `$load()`, `$clean()`, `$classify()` et `$summarize()`.
- `agd$sanity_check()` Affiche un rapport de résumé au format HTML
  statistiques par participant.

**L’ECMS** exige :

- deux fichiers ActiGraph (.agd) par participant, dont un traité avec le
  filtre normal (pour le nombre de pas) et l’autre traité avec le filtre
  faible Filtre d’extension de fréquence (pour tous les autres
  comportements de mouvement variables).
- données de l’accéléromètre au niveau de l’époque de 15 secondes pour
  les participants ≤ 17 ans.
- les données de l’accéléromètre au niveau de l’époque de 60 secondes ou
  moins pour participants ≥ 18 ans.
- âge entier de chaque participant.

Par défaut, **l’ECMS** :

- supprime les jours incomplets de données (\< 24 heures).
- détermine le temps de sommeil, le temps de port et le temps de
  non-port en appliquant le
  <a href="https://www.pbrc.edu/pdf/PBRCSleepEpisodeTimeMacroCode.pdf" target="_blank">Barreira
  algorithme</a> aux données au niveau de l’époque de 60 secondes.
- recherche des périodes de sommeil à partir de 18 h (3 à 5 ans) ou de
  19 h (6 ans et +).
- détermine les comportements de mouvement pour les époques admissibles
  (époques antérieures classé comme temps de port et temps d’éveil) en
  appliquant ce qui suit Points de coupure du nombre d’accéléromètres de
  l’axe 1 à des niveaux d’époque spécifiques à l’âge :

| Âge (années) | Niveau d’époque (secondes) | Point de coupure de SB (nombres) | Point de coupure de l’APL (nombre) | Point de coupure de l’AMP (nombre) | Seuil de l’APV (nombre) |
|:--:|:--:|:--:|:--:|:--:|:--:|
| 3-4 | 15 | 0-24<sup><a href='https://doi.org/10.1080/02640410802334196' target='_blank'>Evenson</a></sup> | 25-419<sup><a href='https://doi.org/10.1038/oby.2006.234' target='_blank'>Pate</a></sup> | 420+<sup><a href='https://doi.org/10.1038/oby.2006.234' target='_blank'>Pate</a></sup> | – |
| 5-17 | 15 | 0-24<sup><a href='https://doi.org/10.1080/02640410802334196' target='_blank'>Evenson</a></sup> | 25-573<sup><a href='https://doi.org/10.1080/02640410802334196' target='_blank'>Evenson</a></sup> | 574-1,002<sup><a href='https://doi.org/10.1080/02640410802334196' target='_blank'>Evenson</a></sup> | 1,003+<sup><a href='https://doi.org/10.1080/02640410802334196' target='_blank'>Evenson</a></sup> |
| 18-64 | 60 | 0-99<sup><a href='https://doi.org/10.1249/mss.0b013e31815a51b3' target='_blank'>Troiano</a></sup> | 100-2,019<sup><a href='https://doi.org/10.1249/mss.0b013e31815a51b3' target='_blank'>Troiano</a></sup> | 2,020-5,998<sup><a href='https://doi.org/10.1249/mss.0b013e31815a51b3' target='_blank'>Troiano</a></sup> | 5,999+<sup><a href='https://doi.org/10.1249/mss.0b013e31815a51b3' target='_blank'>Troiano</a></sup> |
| 65+ | 60 | 0-99<sup><a href='https://doi.org/10.1249/mss.0b013e31815a51b3' target='_blank'>Troiano</a></sup> | 100-2,019<sup><a href='https://doi.org/10.1249/mss.0b013e31815a51b3' target='_blank'>Troiano</a></sup> | 2,020-5,998<sup><a href='https://doi.org/10.1249/mss.0b013e31815a51b3' target='_blank'>Troiano</a></sup> | 5,999+<sup><a href='https://doi.org/10.1249/mss.0b013e31815a51b3' target='_blank'>Troiano</a></sup> |

> **SB :** comportement sédentaire ; **APL :** physique d’intensité
> légère activité ; **APM :** activité physique d’intensité modérée ;
> **APV :** activité physique d’intensité vigoureuse.

- valide les jours (≥ 10 heures de port et ≥ 100 pas). Jours invalides
  sont exclus des résultats des heures d’éveil.
- valide les nuits de sommeil (≥ 160 minutes de sommeil). Nuits
  invalides de le sommeil sont exclus des résultats des heures de
  sommeil.

Pour plus de détails sur les méthodes utilisées dans l’ensemble R de l’
**ECMS** , voir Clarke J, Gribbon A, St-Laurent M, Ferrao T, Barnes J,
Kuzik N, Colley R.
<a href="https://www150.statcan.gc.ca/n1/pub/82-003-x/2026002/article/00001-fra.htm" target="_blank">Comparaison
de l’activité physique et du temps consacré à des activités sédentaires
mesurés à l’aide des accéléromètres ActiGraph GT3X-BT et Actical</a>.
Représentant de la santé 2026 févr. 18; 37(2):3-15. DOI :
10.25318/82-003-x202600200001-fra. PMID : 41730515.

## L’installation

``` r
remotes::install_git(
  url = "https://github.com/statcan/chms",
  force = TRUE,
  upgrade = "never"
)
```

## Utilisation

``` r
# Load dependencies into current R session
library(chms)
library(dplyr)
```

``` r
# Create participant meta (external/non-statcan users)
meta <- tibble(
  id = c("jane-canuck", "john-canuck"),
  age = c(10, 40),
  agd_lfe = c(
    system.file("extdata", "jane-canuck-lfe.agd", package = "chms"),
    system.file("extdata", "john-canuck-lfe.agd", package = "chms")
  ),
  agd_nml = c(
    system.file("extdata", "jane-canuck-nml.agd", package = "chms"),
    system.file("extdata", "john-canuck-nml.agd", package = "chms")
  ),
  start_date = c("2021-05-30", "2021-05-27"),
  epoch_length = c(15, 60)
)

# Print/examine
glimpse(meta)
#> Rows: 2
#> Columns: 6
#> $ id           <chr> "jane-canuck", "john-canuck"
#> $ age          <dbl> 10, 40
#> $ agd_lfe      <chr> "C:/Users/Clippy/Desktop/chms/inst/extdata/jane-c…
#> $ agd_nml      <chr> "C:/Users/Clippy/Desktop/chms/inst/extdata/jane-c…
#> $ start_date   <chr> "2021-05-30", "2021-05-27"
#> $ epoch_length <dbl> 15, 60
```

``` r
# Create participant meta (statcan users)
meta <- get_chms_meta(
  clinic_file = "path/to/clinic/file.sas7bdat",
  agd_dir = "path/to/agd/files/site",
  clinic_id = "CLINICID",
  site = "SITE",
  age = "CLC_AGE",
  day = "V2_DAY",
  month = "V2_MTH",
  year = "V2_YEAR"
)
```

``` r
# Initialize agd R6 class
agd_data <- agd$new(
  id = meta$id,
  age = meta$age,
  agd_lfe = meta$agd_lfe,
  agd_nml = meta$agd_nml,
  epoch_length = meta$epoch_length,
  day_max = 7,
  sleep_algo = "barreira",
  non_wear_algo = "barreira",
  start_date = meta$start_date,
  cpu_max = 2
)

# Print/examine
agd_data
#> 
#> ── 🍁chms::agd$print() method ──
#> 
#> Settings
#> 
#> # A tibble: 2 × 9
#>   id         age   agd_lfe agd_nml epoch_length day_max sleep_algo non_wear_algo
#>   <chr>      <chr> <chr>   <chr>   <chr>        <chr>   <chr>      <chr>        
#> 1 jane-canu… 10    C:/Use… C:/Use… 15           7       barreira   barreira     
#> 2 john-canu… 40    C:/Use… C:/Use… 60           7       barreira   barreira     
#> # ℹ 1 more variable: start_date <chr>
#> 
#> Log
#> 
#> # A tibble: 1 × 4
#>   method timestamp           status  message
#>   <chr>  <dttm>              <chr>   <chr>  
#> 1 new()  2026-08-25 19:23:04 success ""
```

``` r
# Run processing pipeline (load, clean, classify and summarize data)
agd_data$run()
#> 
#> ── 🍁chms::agd$run() method ──
#> 
#> ℹ Crunching data for 2 participants across 2 CPUs.
#> 
#> ■■■■■■■■■■■■■■■■                  50% | ETA:  7s
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ✔ Done!
```

``` r
# Export results
agd_data$export(dir = tempdir())
#> 
#> ── 🍁chms::agd$export() method ──
#> 
#> ℹ Exporting results to 'C:\Users\Clippy\AppData\Local\Temp\Rtmp0kKTmq/agd-run-2026-08-25-19-23-11-65182'.
#> 
#> ✔ Done!
```

``` r
# Export statcan-formatted results
agd_data$export(dir = tempdir(), stc = TRUE)
#> 
#> ── 🍁chms::agd$export() method ──
#> 
#> ℹ Exporting `self$results$summary_full_stc` and `self$results$summary_run` to 'C:\Users\Clippy\AppData\Local\Temp\Rtmp0kKTmq'.
#> 
#> ✔ Done!
```

``` r
# Get settings and pipeline run log
agd_data
#> 
#> ── 🍁chms::agd$print() method ──
#> 
#> Settings
#> 
#> # A tibble: 2 × 9
#>   id         age   agd_lfe agd_nml epoch_length day_max sleep_algo non_wear_algo
#>   <chr>      <chr> <chr>   <chr>   <chr>        <chr>   <chr>      <chr>        
#> 1 jane-canu… 10    C:/Use… C:/Use… 15           7       barreira   barreira     
#> 2 john-canu… 40    C:/Use… C:/Use… 60           7       barreira   barreira     
#> # ℹ 1 more variable: start_date <chr>
#> 
#> Log
#> 
#> # A tibble: 2 × 4
#>   method timestamp           status  message
#>   <chr>  <dttm>              <chr>   <chr>  
#> 1 new()  2026-08-25 19:23:04 success ""     
#> 2 run()  2026-08-25 19:23:11 success ""
```

``` r
# Plot data
plot(agd_data, id = "jane-canuck")
#> 
#> ── 🍁chms::plot(agd) method ──
#> 
#> ℹ Rendering scatter plot for participant `jane-canuck`
```

<img src="man/figures/unnamed-chunk-31-1.png"
style="width: 75%;" />


    #> ✔ Done!

``` r
# Summarize data
summary(agd_data)
#> 
#> ── 🍁chms::summary(agd) method ──
#> 
#> Waking hours summary
#> Participant count: 2
#> 
#> # A tibble: 2 × 14
#>   participant_id device_serial_number wear_time  steps   lpa   mpa   vpa  mvpa
#>   <chr>          <chr>                    <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 jane-canuck    MOS2E26200432             14.7 10659.  251.  35.5  25.8  61.3
#> 2 john-canuck    MOS2E26200637             16.4 11166.  309.  32.4  17.9  50.3
#> # ℹ 6 more variables: lmvpa <dbl>, mpa_bouts <dbl>, vpa_bouts <dbl>,
#> #   mvpa_bouts <dbl>, sb <dbl>, valid_day <dbl>
#> 
#> Sleeping hours summary
#> Participant count: 2
#> 
#> # A tibble: 2 × 16
#>   participant_id device_serial_number wear_time sleep_period_time sleep_episodes
#>   <chr>          <chr>                    <dbl>             <dbl>          <dbl>
#> 1 jane-canuck    MOS2E26200432             9.32              9.63              1
#> 2 john-canuck    MOS2E26200637             7.28              7.28              1
#> # ℹ 11 more variables: nocturnal_sleep_midpoint <chr>, wake_episodes <dbl>,
#> #   total_wake_episode_time <dbl>, total_sleep_episode_time <dbl>,
#> #   sleep_episode_efficiency <dbl>, total_restful_sleep_time <dbl>,
#> #   sleep_episode_movements <dbl>, total_disrupted_sleep <dbl>,
#> #   restful_sleep_efficiency <dbl>, valid_day <dbl>, sleep_episode_log <chr>
```

``` r
# View all results in tab
agd_data$view()

# View specific results in tab
agd_data$view("summary_full")
agd_data$view("summary_full_stc")
agd_data$view("summary_run")
agd_data$view("summary_sleeping_hours")
agd_data$view("summary_waking_hours")

# View issues and run log
agd_data$view("issues")
agd_data$view("log")
```

``` r
# Store results in stand-alone data frames
summary_full <- agd_data$results$summary_full
summary_full_stc <- agd_data$results$summary_full_stc
summary_run <- agd_data$results$summary_run
summary_sleeping_hours <- agd_data$results$summary_sleeping_hours
summary_waking_hours <- agd_data$results$summary_waking_hours
```

``` r
# Render sanity check report
agd_data$sanity_check(dir = tempdir(), name = "My sanity check report")
```

## Documentation

``` r
?agd
```

## Comment citer

``` r
citation("chms")
#> To cite chms in publications, please use:
#> 
#>   Clarke J, Gribbon A, St-Laurent M, Ferrao T, Barnes J, Kuzik N,
#>   Colley R (2026). "Comparison of physical activity and sedentary time
#>   measured with the ActiGraph GT3X-BT and Actical accelerometers."
#>   _Health Rep_, *18*(37(2)), 3-15.
#>   doi:10.25318/82-003-x202600200001-eng
#>   <https://doi.org/10.25318/82-003-x202600200001-eng>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {Comparison of physical activity and sedentary time measured with the ActiGraph GT3X-BT and Actical accelerometers},
#>     author = {J Clarke and A Gribbon and M St-Laurent and T Ferrao and J Barnes and N Kuzik and R Colley},
#>     journal = {Health Rep},
#>     year = {2026},
#>     volume = {18},
#>     number = {37(2)},
#>     pages = {3-15},
#>     doi = {10.25318/82-003-x202600200001-eng},
#>   }
```

</details>

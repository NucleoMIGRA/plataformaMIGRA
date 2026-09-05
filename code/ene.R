# ENE
# Fuente de datos: https://www.ine.gob.cl/estadisticas-por-tema/mercado-laboral/ocupacion-y-desocupacion
# Unidad de observación: persona encuestada por año ENE.
# Espera insumos en `data/raw/ene/anual/` y construye datos procesados y productos públicos.
# Aunque la carpeta publica no distribuye esas bases, aqui queda documentada la logica general del flujo:
# 1. lectura de datos originales;
# 2. procesamiento hacia `data/interim/`;
# 3. construccion de paneles en `data/final/`;
# 4. generacion de tablas espejo;
# 5. generacion de figuras publicables;
# 6. exportación hacia `figures/ene/` y `tables/ene/`.

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(scales)
})

run_ene <- function(build = c("all", "prepare", "figures"), force_rebuild = FALSE) {
  build <- match.arg(build)

  try(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")), silent = TRUE)
  try(suppressWarnings(Sys.setlocale("LC_CTYPE", "es_CL.UTF-8")), silent = TRUE)

  raw_dir <- here::here("data", "raw", "ene", "anual")
  interim_dir <- here::here("data", "interim", "ene")
  final_dir <- here::here("data", "final", "ene")
  figures_root <- here::here("figures", "ene")
  tables_root <- here::here("tables", "ene")

  figures_snapshot_dir <- figures_root
  figures_long_dir <- figures_root
  figures_compare_dir <- figures_root
  tables_snapshot_dir <- tables_root
  tables_long_dir <- tables_root
  tables_compare_dir <- tables_root

  dir.create(interim_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(figures_snapshot_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(figures_long_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(figures_compare_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tables_snapshot_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tables_long_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tables_compare_dir, recursive = TRUE, showWarnings = FALSE)

  log_msg <- function(...) message(sprintf("ENE: %s", paste0(..., collapse = "")))
  pick_base_family <- function() {
    available_fonts <- tryCatch(systemfonts::system_fonts()$family, error = function(e) character())
    available_fonts <- unique(available_fonts)
    dplyr::case_when(
      "Helvetica" %in% available_fonts ~ "Helvetica",
      "Arial" %in% available_fonts ~ "Arial",
      TRUE ~ "sans"
    )
  }
  font_family <- pick_base_family()
  wrap_label <- function(x, width = 26) {
    vapply(x, function(label) paste(strwrap(label, width = width), collapse = "\n"), character(1))
  }
  num_fmt <- function(x) scales::label_number(big.mark = ".", decimal.mark = ",")(x)

  years_target <- 2012:2025
  comparison_years <- 2018:2025
  key_vars <- c(
    "ano_encuesta", "mes_central", "region", "sexo", "edad", "mig4", "nacionalidad",
    "ocup_form", "b7a_1", "b7a_2", "b7a_3", "b7b_1", "b7b_2", "b7b_3", "b7b_4",
    "fact_anual", "categoria_ocupacion", "cae_general", "sector", "ftp", "obe"
  )
  missing_codes <- c(77L, 88L, 99L)

  public_name <- function(file_stem) {
    names <- c(
      fig_7_participacion_laboral_2012_2025 = "figura_01_participacion_laboral_2012_2025",
      fig_8_tasa_ocupacion_2012_2025 = "figura_02_tasa_ocupacion_2012_2025",
      fig_1_ocupacion_formal_informal_2025 = "figura_03_ocupacion_formal_informal_2025",
      fig_2_cotizacion_previsional_2025 = "figura_04_cotizacion_previsional_2025",
      fig_3_cotizacion_salud_2025 = "figura_05_cotizacion_salud_2025",
      fig_4_seguro_desempleo_2025 = "figura_06_seguro_desempleo_2025",
      fig_5_vacaciones_2025 = "figura_07_vacaciones_2025",
      fig_6_dias_pagados_enfermedad_2025 = "figura_08_dias_pagados_enfermedad_2025",
      fig_7_permiso_maternidad_2025 = "figura_09_permiso_maternidad_2025",
      fig_8_guarderia_2025 = "figura_10_guarderia_2025",
      fig_1_share_poblacion_migrante_2012_2025 = "figura_11_share_poblacion_migrante_2012_2025",
      fig_2_stock_poblacion_2012_2025 = "figura_12_stock_poblacion_2012_2025",
      fig_4_estructura_etaria_2012_2025 = "figura_13_estructura_etaria_2012_2025",
      fig_9_sectores_economicos_migrantes_2025 = "figura_14_sectores_economicos_migrantes_2025",
      fig_5_share_migrantes_region_2025 = "figura_15_share_migrantes_region_2025",
      fig_6_cambio_region_2012_2025 = "figura_16_cambio_region_2012_2025",
      fig_1_ocupacion_formal_2018_2025 = "figura_17_ocupacion_formal_2018_2025",
      fig_2_cotizacion_previsional_2018_2025 = "figura_18_cotizacion_previsional_2018_2025",
      fig_3_cotizacion_salud_2018_2025 = "figura_19_cotizacion_salud_2018_2025",
      fig_4_seguro_desempleo_2018_2025 = "figura_20_seguro_desempleo_2018_2025",
      fig_5_vacaciones_2018_2025 = "figura_21_vacaciones_2018_2025",
      fig_6_dias_pagados_enfermedad_2018_2025 = "figura_22_dias_pagados_enfermedad_2018_2025",
      fig_7_permiso_maternidad_2018_2025 = "figura_23_permiso_maternidad_2018_2025",
      fig_8_guarderia_2018_2025 = "figura_24_guarderia_2018_2025",
      fig_9_brecha_indicadores_2025 = "figura_25_brecha_indicadores_2025",
      fig_10_cambio_indicadores_2018_2025 = "figura_26_cambio_indicadores_2018_2025"
    )
    unname(names[[file_stem]])
  }

  parse_num <- function(x) {
    suppressWarnings(readr::parse_double(as.character(x), locale = locale(decimal_mark = ",", grouping_mark = ".")))
  }

  parse_int <- function(x) {
    suppressWarnings(as.integer(as.character(x)))
  }

  label_migrant <- function(mig4, nacionalidad) {
    mig4 <- parse_int(mig4)
    nacionalidad <- parse_int(nacionalidad)
    dplyr::case_when(
      mig4 %in% c(1L, 2L) ~ "Chilenos",
      mig4 == 3L ~ "Migrantes",
      is.na(mig4) & !is.na(nacionalidad) & nacionalidad == 152L ~ "Chilenos",
      is.na(mig4) & !is.na(nacionalidad) & nacionalidad != 152L ~ "Migrantes",
      TRUE ~ NA_character_
    )
  }

  migrant_source <- function(mig4, nacionalidad) {
    mig4 <- parse_int(mig4)
    nacionalidad <- parse_int(nacionalidad)
    dplyr::case_when(
      mig4 %in% c(1L, 2L, 3L) ~ "mig4",
      is.na(mig4) & !is.na(nacionalidad) ~ "nacionalidad",
      TRUE ~ NA_character_
    )
  }

  label_sex <- function(x) {
    x <- parse_int(x)
    dplyr::case_when(
      x == 1L ~ "Hombres",
      x == 2L ~ "Mujeres",
      TRUE ~ NA_character_
    )
  }

  valid_binary <- function(x) {
    x <- parse_int(x)
    !is.na(x) & !(x %in% missing_codes)
  }

  file_year <- function(path) {
    as.integer(stringr::str_extract(basename(path), "\\d{4}"))
  }

  annual_files <- list.files(raw_dir, pattern = "^ano-\\d{4}\\.csv$", full.names = TRUE) %>%
    tibble(path = .) %>%
    mutate(year = purrr::map_int(path, file_year)) %>%
    filter(year %in% years_target) %>%
    arrange(year)

  if (build %in% c("all", "prepare") && nrow(annual_files) == 0) {
    stop("ENE: no se encontraron archivos anuales 2012-2025 en data/raw/ene/anual.")
  }

  read_annual <- function(path, year) {
    header <- names(read_delim(path, delim = ";", n_max = 0, show_col_types = FALSE, progress = FALSE))
    available <- intersect(key_vars, header)
    missing <- setdiff(key_vars, header)

    dat <- read_delim(
      path,
      delim = ";",
      col_types = cols(.default = col_character()),
      col_select = any_of(available),
      show_col_types = FALSE,
      progress = FALSE,
      na = c("", "NA", "nan")
    )

    if (length(missing) > 0) {
      for (nm in missing) dat[[nm]] <- NA_character_
    }

    dat %>%
      mutate(
        year = year,
        weight = parse_num(fact_anual),
        region = parse_int(region),
        edad = parse_int(edad),
        mig4 = parse_int(mig4),
        sexo = parse_int(sexo),
        nacionalidad = parse_int(nacionalidad),
        ocup_form = parse_int(ocup_form),
        across(c(b7a_1, b7a_2, b7a_3, b7b_1, b7b_2, b7b_3, b7b_4, categoria_ocupacion, cae_general, sector, ftp, obe), parse_int),
        migrant_status = label_migrant(mig4, nacionalidad),
        migrant_source = migrant_source(mig4, nacionalidad),
        sex_label = label_sex(sexo),
        age_group = case_when(
          is.na(edad) ~ NA_character_,
          edad < 15 ~ "0 a 14",
          edad < 30 ~ "15 a 29",
          edad < 45 ~ "30 a 44",
          edad < 65 ~ "45 a 64",
          TRUE ~ "65 y más"
        ),
        valid_ocup_form = !is.na(ocup_form) & ocup_form %in% c(1L, 2L),
        valid_b7a_1 = valid_binary(b7a_1),
        valid_b7a_2 = valid_binary(b7a_2),
        valid_b7a_3 = valid_binary(b7a_3),
        valid_b7b_1 = valid_binary(b7b_1),
        valid_b7b_2 = valid_binary(b7b_2),
        valid_b7b_3 = valid_binary(b7b_3),
        valid_b7b_4 = valid_binary(b7b_4),
        labor_rights_module = if_any(starts_with("valid_b7"), identity),
        female = sexo == 2L
      ) %>%
      select(
        year, mes_central, region, sexo, sex_label, female, edad, age_group,
        mig4, nacionalidad, migrant_status, migrant_source,
        ocup_form, valid_ocup_form,
        b7a_1, b7a_2, b7a_3, b7b_1, b7b_2, b7b_3, b7b_4,
        valid_b7a_1, valid_b7a_2, valid_b7a_3, valid_b7b_1, valid_b7b_2, valid_b7b_3, valid_b7b_4,
        labor_rights_module,
        categoria_ocupacion, cae_general, sector, ftp, obe,
        weight
      )
  }

  build_panel <- function() {
    log_msg("construyendo panel anual reducido 2012-2025")

    yearly <- vector("list", nrow(annual_files))
    availability <- vector("list", nrow(annual_files))

    for (i in seq_len(nrow(annual_files))) {
      yr <- annual_files$year[i]
      path <- annual_files$path[i]
      log_msg("procesando ", yr, " desde ", basename(path))
      dat <- read_annual(path, yr)

      saveRDS(dat, file.path(interim_dir, sprintf("ene_%s_reducido.rds", yr)))

      availability[[i]] <- tibble(
        year = yr,
        variable = c("migrant_status", "ocup_form", "b7a_1", "b7a_2", "b7a_3", "b7b_1", "b7b_2", "b7b_3", "b7b_4", "weight"),
        non_missing = c(
          sum(!is.na(dat$migrant_status)),
          sum(dat$valid_ocup_form, na.rm = TRUE),
          sum(dat$valid_b7a_1, na.rm = TRUE),
          sum(dat$valid_b7a_2, na.rm = TRUE),
          sum(dat$valid_b7a_3, na.rm = TRUE),
          sum(dat$valid_b7b_1, na.rm = TRUE),
          sum(dat$valid_b7b_2, na.rm = TRUE),
          sum(dat$valid_b7b_3, na.rm = TRUE),
          sum(dat$valid_b7b_4, na.rm = TRUE),
          sum(!is.na(dat$weight))
        ),
        total_rows = nrow(dat)
      )

      yearly[[i]] <- dat
    }

    panel <- bind_rows(yearly)

    saveRDS(panel, file.path(interim_dir, "ene_panel_reducido.rds"))
    saveRDS(panel, file.path(final_dir, "ene_panel.rds"))

    availability_df <- bind_rows(availability) %>%
      mutate(share_non_missing = non_missing / total_rows)
    write_csv(availability_df, file.path(interim_dir, "ene_variable_availability.csv"))

    coverage <- panel %>%
      group_by(year, migrant_status, migrant_source) %>%
      summarise(
        n = n(),
        weighted_n = sum(weight, na.rm = TRUE),
        .groups = "drop"
      )
    write_csv(coverage, file.path(interim_dir, "ene_panel_coverage.csv"))

    summary_objects <- list(
      availability = availability_df,
      coverage = coverage,
      years = sort(unique(panel$year))
    )
    saveRDS(summary_objects, file.path(final_dir, "ene_summary_objects.rds"))

    readme_lines <- c(
      "# ENE interim",
      "",
      "Archivos mínimos para operar sin raw:",
      "- ene_panel_reducido.rds",
      "- ene_variable_availability.csv",
      "- ene_panel_coverage.csv",
      "",
      "Regla de uso:",
      "- `force_rebuild = TRUE`: reconstruye desde `data/raw/ene/anual`.",
      "- `force_rebuild = FALSE`: usa `interim` y `final`.",
      "",
      "Criterio migratorio:",
      "- 2020-2025: se usa la clasificación migratoria `mig4`.",
      "- 2012-2019: se usa `nacionalidad`, clasificando `152` como chilenos y el resto como migrantes.",
      "",
      "Cobertura actual:",
      sprintf("- años: %s", paste(sort(unique(panel$year)), collapse = ", "))
    )
    writeLines(readme_lines, file.path(interim_dir, "README_panel.md"))

    invisible(panel)
  }

  export_plot <- function(plot, filename, width = 9, height = 6) {
    if (requireNamespace("ragg", quietly = TRUE)) {
      ragg::agg_png(filename, width = width, height = height, units = "in", res = 200, background = "white")
      print(plot)
      grDevices::dev.off()
    } else {
      grDevices::png(filename, width = width, height = height, units = "in", res = 200, type = "cairo")
      print(plot)
      grDevices::dev.off()
    }

  }

  theme_migra <- function() {
    theme_minimal(base_family = font_family) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#d9e2ec", linewidth = 0.35),
        axis.title = element_text(color = "#102a43", size = 11),
        axis.text = element_text(color = "#243b53", size = 10),
        legend.title = element_blank(),
        legend.text = element_text(color = "#243b53", size = 10),
        plot.margin = margin(10, 16, 10, 10)
      )
  }

  migrant_palette <- c("Chilenos" = "#F28E2B", "Migrantes" = "#3A7CB5")
  response_palette <- c("Si" = "#2F7FB8", "No" = "#F28E2B", "Formal" = "#2F7FB8", "Informal" = "#F28E2B")
  year_palette <- c("2012" = "#F28E2B", "2025" = "#2F7FB8")
  gap_palette <- c("positive" = "#2F7FB8", "negative" = "#C95B5B")
  region_order_north_south <- c(15, 1, 2, 3, 4, 5, 13, 6, 7, 16, 8, 9, 14, 10, 11, 12)
  region_labels <- c(
    `15` = "Arica y Parinacota",
    `1` = "Tarapac\u00e1",
    `2` = "Antofagasta",
    `3` = "Atacama",
    `4` = "Coquimbo",
    `5` = "Valpara\u00edso",
    `13` = "Metropolitana",
    `6` = "O'Higgins",
    `7` = "Maule",
    `16` = "\u00d1uble",
    `8` = "Biob\u00edo",
    `9` = "La Araucan\u00eda",
    `14` = "Los R\u00edos",
    `10` = "Los Lagos",
    `11` = "Ays\u00e9n",
    `12` = "Magallanes"
  )
  region_labels_plot <- wrap_label(region_labels, width = 18)
  age_levels <- c("15 a 29", "30 a 44", "45 a 64", "65 y mas")
  sector_labels <- c(
    `1` = "Sector primario",
    `2` = "Sector secundario",
    `3` = "Sector terciario"
  )

  figure_specs <- list(
    list(idx = 1, var = "ocup_form", valid = "valid_ocup_form", label = c("1" = "Formal", "2" = "Informal"), female_only = FALSE,
         indicator_label = "Ocupacion formal",
         snapshot_file = "fig_1_ocupacion_formal_informal_2025", compare_file = "fig_1_ocupacion_formal_2018_2025", positive = 1L),
    list(idx = 2, var = "b7a_1", valid = "valid_b7a_1", label = c("1" = "Si", "2" = "No"), female_only = FALSE,
         indicator_label = "Cotizacion previsional",
         snapshot_file = "fig_2_cotizacion_previsional_2025", compare_file = "fig_2_cotizacion_previsional_2018_2025", positive = 1L),
    list(idx = 3, var = "b7a_2", valid = "valid_b7a_2", label = c("1" = "Si", "2" = "No"), female_only = FALSE,
         indicator_label = "Cotizacion de salud",
         snapshot_file = "fig_3_cotizacion_salud_2025", compare_file = "fig_3_cotizacion_salud_2018_2025", positive = 1L),
    list(idx = 4, var = "b7a_3", valid = "valid_b7a_3", label = c("1" = "Si", "2" = "No"), female_only = FALSE,
         indicator_label = "Seguro de desempleo",
         snapshot_file = "fig_4_seguro_desempleo_2025", compare_file = "fig_4_seguro_desempleo_2018_2025", positive = 1L),
    list(idx = 5, var = "b7b_1", valid = "valid_b7b_1", label = c("1" = "Si", "2" = "No"), female_only = FALSE,
         indicator_label = "Vacaciones",
         snapshot_file = "fig_5_vacaciones_2025", compare_file = "fig_5_vacaciones_2018_2025", positive = 1L),
    list(idx = 6, var = "b7b_2", valid = "valid_b7b_2", label = c("1" = "Si", "2" = "No"), female_only = FALSE,
         indicator_label = "Dias pagados por enfermedad",
         snapshot_file = "fig_6_dias_pagados_enfermedad_2025", compare_file = "fig_6_dias_pagados_enfermedad_2018_2025", positive = 1L),
    list(idx = 7, var = "b7b_3", valid = "valid_b7b_3", label = c("1" = "Si", "2" = "No"), female_only = TRUE,
         indicator_label = "Permiso de maternidad",
         snapshot_file = "fig_7_permiso_maternidad_2025", compare_file = "fig_7_permiso_maternidad_2018_2025", positive = 1L),
    list(idx = 8, var = "b7b_4", valid = "valid_b7b_4", label = c("1" = "Si", "2" = "No"), female_only = TRUE,
         indicator_label = "Guarderia o sala cuna",
         snapshot_file = "fig_8_guarderia_2025", compare_file = "fig_8_guarderia_2018_2025", positive = 1L)
  )

  prep_snapshot <- function(panel, spec) {
    dat <- panel %>%
      filter(year == 2025, !is.na(migrant_status), .data[[spec$valid]])

    if (isTRUE(spec$female_only)) dat <- dat %>% filter(female)

    dat %>%
      mutate(response = dplyr::recode(as.character(.data[[spec$var]]), !!!spec$label),
             response = factor(response, levels = unname(spec$label))) %>%
      group_by(migrant_status, response) %>%
      summarise(weighted_n = sum(weight, na.rm = TRUE), .groups = "drop_last") %>%
      mutate(share = 100 * weighted_n / sum(weighted_n)) %>%
      ungroup() %>%
      mutate(migrant_status = factor(migrant_status, levels = c("Chilenos", "Migrantes")))
  }

  prep_compare <- function(panel, spec) {
    dat <- panel %>%
      filter(year %in% comparison_years, !is.na(migrant_status), .data[[spec$valid]])

    if (isTRUE(spec$female_only)) dat <- dat %>% filter(female)

    dat %>%
      group_by(year, migrant_status) %>%
      summarise(
        weighted_yes = sum(weight[.data[[spec$var]] == spec$positive], na.rm = TRUE),
        weighted_total = sum(weight, na.rm = TRUE),
        share_yes = 100 * weighted_yes / weighted_total,
        .groups = "drop"
      ) %>%
      mutate(migrant_status = factor(migrant_status, levels = c("Chilenos", "Migrantes")))
  }

  prep_long_share <- function(panel) {
    panel %>%
      filter(!is.na(migrant_status)) %>%
      group_by(year, migrant_status) %>%
      summarise(pop = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(year) %>%
      mutate(total = sum(pop), share = 100 * pop / total) %>%
      ungroup() %>%
      filter(migrant_status == "Migrantes")
  }

  prep_long_stock <- function(panel) {
    panel %>%
      filter(!is.na(migrant_status)) %>%
      group_by(year, migrant_status) %>%
      summarise(pop = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      mutate(migrant_status = factor(migrant_status, levels = c("Chilenos", "Migrantes")))
  }

  prep_long_sex <- function(panel) {
    panel %>%
      filter(!is.na(migrant_status), !is.na(sex_label), edad >= 15) %>%
      group_by(year, migrant_status, sex_label) %>%
      summarise(pop = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(year, migrant_status) %>%
      mutate(share = 100 * pop / sum(pop)) %>%
      ungroup() %>%
      filter(sex_label == "Mujeres") %>%
      mutate(migrant_status = factor(migrant_status, levels = c("Chilenos", "Migrantes")))
  }

  prep_long_age <- function(panel) {
    panel %>%
      filter(year %in% c(2012, 2025), !is.na(migrant_status), edad >= 15, !is.na(age_group)) %>%
      mutate(
        age_group = if_else(age_group == "65 y más", "65 y mas", age_group),
        age_group = factor(age_group, levels = rev(age_levels))
      ) %>%
      group_by(year, migrant_status, age_group) %>%
      summarise(pop = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(year, migrant_status) %>%
      mutate(share = 100 * pop / sum(pop)) %>%
      ungroup() %>%
      mutate(
        migrant_status = factor(migrant_status, levels = c("Chilenos", "Migrantes")),
        year_label = factor(as.character(year), levels = c("2012", "2025"))
      )
  }

  prep_region_snapshot <- function(panel) {
    panel %>%
      filter(year == 2025, !is.na(migrant_status), !is.na(region), region %in% region_order_north_south) %>%
      group_by(region, migrant_status) %>%
      summarise(pop = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(region) %>%
      mutate(total = sum(pop), share = 100 * pop / total) %>%
      ungroup() %>%
      filter(migrant_status == "Migrantes") %>%
      mutate(
        region_label = recode(as.character(region), !!!region_labels),
        region_plot = factor(
          wrap_label(region_label, width = 18),
          levels = rev(region_labels_plot[as.character(region_order_north_south)])
        )
      )
  }

  prep_region_change <- function(panel) {
    dat <- panel %>%
      filter(year %in% c(2012, 2025), !is.na(migrant_status), !is.na(region), region %in% region_order_north_south) %>%
      group_by(year, region, migrant_status) %>%
      summarise(pop = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(year, region) %>%
      mutate(total = sum(pop), share = 100 * pop / total) %>%
      ungroup() %>%
      filter(migrant_status == "Migrantes") %>%
      mutate(region_name = recode(as.character(region), !!!region_labels))

    dat %>%
      mutate(
        region_label = region_name,
        region_plot = factor(
          wrap_label(region_name, width = 18),
          levels = rev(region_labels_plot[as.character(region_order_north_south)])
        ),
        year_label = factor(as.character(year), levels = c("2012", "2025"))
      )
  }

  prep_labor_participation <- function(panel) {
    panel %>%
      filter(year %in% years_target, !is.na(migrant_status), edad >= 15, !is.na(cae_general)) %>%
      group_by(year, migrant_status) %>%
      summarise(
        labor_force = sum(weight[cae_general %in% c(1L, 2L, 3L)], na.rm = TRUE),
        population_15plus = sum(weight, na.rm = TRUE),
        share = 100 * labor_force / population_15plus,
        .groups = "drop"
      ) %>%
      mutate(migrant_status = factor(migrant_status, levels = c("Chilenos", "Migrantes")))
  }

  prep_employment_rate <- function(panel) {
    panel %>%
      filter(year %in% years_target, !is.na(migrant_status), edad >= 15, !is.na(cae_general)) %>%
      group_by(year, migrant_status) %>%
      summarise(
        occupied = sum(weight[cae_general == 1L], na.rm = TRUE),
        population_15plus = sum(weight, na.rm = TRUE),
        share = 100 * occupied / population_15plus,
        .groups = "drop"
      ) %>%
      mutate(migrant_status = factor(migrant_status, levels = c("Chilenos", "Migrantes")))
  }

  prep_region_employment_snapshot <- function(panel) {
    panel %>%
      filter(year == 2025, cae_general == 1L, !is.na(region), region %in% region_order_north_south, !is.na(migrant_status)) %>%
      group_by(region, migrant_status) %>%
      summarise(occupied = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(region) %>%
      mutate(total_regional_occupied = sum(occupied), share = 100 * occupied / total_regional_occupied) %>%
      ungroup() %>%
      filter(migrant_status == "Migrantes") %>%
      mutate(
        region_label = recode(as.character(region), !!!region_labels),
        region_plot = factor(
          wrap_label(region_label, width = 18),
          levels = rev(region_labels_plot[as.character(region_order_north_south)])
        )
      )
  }

  prep_sector_snapshot <- function(panel) {
    panel %>%
      filter(year == 2025, cae_general == 1L, migrant_status == "Migrantes", !is.na(sector), sector %in% c(1L, 2L, 3L)) %>%
      group_by(sector) %>%
      summarise(occupied = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        sector_label = recode(as.character(sector), !!!sector_labels),
        share = 100 * occupied / sum(occupied),
        sector_plot = factor(sector_label, levels = rev(sector_label[order(share)]))
      )
  }

  prep_compare_gap <- function(panel) {
    purrr::map_dfr(figure_specs, function(spec) {
      prep_compare(panel, spec) %>%
        filter(year == 2025) %>%
        transmute(indicator = spec$indicator_label, migrant_status, share = share_yes)
    }) %>%
      tidyr::pivot_wider(names_from = migrant_status, values_from = share) %>%
      mutate(gap = Migrantes - Chilenos) %>%
      arrange(gap) %>%
      mutate(indicator = factor(indicator, levels = indicator))
  }

  prep_compare_change <- function(panel) {
    dat <- purrr::map_dfr(figure_specs, function(spec) {
      prep_compare(panel, spec) %>%
        filter(year %in% c(2018, 2025)) %>%
        transmute(indicator = spec$indicator_label, year, migrant_status, share = share_yes)
    })

    order_levels <- dat %>%
      filter(year == 2025, migrant_status == "Chilenos") %>%
      arrange(share) %>%
      pull(indicator)

    dat %>%
      mutate(
        indicator = factor(indicator, levels = order_levels),
        year_label = factor(as.character(year), levels = c("2018", "2025")),
        migrant_status = factor(migrant_status, levels = c("Chilenos", "Migrantes"))
      )
  }

  plot_snapshot <- function(df, spec) {
    ggplot(df, aes(x = migrant_status, y = share, fill = response)) +
      geom_col(width = 0.62, color = "white") +
      geom_text(aes(label = ifelse(share >= 5, sprintf("%.1f%%", share), "")),
                position = position_stack(vjust = 0.5), size = 3.6, color = "white", fontface = "bold") +
      scale_fill_manual(values = response_palette[levels(df$response)], drop = FALSE) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
      labs(x = NULL, y = "Porcentaje") +
      theme_migra() +
      theme(legend.position = "bottom")
  }

  plot_compare <- function(df) {
    ggplot(df, aes(x = year, y = share_yes, color = migrant_status)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.6) +
      scale_color_manual(values = migrant_palette) +
      scale_x_continuous(breaks = comparison_years) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = scales::pretty_breaks(n = 5), expand = expansion(mult = c(0.02, 0.05))) +
      labs(x = NULL, y = "Porcentaje") +
      theme_migra() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)
      )
  }

  plot_long_labor <- function(df, ylab = "Porcentaje") {
    ggplot(df, aes(x = year, y = share, color = migrant_status)) +
      geom_line(linewidth = 1.25) +
      geom_point(size = 2.6) +
      scale_color_manual(values = migrant_palette) +
      scale_x_continuous(breaks = years_target) +
      scale_y_continuous(labels = function(x) paste0(round(x, 1), "%"), breaks = scales::pretty_breaks(n = 6), expand = expansion(mult = c(0.02, 0.05))) +
      labs(x = NULL, y = ylab) +
      theme_migra() +
      theme(legend.position = "bottom")
  }

  plot_long_share <- function(df) {
    ggplot(df, aes(x = year, y = share)) +
      geom_area(fill = "#D7E5F3", alpha = 0.8) +
      geom_line(color = "#2F7FB8", linewidth = 1.3) +
      geom_point(color = "#2F7FB8", size = 2.5) +
      scale_x_continuous(breaks = years_target) +
      scale_y_continuous(labels = function(x) paste0(round(x, 1), "%"), breaks = scales::pretty_breaks(n = 6), expand = expansion(mult = c(0, 0.04))) +
      labs(x = NULL, y = "Porcentaje") +
      theme_migra()
  }

  plot_long_stock <- function(df) {
    ggplot(df, aes(x = year, y = pop / 1e6, color = migrant_status)) +
      geom_line(linewidth = 1.25) +
      geom_point(size = 2.5) +
      scale_color_manual(values = migrant_palette) +
      scale_x_continuous(breaks = years_target) +
      scale_y_continuous(labels = function(x) paste0(format(round(x, 1), nsmall = 1), "M"), breaks = scales::pretty_breaks(n = 6)) +
      labs(x = NULL, y = "Personas") +
      theme_migra() +
      theme(legend.position = "bottom")
  }

  plot_long_sex <- function(df) {
    ggplot(df, aes(x = year, y = share, color = migrant_status)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.5) +
      scale_color_manual(values = migrant_palette) +
      scale_x_continuous(breaks = years_target) +
      scale_y_continuous(labels = function(x) paste0(round(x, 1), "%"), breaks = scales::pretty_breaks(n = 6)) +
      labs(x = NULL, y = "Mujeres sobre el total") +
      theme_migra() +
      theme(legend.position = "bottom")
  }

  plot_long_age <- function(df) {
    ggplot(df, aes(x = share, y = age_group, color = year_label)) +
      geom_line(aes(group = interaction(migrant_status, age_group)), color = "#BFC7D5", linewidth = 0.8) +
      geom_point(size = 3) +
      scale_color_manual(values = year_palette) +
      scale_x_continuous(labels = function(x) paste0(round(x, 0), "%"), breaks = scales::pretty_breaks(n = 5)) +
      labs(x = "Porcentaje", y = NULL) +
      facet_wrap(~migrant_status, ncol = 2) +
      theme_migra() +
      theme(legend.position = "bottom")
  }

  plot_region_snapshot <- function(df) {
    ggplot(df, aes(x = region_plot, y = share)) +
      geom_col(fill = "#2F7FB8", width = 0.68) +
      geom_text(aes(label = sprintf("%.1f%%", share)), hjust = -0.1, size = 3.3, color = "#334E68") +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = function(x) paste0(round(x, 0), "%"), expand = expansion(mult = c(0, 0.1))) +
      labs(x = NULL, y = "Porcentaje") +
      theme_migra()
  }

  plot_region_change <- function(df) {
    wide <- df %>%
      group_by(region_plot, year_label) %>%
      summarise(share = sum(share, na.rm = TRUE), .groups = "drop") %>%
      select(region_plot, year_label, share) %>%
      tidyr::pivot_wider(names_from = year_label, values_from = share)

    ggplot(wide, aes(y = region_plot)) +
      geom_segment(aes(x = `2012`, xend = `2025`, yend = region_plot), color = "#BFC7D5", linewidth = 1) +
      geom_point(aes(x = `2012`, color = "2012"), size = 2.8) +
      geom_point(aes(x = `2025`, color = "2025"), size = 2.8) +
      scale_color_manual(values = year_palette) +
      scale_x_continuous(labels = function(x) paste0(round(x, 0), "%"), expand = expansion(mult = c(0.02, 0.08))) +
      labs(x = "Porcentaje de migrantes en la poblacion regional", y = NULL) +
      theme_migra() +
      theme(legend.position = "bottom")
  }

  plot_region_employment_snapshot <- function(df) {
    ggplot(df, aes(x = region_plot, y = share)) +
      geom_col(fill = "#2F7FB8", width = 0.68) +
      geom_text(aes(label = sprintf("%.1f%%", share)), hjust = -0.1, size = 3.3, color = "#334E68") +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = function(x) paste0(round(x, 0), "%"), expand = expansion(mult = c(0, 0.1))) +
      labs(x = NULL, y = "Porcentaje de ocupados regionales") +
      theme_migra()
  }

  plot_sector_snapshot <- function(df) {
    ggplot(df, aes(x = sector_plot, y = share)) +
      geom_col(fill = "#2F7FB8", width = 0.68) +
      geom_text(aes(label = sprintf("%.1f%%", share)), hjust = -0.1, size = 3.4, color = "#334E68") +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = function(x) paste0(round(x, 0), "%"), expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = "Porcentaje de ocupados migrantes") +
      theme_migra()
  }

  plot_compare_gap <- function(df) {
    ggplot(df, aes(x = indicator, y = gap, fill = if_else(gap >= 0, "positive", "negative"))) +
      geom_col(width = 0.7) +
      geom_hline(yintercept = 0, color = "#8EA0B3", linewidth = 0.7) +
      geom_text(aes(label = sprintf("%+.1f pp", gap)),
                hjust = if_else(df$gap >= 0, -0.1, 1.1), size = 3.3, color = "#334E68") +
      coord_flip(clip = "off") +
      scale_fill_manual(values = gap_palette, guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0.12, 0.12))) +
      labs(x = NULL, y = "Brecha migrantes - chilenos (pp)") +
      theme_migra()
  }

  plot_compare_change <- function(df) {
    wide <- df %>%
      select(indicator, migrant_status, year_label, share) %>%
      tidyr::pivot_wider(names_from = year_label, values_from = share)

    ggplot(wide, aes(y = indicator)) +
      geom_segment(aes(x = `2018`, xend = `2025`, yend = indicator), color = "#BFC7D5", linewidth = 1) +
      geom_point(aes(x = `2018`, color = "2018"), size = 2.8) +
      geom_point(aes(x = `2025`, color = "2025"), size = 2.8) +
      scale_color_manual(values = c("2018" = "#F28E2B", "2025" = "#2F7FB8")) +
      scale_x_continuous(labels = function(x) paste0(round(x, 0), "%"), expand = expansion(mult = c(0.02, 0.05))) +
      labs(x = "Porcentaje", y = NULL) +
      facet_wrap(~migrant_status, ncol = 2) +
      theme_migra() +
      theme(legend.position = "bottom")
  }

  build_figures <- function(panel) {
    log_msg("generando figuras y tablas")

    snapshot_tables <- vector("list", length(figure_specs))
    compare_tables <- vector("list", length(figure_specs))

    for (i in seq_along(figure_specs)) {
      spec <- figure_specs[[i]]

      snap_df <- prep_snapshot(panel, spec)
      comp_df <- prep_compare(panel, spec)

      snapshot_tables[[i]] <- snap_df %>% mutate(figure = spec$snapshot_file)
      compare_tables[[i]] <- comp_df %>% mutate(figure = spec$compare_file)

      write_csv(snap_df, file.path(tables_snapshot_dir, paste0(public_name(spec$snapshot_file), ".csv")))
      write_csv(comp_df, file.path(tables_compare_dir, paste0(public_name(spec$compare_file), ".csv")))

      log_msg("exportando ", spec$snapshot_file)
      export_plot(plot_snapshot(snap_df, spec), file.path(figures_snapshot_dir, paste0(public_name(spec$snapshot_file), ".png")), width = 8.5, height = 6)
      log_msg("exportando ", spec$compare_file)
      export_plot(plot_compare(comp_df), file.path(figures_compare_dir, paste0(public_name(spec$compare_file), ".png")), width = 8.8, height = 5.8)
    }

    long_share <- prep_long_share(panel)
    long_stock <- prep_long_stock(panel)
    long_sex <- prep_long_sex(panel)
    long_age <- prep_long_age(panel)
    region_snapshot <- prep_region_snapshot(panel)
    region_change <- prep_region_change(panel)
    labor_participation <- prep_labor_participation(panel)
    employment_rate <- prep_employment_rate(panel)
    region_employment_snapshot <- prep_region_employment_snapshot(panel)
    sector_snapshot <- prep_sector_snapshot(panel)
    compare_gap <- prep_compare_gap(panel)
    compare_change <- prep_compare_change(panel)

    write_csv(long_share, file.path(tables_long_dir, paste0(public_name("fig_1_share_poblacion_migrante_2012_2025"), ".csv")))
    write_csv(long_stock, file.path(tables_long_dir, paste0(public_name("fig_2_stock_poblacion_2012_2025"), ".csv")))
    write_csv(long_age, file.path(tables_long_dir, paste0(public_name("fig_4_estructura_etaria_2012_2025"), ".csv")))
    write_csv(region_snapshot, file.path(tables_long_dir, paste0(public_name("fig_5_share_migrantes_region_2025"), ".csv")))
    write_csv(region_change, file.path(tables_long_dir, paste0(public_name("fig_6_cambio_region_2012_2025"), ".csv")))
    write_csv(labor_participation, file.path(tables_long_dir, paste0(public_name("fig_7_participacion_laboral_2012_2025"), ".csv")))
    write_csv(employment_rate, file.path(tables_long_dir, paste0(public_name("fig_8_tasa_ocupacion_2012_2025"), ".csv")))
    write_csv(sector_snapshot, file.path(tables_snapshot_dir, paste0(public_name("fig_9_sectores_economicos_migrantes_2025"), ".csv")))
    write_csv(compare_gap, file.path(tables_compare_dir, paste0(public_name("fig_9_brecha_indicadores_2025"), ".csv")))
    write_csv(compare_change, file.path(tables_compare_dir, paste0(public_name("fig_10_cambio_indicadores_2018_2025"), ".csv")))

    log_msg("exportando fig_1_share_poblacion_migrante_2012_2025")
    export_plot(plot_long_share(long_share), file.path(figures_long_dir, paste0(public_name("fig_1_share_poblacion_migrante_2012_2025"), ".png")), width = 8.8, height = 5.8)
    log_msg("exportando fig_2_stock_poblacion_2012_2025")
    export_plot(plot_long_stock(long_stock), file.path(figures_long_dir, paste0(public_name("fig_2_stock_poblacion_2012_2025"), ".png")), width = 8.8, height = 5.8)
    log_msg("exportando fig_4_estructura_etaria_2012_2025")
    export_plot(plot_long_age(long_age), file.path(figures_long_dir, paste0(public_name("fig_4_estructura_etaria_2012_2025"), ".png")), width = 9.2, height = 5.8)
    log_msg("exportando fig_5_share_migrantes_region_2025")
    export_plot(plot_region_snapshot(region_snapshot), file.path(figures_long_dir, paste0(public_name("fig_5_share_migrantes_region_2025"), ".png")), width = 8.8, height = 7.6)
    log_msg("exportando fig_6_cambio_region_2012_2025")
    export_plot(plot_region_change(region_change), file.path(figures_long_dir, paste0(public_name("fig_6_cambio_region_2012_2025"), ".png")), width = 9, height = 7.6)
    log_msg("exportando fig_7_participacion_laboral_2012_2025")
    export_plot(plot_long_labor(labor_participation, "Participación laboral"), file.path(figures_long_dir, paste0(public_name("fig_7_participacion_laboral_2012_2025"), ".png")), width = 8.8, height = 5.8)
    log_msg("exportando fig_8_tasa_ocupacion_2012_2025")
    export_plot(plot_long_labor(employment_rate, "Tasa de ocupación"), file.path(figures_long_dir, paste0(public_name("fig_8_tasa_ocupacion_2012_2025"), ".png")), width = 8.8, height = 5.8)
    log_msg("exportando fig_9_sectores_economicos_migrantes_2025")
    export_plot(plot_sector_snapshot(sector_snapshot), file.path(figures_snapshot_dir, paste0(public_name("fig_9_sectores_economicos_migrantes_2025"), ".png")), width = 8.6, height = 5.8)
    log_msg("exportando fig_9_brecha_indicadores_2025")
    export_plot(plot_compare_gap(compare_gap), file.path(figures_compare_dir, paste0(public_name("fig_9_brecha_indicadores_2025"), ".png")), width = 8.8, height = 6.2)
    log_msg("exportando fig_10_cambio_indicadores_2018_2025")
    export_plot(plot_compare_change(compare_change), file.path(figures_compare_dir, paste0(public_name("fig_10_cambio_indicadores_2018_2025"), ".png")), width = 9.4, height = 6.4)

    summary_objects <- list(
      snapshot_2025 = bind_rows(snapshot_tables),
      ene_2012_2025 = list(
        share = long_share,
        stock = long_stock,
        sex = long_sex,
        age = long_age,
        region_snapshot = region_snapshot,
        region_change = region_change,
        labor_participation = labor_participation,
        employment_rate = employment_rate,
        region_employment_snapshot = region_employment_snapshot
      ),
      ene_2018_2025 = list(
        series = bind_rows(compare_tables),
        gap_2025 = compare_gap,
        change_2018_2025 = compare_change
      ),
      snapshot_sector_2025 = sector_snapshot
    )
    saveRDS(summary_objects, file.path(final_dir, "ene_figure_objects.rds"))

    invisible(summary_objects)
  }

  panel_path <- file.path(final_dir, "ene_panel.rds")
  objects_path <- file.path(final_dir, "ene_summary_objects.rds")

  if (build %in% c("all", "prepare")) {
    panel <- build_panel()
  }

  if (build %in% c("all", "figures")) {
    if (force_rebuild || !file.exists(panel_path) || !file.exists(objects_path)) {
      panel <- build_panel()
    } else {
      log_msg("usando panel final existente en data/final/ene")
      panel <- readRDS(panel_path)
    }

    build_figures(panel)
  }

  log_msg("pipeline completado")
  invisible(TRUE)
}

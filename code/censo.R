# CENSO
# Fuente de datos: https://censo2024.ine.gob.cl/
# Unidad de observación: persona u hogar censado, según el indicador.
# Espera insumos en `data/raw/censo/` y construye datos procesados y productos públicos.
# Aunque la carpeta publica no distribuye esas bases, aqui queda documentada la logica general del flujo:
# 1. lectura de datos originales;
# 2. procesamiento hacia `data/interim/`;
# 3. construccion de paneles en `data/final/`;
# 4. generacion de tablas espejo;
# 5. generacion de figuras publicables;
# 6. exportación hacia `figures/censo/` y `tables/censo/`.

run_censo <- function(build = c("all", "prepare", "figures"), force_rebuild = FALSE) {
  suppressPackageStartupMessages({
    library(here)
    library(dplyr)
    library(readr)
    library(readxl)
    library(data.table)
    library(ggplot2)
    library(scales)
    library(stringi)
    library(arrow)
    library(tibble)
  })

  build <- match.arg(build)

  set_utf8_locale <- function() {
    for (loc in c("C.UTF-8", "en_US.UTF-8", "es_CL.UTF-8", "es_ES.UTF-8")) {
      out <- try(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)), silent = TRUE)
      if (!inherits(out, "try-error") && !is.na(out)) return(out)
    }
    NA_character_
  }
  set_utf8_locale()

  available_fonts <- tryCatch(systemfonts::system_fonts()$family, error = function(e) character())
  available_fonts <- unique(available_fonts)
  base_family <- dplyr::case_when(
    "Helvetica" %in% available_fonts ~ "Helvetica",
    "Arial" %in% available_fonts ~ "Arial",
    TRUE ~ "sans"
  )

  root <- here::here()
  raw_dir <- file.path(root, "data", "raw", "censo")
  interim_dir <- file.path(root, "data", "interim", "censo")
  final_dir <- file.path(root, "data", "final", "censo")
  fig_root <- file.path(root, "figures", "censo")
  tab_root <- file.path(root, "tables", "censo")

  fig_compare <- fig_root
  fig_2017 <- fig_root
  fig_2024 <- fig_root
  tab_compare <- tab_root
  tab_2017 <- tab_root
  tab_2024 <- tab_root

  for (p in c(interim_dir, final_dir, fig_root, tab_root)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }

  theme_set(theme_minimal(base_family = base_family))

  clean_text <- function(x) {
    x <- trimws(enc2utf8(as.character(x)))
    x <- gsub("\\s+", " ", x)
    x
  }

  utf8_fix <- function(x) {
    if (is.null(x)) return(x)
    if (is.factor(x)) x <- as.character(x)
    if (!is.character(x)) return(x)
    out <- iconv(x, from = "UTF-8", to = "UTF-8")
    out[is.na(out)] <- x[is.na(out)]
    out
  }
  utf8_fix_factor <- function(x) {
    lvls <- levels(x)
    factor(utf8_fix(as.character(x)), levels = utf8_fix(lvls), ordered = is.ordered(x))
  }

  title_case <- function(x) {
    x <- clean_text(x)
    x <- stringi::stri_trans_totitle(stri_trans_tolower(x))
    x <- gsub(" Y ", " y ", x)
    x <- gsub(" De ", " de ", x)
    x <- gsub(" Del ", " del ", x)
    x <- gsub(" La ", " la ", x)
    x <- gsub(" Los ", " los ", x)
    x <- gsub(" Las ", " las ", x)
    x <- gsub(" O'higgins", " O'Higgins", x)
    x <- gsub("Republica Dominicana", "República Dominicana", x, fixed = TRUE)
    x <- gsub("Haiti", "Haití", x, fixed = TRUE)
    x <- gsub("Peru", "Perú", x, fixed = TRUE)
    x <- gsub("Mexico", "México", x, fixed = TRUE)
    x <- gsub("Panama", "Panamá", x, fixed = TRUE)
    x <- gsub("Espana", "España", x, fixed = TRUE)
    x <- gsub("Africa", "África", x, fixed = TRUE)
    x <- gsub("Oceania", "Oceanía", x, fixed = TRUE)
    x
  }
  wrap_label <- function(x, width = 26) {
    vapply(x, function(label) paste(strwrap(label, width = width), collapse = "\n"), character(1))
  }

  pct_fmt <- function(x) scales::label_percent(accuracy = 0.1, decimal.mark = ",")(x)
  num_fmt <- function(x) scales::label_number(big.mark = ".", decimal.mark = ",")(x)

  censo_palette <- list(
    migrant = "#3A7DB6",
    local = "#F28C38",
    accent = "#D64F45",
    olive = "#7D8B52",
    teal = "#4FA3A5",
    grey = "#7A7A7A",
    light = "#F6F1E8"
  )

  region_lookup_tbl <- tibble(
    region = c(15L, 1L, 2L, 3L, 4L, 5L, 13L, 6L, 7L, 16L, 8L, 9L, 14L, 10L, 11L, 12L),
    region_label = c(
      "Arica y Parinacota",
      "Tarapac\u00e1",
      "Antofagasta",
      "Atacama",
      "Coquimbo",
      "Valpara\u00edso",
      "Metropolitana",
      "O'Higgins",
      "Maule",
      "\u00d1uble",
      "Biob\u00edo",
      "La Araucan\u00eda",
      "Los R\u00edos",
      "Los Lagos",
      "Ays\u00e9n",
      "Magallanes"
    )
  )
  region_levels <- region_lookup_tbl$region_label
  region_plot_levels <- wrap_label(region_levels, width = 18)

  age_band_labels <- c(
    `0` = "0-4", `5` = "5-9", `10` = "10-14", `15` = "15-19", `20` = "20-24",
    `25` = "25-29", `30` = "30-34", `35` = "35-39", `40` = "40-44", `45` = "45-49",
    `50` = "50-54", `55` = "55-59", `60` = "60-64", `65` = "65-69", `70` = "70-74",
    `75` = "75-79", `80` = "80-84", `85` = "85+"
  )
  age_band_levels <- unname(age_band_labels)

  age_group_broad <- function(age) {
    dplyr::case_when(
      age < 15 ~ "0 a 14",
      age < 30 ~ "15 a 29",
      age < 45 ~ "30 a 44",
      age < 65 ~ "45 a 64",
      age >= 65 ~ "65 y más",
      TRUE ~ NA_character_
    )
  }

  base_theme <- function() {
    theme_minimal(base_family = base_family) +
      theme(
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        axis.title = element_text(color = "#102a43"),
        axis.text = element_text(color = "#243b53"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#d9e2ec", linewidth = 0.35),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(color = "#243b53"),
        plot.background = element_rect(fill = "white", color = NA)
      )
  }

  save_plot <- function(plot, path, width = 10, height = 6, dpi = 320) {
    plot <- plot + labs(title = NULL, subtitle = NULL, caption = NULL)
    plot$labels <- lapply(plot$labels, utf8_fix)
    if (!is.null(plot$data) && nrow(plot$data) > 0) {
      plot$data <- as.data.frame(plot$data)
      for (nm in names(plot$data)) {
        if (is.factor(plot$data[[nm]])) {
          plot$data[[nm]] <- utf8_fix_factor(plot$data[[nm]])
        } else if (is.character(plot$data[[nm]])) {
          plot$data[[nm]] <- utf8_fix(plot$data[[nm]])
        }
      }
    }
    for (i in seq_along(plot$layers)) {
      layer_data <- plot$layers[[i]]$data
      if (!is.null(layer_data) && !inherits(layer_data, "waiver") && NROW(layer_data) > 0) {
        layer_data <- as.data.frame(layer_data)
        for (nm in names(layer_data)) {
          if (is.factor(layer_data[[nm]])) {
            layer_data[[nm]] <- utf8_fix_factor(layer_data[[nm]])
          } else if (is.character(layer_data[[nm]])) {
            layer_data[[nm]] <- utf8_fix(layer_data[[nm]])
          }
        }
        plot$layers[[i]]$data <- layer_data
      }
    }
    if (requireNamespace("ragg", quietly = TRUE)) {
      ragg::agg_png(path, width = width, height = height, units = "in", res = dpi, scaling = 1)
      print(plot)
      dev.off()
    } else {
      ggsave(path, plot = plot, width = width, height = height, dpi = dpi, bg = "white")
    }
  }

  read_label_file <- function(path) {
    read_delim(
      path,
      delim = ";",
      col_names = c("code", "label"),
      show_col_types = FALSE,
      locale = locale(encoding = "UTF-8")
    ) %>%
      mutate(
        code = suppressWarnings(as.integer(code)),
        label = clean_text(label)
      )
  }

  load_lookups <- function() {
    dict_path <- file.path(raw_dir, "2024", "microdatos", "diccionario_variables_censo2024.xlsx")
    territory <- read_excel(dict_path, sheet = "codigos_territoriales")
    specific <- read_excel(dict_path, sheet = "cod_territoriales_especificos")
    dict_people <- read_excel(dict_path, sheet = "tabla_personas")

    names(territory)[1:3] <- c("code", "division", "label")
    names(specific)[1:2] <- c("code", "label")
    names(dict_people)[2] <- "variable"
    names(dict_people)[4] <- "value"
    names(dict_people)[5] <- "label"

    comuna_lookup <- territory %>%
      transmute(code = as.integer(code), division = clean_text(division), label = clean_text(label)) %>%
      filter(division == "Comuna")

    country_2017 <- read_label_file(file.path(raw_dir, "2017", "personas", "csv-personas-censo-2017", "etiquetas_persona_pais.csv"))
    country_2024 <- specific %>% transmute(code = as.integer(code), label = clean_text(label))
    country_lookup <- bind_rows(country_2017, country_2024) %>%
      filter(!is.na(code), !is.na(label), label != "") %>%
      distinct(code, .keep_all = TRUE)

    arrival_lookup <- dict_people %>%
      filter(variable == "p26_llegada_periodo") %>%
      transmute(code = suppressWarnings(as.integer(value)), label = clean_text(label)) %>%
      filter(!is.na(code), code > 0)

    list(comuna = comuna_lookup, country = country_lookup, arrival = arrival_lookup)
  }

  aggregate_school <- function(df) {
    df %>%
      group_by(year, migrant_status) %>%
      summarise(
        promedio_escolaridad = weighted.mean(years_school, n),
        prop_13mas = sum(n[years_school >= 13], na.rm = TRUE) / sum(n),
        .groups = "drop"
      )
  }

  aggregate_children <- function(df) {
    df %>%
      group_by(year, migrant_status) %>%
      summarise(
        hijos_promedio = weighted.mean(children_ever, n),
        .groups = "drop"
      )
  }

  aggregate_child_chile <- function(df) {
    df %>%
      group_by(year, migrant_status) %>%
      summarise(prop_hijo_chile = weighted.mean(has_child_chile, n), .groups = "drop")
  }

  build_2017_reduced <- function() {
    message("CENSO: leyendo microdato 2017...")
    person_path <- file.path(raw_dir, "2017", "personas", "Microdato_Censo2017-Personas.csv")
    if (!file.exists(person_path)) stop("CENSO 2017: no existe Microdato_Censo2017-Personas.csv")

    cols <- c(
      "REGION", "COMUNA", "AREA", "NVIV", "NHOGAR", "P07", "P08", "P09",
      "P12PAIS", "P12A_LLEGADA", "P19", "P20", "P21A", "ESCOLARIDAD", "P15",
      "REGION_15R", "COMUNA_15R"
    )

    dt <- fread(person_path, sep = ";", select = cols, encoding = "UTF-8", showProgress = FALSE)
    setnames(dt, tolower(names(dt)))

    dt[, region := fifelse(!is.na(region_15r) & region_15r > 0, region_15r, region)]
    dt[, comuna := fifelse(!is.na(comuna_15r) & comuna_15r > 0, comuna_15r, comuna)]
    dt[, age := fifelse(p09 >= 0 & p09 <= 110, p09, NA_real_)]
    dt[, sex := fifelse(p08 == 1, "Hombres", fifelse(p08 == 2, "Mujeres", NA_character_))]
    dt <- dt[!p12pais %in% c(997, 999)]
    dt[, migrant := p12pais != 998]
    dt[, migrant_status := fifelse(migrant, "Nacidos fuera de Chile", "Nacidos en Chile")]
    dt[, years_school := fifelse(escolaridad >= 0 & escolaridad <= 30, as.numeric(escolaridad), NA_real_)]
    dt[, higher_edu := fifelse(p15 >= 11 & p15 <= 14, 1, fifelse(p15 <= 10, 0, NA_real_))]
    dt[, children_ever := fifelse(p20 >= 0 & p20 < 98, as.numeric(p20), NA_real_)]
    dt[, area_label := fifelse(area == 1, "Urbano", fifelse(area == 2, "Rural", NA_character_))]
    dt[, age5 := pmin(floor(age / 5) * 5, 85)]
    dt[, age_group := age_group_broad(age)]

    region_summary <- dt[!is.na(region), .(total_personas = .N, migrantes = sum(migrant, na.rm = TRUE)), by = .(region)]
    region_summary[, `:=`(year = 2017L, share_migrantes = migrantes / total_personas)]

    school_counts <- dt[age >= 25 & age <= 50 & !is.na(years_school), .(n = .N), by = .(migrant_status, years_school)]
    school_counts[, year := 2017L]

    higher_edu_summary <- dt[age >= 25 & age <= 50 & !is.na(higher_edu), .(n = .N), by = .(migrant_status, higher_edu)]
    higher_edu_summary[, year := 2017L]

    child_counts <- dt[p08 == 2 & age >= 18 & age <= 50 & !is.na(children_ever), .(n = .N), by = .(migrant_status, children_ever)]
    child_counts[, year := 2017L]

    child_chile <- copy(dt)
    child_chile <- child_chile[p08 == 2 & age >= 18 & age <= 50]
    child_chile[p12a_llegada == 9998, p12a_llegada := 0]
    child_chile[p12a_llegada == 9999, p12a_llegada := NA]
    child_chile[p21a == 9999, p21a := NA]
    child_chile[, has_child_chile := fifelse(migrant == TRUE & !is.na(p12a_llegada) & !is.na(p21a) & p12a_llegada <= p21a, 1,
                                      fifelse(migrant == FALSE & p19 > 0, 1, 0))]
    child_chile_summary <- child_chile[!is.na(has_child_chile), .(n = .N), by = .(migrant_status, has_child_chile)]
    child_chile_summary[, year := 2017L]

    dt[, has_child_chile := NA_real_]
    dt[p08 == 2 & age >= 18 & age <= 50 & !is.na(p12a_llegada) & !is.na(p21a) & migrant == TRUE, has_child_chile := fifelse(p12a_llegada <= p21a, 1, 0)]
    dt[p08 == 2 & age >= 18 & age <= 50 & migrant == FALSE, has_child_chile := fifelse(p19 > 0, 1, 0)]

    sex_summary <- dt[migrant == TRUE & !is.na(sex), .(n = .N), by = .(sex)]
    sex_summary[, year := 2017L]

    age_group_summary <- dt[migrant == TRUE & !is.na(age_group), .(n = .N), by = .(age_group)]
    age_group_summary[, year := 2017L]

    pyramid <- dt[migrant == TRUE & !is.na(sex) & !is.na(age5), .(n = .N), by = .(sex, age5)]
    pyramid[, year := 2017L]

    area_summary <- dt[!is.na(area_label), .(n = .N), by = .(migrant_status, area_label)]
    area_summary[, year := 2017L]

    hh <- dt[, .(
      has_migrant = as.integer(any(migrant, na.rm = TRUE)),
      head_migrant = as.integer(any(p07 == 1 & migrant, na.rm = TRUE))
    ), by = .(region, comuna, nviv, nhogar)]

    household_summary <- hh[, .(
      hogares = .N,
      prop_hogar_con_migrante = mean(has_migrant, na.rm = TRUE),
      prop_jefatura_migrante = mean(head_migrant, na.rm = TRUE)
    )]
    household_summary[, year := 2017L]

    reduced <- dt[, .(
      year = 2017L, region, comuna, area_label, nviv, nhogar, p07, sex, age, age5, age_group,
      migrant, migrant_status, years_school, higher_edu, children_ever, has_child_chile,
      country_code = p12pais
    )]
    as_tibble(reduced)
  }

  summarise_2017_from_reduced <- function(df) {
    region_summary <- df %>% filter(!is.na(region)) %>% count(region, wt = NULL, name = 'total_personas')
    region_mig <- df %>% filter(!is.na(region), migrant) %>% count(region, name = 'migrantes')
    region_summary <- region_summary %>% left_join(region_mig, by = 'region') %>% mutate(migrantes = dplyr::coalesce(migrantes, 0L), year = 2017L, share_migrantes = migrantes / total_personas)

    school_counts <- df %>% filter(age >= 25, age <= 50, !is.na(years_school)) %>% count(migrant_status, years_school, name = 'n') %>% mutate(year = 2017L)
    higher_edu_summary <- df %>% filter(age >= 25, age <= 50, !is.na(higher_edu)) %>% count(migrant_status, higher_edu, name = 'n') %>% mutate(year = 2017L)
    child_counts <- df %>% filter(sex == 'Mujeres', age >= 18, age <= 50, !is.na(children_ever)) %>% count(migrant_status, children_ever, name = 'n') %>% mutate(year = 2017L)
    child_chile_summary <- df %>% filter(sex == 'Mujeres', age >= 18, age <= 50, !is.na(has_child_chile)) %>% count(migrant_status, has_child_chile, name = 'n') %>% mutate(year = 2017L)
    sex_summary <- df %>% filter(migrant, !is.na(sex)) %>% count(sex, name = 'n') %>% mutate(year = 2017L)
    age_group_summary <- df %>% filter(migrant, !is.na(age_group)) %>% count(age_group, name = 'n') %>% mutate(year = 2017L)
    pyramid <- df %>% filter(migrant, !is.na(sex), !is.na(age5)) %>% count(sex, age5, name = 'n') %>% mutate(year = 2017L)
    area_summary <- df %>% filter(!is.na(area_label)) %>% count(migrant_status, area_label, name = 'n') %>% mutate(year = 2017L)

    household_summary <- df %>% group_by(region, comuna, nviv, nhogar) %>% summarise(has_migrant = as.integer(any(migrant, na.rm = TRUE)), head_migrant = as.integer(any(p07 == 1 & migrant, na.rm = TRUE)), .groups = 'drop') %>% summarise(hogares = n(), prop_hogar_con_migrante = mean(has_migrant, na.rm = TRUE), prop_jefatura_migrante = mean(head_migrant, na.rm = TRUE), .groups = 'drop') %>% mutate(year = 2017L)

    country_summary <- df %>% filter(migrant) %>% count(code = country_code, name = 'n') %>% mutate(year = 2017L)

    list(region = region_summary, school_counts = school_counts, higher_edu = higher_edu_summary, child_counts = child_counts, child_chile = child_chile_summary, sex = sex_summary, age_group = age_group_summary, pyramid = pyramid, area = area_summary, household = household_summary, country = country_summary)
  }

  build_2024_country_from_raw <- function() {
    read_excel(
      file.path(raw_dir, '2024', 'otros', 'D4_Inmigracion-Internacional.xlsx'),
      sheet = '3',
      skip = 3
    ) %>%
      transmute(
        region_code = suppressWarnings(as.integer(.[[1]])),
        country = clean_text(.[[3]]),
        n = suppressWarnings(as.numeric(.[[4]]))
      ) %>%
      filter(region_code == 0, !is.na(n), n > 0) %>%
      filter(!country %in% c(
        'Total nacidos fuera del país',
        'Otros países de América del Sur',
        'Otros países de América Central y el Caribe',
        'América del Norte', 'Europa', 'Asia', 'África', 'Oceanía'
      )) %>%
      transmute(year = 2024L, country = title_case(country), n = n)
  }

  build_2024_reduced <- function() {
    message("CENSO: leyendo microdato 2024...")
    person_path <- file.path(raw_dir, "2024", "microdatos", "viv_hog_per_censo2024", "personas_censo2024.parquet")
    if (!file.exists(person_path)) stop("CENSO 2024: no existe personas_censo2024.parquet")

    ds <- open_dataset(person_path, format = "parquet")

    reduced <- ds %>%
      select(id_vivienda, id_hogar, region, comuna, area, parentesco, sexo, edad, edad_quinquenal, p25_lug_nacimiento_rec, escolaridad, p46a_tot_hijs_nac, p26_llegada_periodo) %>%
      collect() %>%
      transmute(
        year = 2024L, id_vivienda, id_hogar, region, comuna,
        area_label = if_else(area == 1L, 'Urbano', if_else(area == 2L, 'Rural', NA_character_)),
        parentesco,
        sex = if_else(sexo == 1L, 'Hombres', if_else(sexo == 2L, 'Mujeres', NA_character_)),
        age = as.numeric(edad),
        age5 = as.numeric(edad_quinquenal),
        age_group = case_when(edad < 15 ~ '0 a 14', edad < 30 ~ '15 a 29', edad < 45 ~ '30 a 44', edad < 65 ~ '45 a 64', edad >= 65 ~ '65 y más', TRUE ~ NA_character_),
        migrant = p25_lug_nacimiento_rec == 2L,
        migrant_status = if_else(p25_lug_nacimiento_rec == 2L, 'Nacidos fuera de Chile', 'Nacidos en Chile'),
        years_school = if_else(escolaridad >= 0 & escolaridad <= 30, as.numeric(escolaridad), NA_real_),
        higher_edu = if_else(escolaridad >= 13 & escolaridad <= 30, 1, if_else(escolaridad >= 0 & escolaridad < 13, 0, NA_real_)),
        children_ever = if_else(p46a_tot_hijs_nac >= 0 & p46a_tot_hijs_nac < 40, as.numeric(p46a_tot_hijs_nac), NA_real_),
        arrival_code = if_else(p26_llegada_periodo > 0, as.integer(p26_llegada_periodo), NA_integer_)
      )
    as_tibble(reduced)
  }

  summarise_2024_from_reduced <- function(df, country_summary) {
    region_summary <- df %>% filter(!is.na(region)) %>% count(region, name = 'total_personas')
    region_mig <- df %>% filter(!is.na(region), migrant) %>% count(region, name = 'migrantes')
    region_summary <- region_summary %>% left_join(region_mig, by = 'region') %>% mutate(migrantes = dplyr::coalesce(migrantes, 0L), year = 2024L, share_migrantes = migrantes / total_personas)
    commune_summary <- df %>% filter(!is.na(region), !is.na(comuna), migrant) %>% count(region, comuna, name = 'migrantes') %>% mutate(year = 2024L)
    school_counts <- df %>% filter(age >= 25, age <= 50, !is.na(years_school)) %>% count(migrant_status, years_school, name = 'n') %>% mutate(year = 2024L)
    higher_edu_summary <- df %>% filter(age >= 25, age <= 50, !is.na(higher_edu)) %>% count(migrant_status, higher_edu, name = 'n') %>% mutate(year = 2024L)
    child_counts <- df %>% filter(sex == 'Mujeres', age >= 18, age <= 50, !is.na(children_ever)) %>% count(migrant_status, children_ever, name = 'n') %>% mutate(year = 2024L)
    sex_summary <- df %>% filter(migrant, !is.na(sex)) %>% count(sex, name = 'n') %>% mutate(year = 2024L)
    age_group_summary <- df %>% filter(migrant, !is.na(age_group)) %>% count(age_group, name = 'n') %>% mutate(year = 2024L)
    pyramid <- df %>% filter(migrant, !is.na(sex), !is.na(age5)) %>% count(sex, age5, name = 'n') %>% mutate(year = 2024L)
    area_summary <- df %>% filter(!is.na(area_label)) %>% count(migrant_status, area_label, name = 'n') %>% mutate(year = 2024L)
    household_summary <- df %>% group_by(id_vivienda, id_hogar) %>% summarise(has_migrant = as.integer(any(migrant, na.rm = TRUE)), head_migrant = as.integer(any(parentesco == 1L & migrant, na.rm = TRUE)), .groups = 'drop') %>% summarise(hogares = n(), prop_hogar_con_migrante = mean(has_migrant, na.rm = TRUE), prop_jefatura_migrante = mean(head_migrant, na.rm = TRUE), .groups = 'drop') %>% mutate(year = 2024L)
    arrival_summary <- df %>% filter(migrant, !is.na(arrival_code)) %>% count(code = arrival_code, name = 'n')
    list(region = region_summary, comuna = commune_summary, school_counts = school_counts, higher_edu = higher_edu_summary, child_counts = child_counts, sex = sex_summary, age_group = age_group_summary, pyramid = pyramid, area = area_summary, household = household_summary, country = country_summary, arrival = arrival_summary)
  }

  build_summaries <- function() {
    summary_objects_path <- file.path(final_dir, "censo_summary_objects.rds")
    if (file.exists(summary_objects_path) && !force_rebuild) {
      return(readRDS(summary_objects_path))
    }

    lookups_path <- file.path(interim_dir, "censo_lookups.rds")
    country_2024_path <- file.path(interim_dir, "censo_2024_country_summary.rds")
    interim_2017_reduced <- file.path(interim_dir, "censo_2017_personas_reducido.parquet")
    interim_2024_reduced <- file.path(interim_dir, "censo_2024_personas_reducido.parquet")
    interim_2017 <- file.path(interim_dir, "censo_2017_summary.rds")
    interim_2024 <- file.path(interim_dir, "censo_2024_summary.rds")

    raw_available <- dir.exists(raw_dir) && length(list.files(raw_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)) > 0

    if (file.exists(lookups_path)) {
      lookups <- readRDS(lookups_path)
    } else {
      if (!raw_available) stop("CENSO: faltan censo_lookups.rds y raw no está disponible.")
      lookups <- load_lookups()
      saveRDS(lookups, lookups_path)
    }

    if (file.exists(country_2024_path)) {
      country_2024 <- readRDS(country_2024_path)
    } else {
      if (!raw_available) stop("CENSO: falta censo_2024_country_summary.rds y raw no está disponible.")
      country_2024 <- build_2024_country_from_raw()
      saveRDS(country_2024, country_2024_path)
    }

    d2017 <- if (file.exists(interim_2017_reduced)) {
      arrow::read_parquet(interim_2017_reduced, as_data_frame = TRUE)
    } else {
      if (!raw_available) stop("CENSO: falta censo_2017_personas_reducido.parquet y raw no está disponible.")
      tmp <- build_2017_reduced(); arrow::write_parquet(tmp, interim_2017_reduced); tmp
    }

    d2024 <- if (file.exists(interim_2024_reduced)) {
      arrow::read_parquet(interim_2024_reduced, as_data_frame = TRUE)
    } else {
      if (!raw_available) stop("CENSO: falta censo_2024_personas_reducido.parquet y raw no está disponible.")
      tmp <- build_2024_reduced(); arrow::write_parquet(tmp, interim_2024_reduced); tmp
    }

    s2017 <- if (file.exists(interim_2017)) {
      readRDS(interim_2017)
    } else {
      tmp <- summarise_2017_from_reduced(d2017); saveRDS(tmp, interim_2017); tmp
    }

    s2024 <- if (file.exists(interim_2024)) {
      readRDS(interim_2024)
    } else {
      tmp <- summarise_2024_from_reduced(d2024, country_2024); saveRDS(tmp, interim_2024); tmp
    }

    school_summary <- aggregate_school(bind_rows(s2017$school_counts, s2024$school_counts))
    child_summary <- aggregate_children(bind_rows(s2017$child_counts, s2024$child_counts))
    child_chile_2017 <- aggregate_child_chile(s2017$child_chile)

    higher_edu_compare <- bind_rows(s2017$higher_edu, s2024$higher_edu) %>%
      group_by(year, migrant_status) %>%
      summarise(prop_superior = sum(n[higher_edu == 1], na.rm = TRUE) / sum(n), .groups = "drop")

    region_all <- bind_rows(s2017$region, s2024$region) %>% left_join(region_lookup_tbl, by = "region")
    comuna_2024 <- s2024$comuna %>%
      left_join(lookups$comuna %>% select(code, comuna_label = label), by = c("comuna" = "code")) %>%
      left_join(region_lookup_tbl, by = "region")

    excluded_country_codes <- c(2, 5, 9, 10, 13, 21, 29, 142, 150, 997, 998, 999)
    country_all <- bind_rows(s2017$country, s2024$country) %>%
      left_join(lookups$country, by = "code") %>%
      mutate(country = dplyr::coalesce(country, title_case(if_else(is.na(label), paste("Codigo", code), label)))) %>%
      filter(is.na(code) | !code %in% excluded_country_codes) %>%
      filter(!is.na(country), country != "NA") %>%
      filter(!startsWith(country, "Total"), !startsWith(country, "Otros P"))

    top_compare <- country_all %>% group_by(country) %>% summarise(total = sum(n), .groups = "drop") %>% arrange(desc(total)) %>% slice_head(n = 8) %>% pull(country)
    country_compare <- country_all %>%
      mutate(country_group = if_else(country %in% top_compare, country, "Otros países")) %>%
      group_by(year, country_group) %>%
      summarise(n = sum(n), .groups = "drop")
    country_compare_order <- country_compare %>% filter(year == 2024) %>% arrange(desc(n)) %>% pull(country_group) %>% unique()
    country_compare$country_group <- factor(country_compare$country_group, levels = rev(c(setdiff(country_compare_order, "Otros países"), "Otros países")))

    country_2017 <- country_all %>% filter(year == 2017) %>% arrange(desc(n)) %>% slice_head(n = 12) %>% mutate(country = factor(country, levels = rev(country)))
    country_2024 <- country_all %>% filter(year == 2024) %>% arrange(desc(n)) %>% slice_head(n = 12) %>% mutate(country = factor(country, levels = rev(country)))

    arrival_2024 <- s2024$arrival %>%
      left_join(lookups$arrival, by = "code") %>%
      filter(!is.na(label)) %>%
      mutate(
        label = title_case(label),
        label = case_when(
          label == "Antes De 1990" ~ "Antes de 1990",
          label == "Entre 1990 Y 1999" ~ "Entre 1990 y 1999",
          label == "Entre 2000 Y 2009" ~ "Entre 2000 y 2009",
          label == "Entre 2010 Y 2013" ~ "Entre 2010 y 2013",
          label == "Entre 2014 Y 2016" ~ "Entre 2014 y 2016",
          label == "Entre 2017 Y 2019" ~ "Entre 2017 y 2019",
          label == "Entre 2020 Y 2022" ~ "Entre 2020 y 2022",
          label == "Entre 2023 Y 2024" ~ "Entre 2023 y 2024",
          TRUE ~ label
        ),
        order_id = match(
          label,
          c(
            "Antes de 1990",
            "Entre 1990 y 1999",
            "Entre 2000 y 2009",
            "Entre 2010 y 2013",
            "Entre 2014 y 2016",
            "Entre 2017 y 2019",
            "Entre 2020 y 2022",
            "Entre 2023 y 2024"
          )
        )
      ) %>%
      arrange(order_id)

    area_compare <- bind_rows(s2017$area, s2024$area) %>%
      group_by(year, migrant_status) %>%
      mutate(share = n / sum(n)) %>%
      ungroup()

    sex_compare <- bind_rows(s2017$sex, s2024$sex) %>%
      group_by(year) %>%
      mutate(share = n / sum(n)) %>%
      ungroup()

    age_group_compare <- bind_rows(s2017$age_group, s2024$age_group) %>%
      group_by(year) %>%
      mutate(share = n / sum(n)) %>%
      ungroup()

    snapshot_2017 <- list(
      region = s2017$region %>% left_join(region_lookup_tbl, by = "region"),
      country = country_2017,
      pyramid = s2017$pyramid,
      sex = s2017$sex %>% mutate(share = n / sum(n)),
      age_group = s2017$age_group %>% mutate(share = n / sum(n)),
      child_chile = child_chile_2017,
      school = school_summary %>% filter(year == 2017),
      higher = higher_edu_compare %>% filter(year == 2017)
    )

    snapshot_2024 <- list(
      region = s2024$region %>% left_join(region_lookup_tbl, by = "region"),
      comuna = comuna_2024,
      country = country_2024,
      pyramid = s2024$pyramid,
      sex = s2024$sex %>% mutate(share = n / sum(n)),
      age_group = s2024$age_group %>% mutate(share = n / sum(n)),
      arrival = arrival_2024,
      school = school_summary %>% filter(year == 2024),
      higher = higher_edu_compare %>% filter(year == 2024)
    )

    compare <- list(
      region = region_all,
      school = school_summary,
      higher = higher_edu_compare,
      household = bind_rows(s2017$household, s2024$household),
      children = child_summary,
      country = country_compare,
      area = area_compare,
      sex = sex_compare,
      age_group = age_group_compare
    )

    out <- list(compare = compare, snapshot_2017 = snapshot_2017, snapshot_2024 = snapshot_2024)
    saveRDS(out, file.path(final_dir, "censo_summary_objects.rds"))
    write_rds(region_all, file.path(final_dir, "censo_region_summary.rds"))
    write_rds(comuna_2024, file.path(final_dir, "censo_comuna_2024_summary.rds"))
    write_rds(school_summary, file.path(final_dir, "censo_school_summary.rds"))
    write_rds(compare$household, file.path(final_dir, "censo_household_summary.rds"))
    write_rds(country_compare, file.path(final_dir, "censo_country_compare.rds"))
    write_rds(arrival_2024, file.path(final_dir, "censo_arrival_2024.rds"))
    out
  }

  plot_pyramid <- function(df, title, subtitle, out_path) {
    x <- df %>%
      group_by(year) %>%
      mutate(share = n / sum(n), value = if_else(sex == "Hombres", -share, share)) %>%
      ungroup() %>%
      mutate(age_band = factor(age_band_labels[as.character(age5)], levels = rev(age_band_levels), ordered = TRUE))

    p <- ggplot(x, aes(x = value, y = age_band, fill = sex)) +
      geom_col(width = 0.88) +
      scale_fill_manual(values = c("Hombres" = censo_palette$migrant, "Mujeres" = censo_palette$accent)) +
      scale_x_continuous(labels = function(v) pct_fmt(abs(v)), expand = expansion(mult = c(0.05, 0.05))) +
      labs(title = title, subtitle = subtitle, x = "Porcentaje de la población migrante", y = NULL, caption = "Fuente: Elaboración propia con microdatos oficiales del Censo.") +
      base_theme()
    save_plot(p, out_path, width = 9, height = 6.4)
  }

  plot_barh <- function(df, x, y, fill = NULL, title, subtitle = NULL, xlab = NULL, caption = NULL, out_path, width = 10, height = 6.5, label_fun = NULL, dodge = FALSE) {
    aes_base <- aes(x = {{ x }}, y = {{ y }})
    if (!is.null(fill)) aes_base$fill <- rlang::enquo(fill)
    pos <- if (dodge) position_dodge(width = 0.75) else "identity"
    p <- ggplot(df, aes_base) +
      geom_col(position = pos, width = 0.72) +
      labs(title = title, subtitle = subtitle, x = xlab, y = NULL, caption = caption) +
      base_theme()
    if (!is.null(fill)) p <- p + scale_fill_manual(values = c(censo_palette$grey, censo_palette$migrant, censo_palette$local, censo_palette$accent, censo_palette$olive, censo_palette$teal))
    if (!is.null(label_fun)) p <- p + scale_x_continuous(labels = label_fun)
    save_plot(p, out_path, width = width, height = height)
  }

  make_figures <- function(obj) {
    cmp <- obj$compare
    s17 <- obj$snapshot_2017
    s24 <- obj$snapshot_2024

    clean_country_label <- function(x) {
      x <- enc2utf8(as.character(x))
      y <- iconv(x, from = '', to = 'UTF-8', sub = '')
      y[is.na(y)] <- x[is.na(y)]
      y[grepl("<c3><ad>", y, fixed = TRUE)] <- "Otros pa\u00edses"
      y <- gsub("Otros paises", "Otros pa\u00edses", y, fixed = TRUE)
      y <- gsub("Pais de nacimiento no declarado", "Pa\u00eds de nacimiento no declarado", y, fixed = TRUE)
      y <- gsub("America del Norte", "Am\u00e9rica del Norte", y, fixed = TRUE)
      y <- gsub("Africa", "\u00c1frica", y, fixed = TRUE)
      y <- gsub("Oceania", "Ocean\u00eda", y, fixed = TRUE)
      y[grepl('Venezuela', y, fixed = TRUE)] <- 'Venezuela'
      y[grepl('Bolivia', y, fixed = TRUE)] <- 'Bolivia'
      y
    }

    normalize_ascii <- function(x) {
      x <- enc2utf8(as.character(x))
      y <- iconv(x, from = '', to = 'UTF-8', sub = '')
      y[is.na(y)] <- x[is.na(y)]
      tolower(iconv(y, from = 'UTF-8', to = 'ASCII//TRANSLIT', sub = ''))
    }
    is_residual_country <- function(x) {
      ascii <- normalize_ascii(x)
      grepl("^(otros|otros paises|pais de nacimiento no declarado|no informa|sin informacion|resto)$", ascii) |
        grepl("^(america del norte|africa|oceania|europa|asia)$", ascii) |
        grepl("otros|norte|frica|cean|declarado|sin inform|no inform|resto", x, ignore.case = TRUE)
    }
    build_country_ranking <- function(df, country_col = "country", value_col = "n", top_n = 9L, other_label = "Otros países") {
      country_sym <- rlang::sym(country_col)
      value_sym <- rlang::sym(value_col)

      ranked <- df %>%
        mutate(country_clean = clean_country_label(as.character(!!country_sym))) %>%
        group_by(country_clean) %>%
        summarise(n = sum(!!value_sym, na.rm = TRUE), .groups = "drop") %>%
        mutate(is_residual = is_residual_country(country_clean))

      top_main <- ranked %>%
        filter(!is_residual) %>%
        arrange(desc(n)) %>%
        slice_head(n = top_n)

      other_total <- ranked %>%
        filter(is_residual | !country_clean %in% top_main$country_clean) %>%
        summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
        pull(n)

      out <- top_main %>%
        transmute(country = country_clean, n = n, is_residual = FALSE)

      if (length(other_total) == 1 && !is.na(other_total) && other_total > 0) {
        out <- bind_rows(out, tibble(country = other_label, n = other_total, is_residual = TRUE))
      }

      out %>%
        mutate(country = clean_country_label(country)) %>%
        arrange(is_residual, desc(n)) %>%
        mutate(country = factor(country, levels = rev(country)))
    }

    cmp_region <- merge(
      expand.grid(region = region_lookup_tbl$region, year = c(2017L, 2024L)),
      cmp$region %>% select(-any_of('region_label')),
      by = c('region', 'year'),
      all.x = TRUE
    ) %>%
      as_tibble() %>%
      left_join(region_lookup_tbl, by = 'region') %>%
      mutate(
        total_personas = dplyr::coalesce(total_personas, 0),
        migrantes = dplyr::coalesce(migrantes, 0),
        share_migrantes = dplyr::coalesce(share_migrantes, 0),
        region_plot = factor(wrap_label(region_label, width = 18), levels = rev(region_plot_levels))
      )
    p1 <- ggplot(cmp_region, aes(x = migrantes, y = region_plot, fill = factor(year))) +
      geom_col(position = position_dodge(width = 0.75), width = 0.68) +
      scale_fill_manual(values = c(`2017` = censo_palette$grey, `2024` = censo_palette$migrant)) +
      scale_x_continuous(labels = num_fmt) +
      scale_y_discrete(limits = rev(region_plot_levels), drop = FALSE) +
      labs(title = "Figura 1. Población nacida fuera de Chile por región", subtitle = "Comparación entre Censo 2017 y Censo 2024", x = "Personas", y = NULL, caption = "Fuente: Elaboración propia con microdatos oficiales del Censo.") +
      base_theme()
    save_plot(p1, file.path(fig_compare, "figura_01_region_migrantes_2017_2024.png"), 10, 7)
    write_csv(cmp_region, file.path(tab_compare, "figura_01_region_migrantes_2017_2024.csv"))

    p2 <- ggplot(cmp_region, aes(x = share_migrantes, y = region_plot, fill = factor(year))) +
      geom_col(position = position_dodge(width = 0.75), width = 0.68) +
      scale_fill_manual(values = c(`2017` = censo_palette$grey, `2024` = censo_palette$accent)) +
      scale_x_continuous(labels = pct_fmt) +
      scale_y_discrete(limits = rev(region_plot_levels), drop = FALSE) +
      labs(title = "Figura 2. Participación regional de población nacida fuera de Chile", subtitle = "Porcentaje de la población regional", x = "Porcentaje", y = NULL, caption = "Fuente: Elaboración propia con microdatos oficiales del Censo.") +
      base_theme()
    save_plot(p2, file.path(fig_compare, "figura_02_share_migrantes_region_2017_2024.png"), 10, 7)
    write_csv(cmp_region %>% select(year, region, region_label, share_migrantes), file.path(tab_compare, "figura_02_share_migrantes_region_2017_2024.csv"))

    school_plot <- cmp$school %>% mutate(year = factor(year), migrant_status = factor(migrant_status, levels = c("Nacidos en Chile", "Nacidos fuera de Chile")))
    p3 <- ggplot(school_plot, aes(x = year, y = promedio_escolaridad, fill = migrant_status)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_text(aes(label = number(promedio_escolaridad, accuracy = 0.1, decimal.mark = ",")), position = position_dodge(width = 0.7), vjust = -0.35, size = 3.2, color = "#425466") +
      scale_fill_manual(values = c("Nacidos en Chile" = censo_palette$local, "Nacidos fuera de Chile" = censo_palette$migrant)) +
      labs(title = "Figura 3. Años promedio de escolaridad en población de 25 a 50 años", subtitle = "Comparación por lugar de nacimiento", x = NULL, y = "Años", caption = "Fuente: Elaboración propia con microdatos oficiales del Censo.") +
      base_theme()
    save_plot(p3, file.path(fig_compare, "figura_12_escolaridad_promedio_25_50.png"), 8.8, 6)
    write_csv(cmp$school, file.path(tab_compare, "figura_12_escolaridad_promedio_25_50.csv"))

    higher_plot <- cmp$higher %>% mutate(year = factor(year), migrant_status = factor(migrant_status, levels = c("Nacidos en Chile", "Nacidos fuera de Chile")))
    p4 <- ggplot(higher_plot, aes(x = year, y = prop_superior, fill = migrant_status)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_text(aes(label = pct_fmt(prop_superior)), position = position_dodge(width = 0.7), vjust = -0.35, size = 3.2, color = "#425466") +
      scale_fill_manual(values = c("Nacidos en Chile" = censo_palette$local, "Nacidos fuera de Chile" = censo_palette$migrant)) +
      scale_y_continuous(labels = pct_fmt, expand = expansion(mult = c(0, 0.08))) +
      labs(title = "Figura 4. Personas de 25 a 50 años con educación superior", subtitle = "Comparación por lugar de nacimiento", x = NULL, y = "Porcentaje", caption = "Fuente: Elaboración propia con microdatos oficiales del Censo.") +
      base_theme()
    save_plot(p4, file.path(fig_compare, "figura_13_educacion_superior_25_50.png"), 8.8, 6)
    write_csv(cmp$higher, file.path(tab_compare, "figura_13_educacion_superior_25_50.csv"))

    hh_plot <- cmp$household %>% mutate(year = factor(year))
    p5 <- ggplot(hh_plot, aes(x = year, y = prop_hogar_con_migrante)) +
      geom_col(width = 0.56, fill = censo_palette$migrant) +
      geom_text(aes(label = pct_fmt(prop_hogar_con_migrante)), vjust = -0.35, size = 3.2, color = "#425466") +
      scale_y_continuous(labels = pct_fmt, expand = expansion(mult = c(0, 0.08))) +
      labs(title = "Figura 5. Hogares con al menos una persona nacida fuera de Chile", subtitle = "Comparación entre 2017 y 2024", x = NULL, y = "Porcentaje", caption = "Fuente: Elaboración propia con microdatos oficiales del Censo.") +
      base_theme()
    save_plot(p5, file.path(fig_compare, "figura_16_hogares_con_migrantes.png"), 7, 5.5)
    write_csv(cmp$household, file.path(tab_compare, "figura_16_hogares_con_migrantes.csv"))

    p6 <- ggplot(hh_plot, aes(x = year, y = prop_jefatura_migrante)) +
      geom_col(width = 0.56, fill = censo_palette$accent) +
      geom_text(aes(label = pct_fmt(prop_jefatura_migrante)), vjust = -0.35, size = 3.2, color = "#425466") +
      scale_y_continuous(labels = pct_fmt, expand = expansion(mult = c(0, 0.08))) +
      labs(title = "Figura 6. Hogares con jefatura nacida fuera de Chile", subtitle = "Comparación entre 2017 y 2024", x = NULL, y = "Porcentaje", caption = "Fuente: Elaboración propia con microdatos oficiales del Censo.") +
      base_theme()
    save_plot(p6, file.path(fig_compare, "figura_17_jefatura_hogar_migrante.png"), 7, 5.5)
    write_csv(cmp$household, file.path(tab_compare, "figura_17_jefatura_hogar_migrante.csv"))

    child_plot <- cmp$children %>% mutate(year = factor(year), migrant_status = factor(migrant_status, levels = c("Nacidos en Chile", "Nacidos fuera de Chile")))
    p7 <- ggplot(child_plot, aes(x = year, y = hijos_promedio, fill = migrant_status)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_text(aes(label = number(hijos_promedio, accuracy = 0.1, decimal.mark = ",")), position = position_dodge(width = 0.7), vjust = -0.35, size = 3.2, color = "#425466") +
      scale_fill_manual(values = c("Nacidos en Chile" = censo_palette$local, "Nacidos fuera de Chile" = censo_palette$migrant)) +
      labs(title = "Figura 7. Número promedio de hijos nacidos vivos en mujeres de 18 a 50 años", subtitle = "Comparación por lugar de nacimiento", x = NULL, y = "Promedio de hijos", caption = "Fuente: Elaboración propia con microdatos oficiales del Censo.") +
      base_theme()
    save_plot(p7, file.path(fig_compare, "figura_18_hijos_promedio_mujeres_18_50.png"), 8.8, 6)
    write_csv(cmp$children, file.path(tab_compare, "figura_18_hijos_promedio_mujeres_18_50.csv"))

    cmp_country_plot <- cmp$country %>%
      filter(!is.na(country_group)) %>%
      mutate(country_group = clean_country_label(as.character(country_group))) %>%
      group_by(year, country_group) %>%
      summarise(n = sum(n), .groups = 'drop')
    country_order_cmp <- cmp_country_plot %>%
      group_by(country_group) %>%
      summarise(total = sum(n), .groups = 'drop') %>%
      mutate(is_other = is_residual_country(country_group)) %>%
      arrange(is_other, desc(total)) %>%
      pull(country_group)
    cmp_country_plot <- cmp_country_plot %>%
      mutate(country_group = factor(country_group, levels = rev(country_order_cmp)))
    p8 <- ggplot(cmp_country_plot, aes(x = n, y = country_group, fill = factor(year))) +
      geom_col(position = position_dodge(width = 0.75), width = 0.68) +
      scale_fill_manual(values = c(`2017` = censo_palette$grey, `2024` = censo_palette$migrant)) +
      scale_x_continuous(labels = num_fmt) +
      labs(title = "Figura 8. Principales países de nacimiento de la población migrante", subtitle = "Comparación entre 2017 y 2024", x = "Personas", y = NULL, caption = "Fuente: Elaboración propia con microdatos oficiales del Censo.") +
      base_theme()
    save_plot(p8, file.path(fig_compare, "figura_21_principales_paises_nacimiento.png"), 10, 6.8)
    write_csv(cmp_country_plot %>% arrange(year, desc(n)), file.path(tab_compare, "figura_21_principales_paises_nacimiento.csv"))

    area_plot <- cmp$area %>% mutate(year = factor(year), area_label = factor(area_label, levels = c("Urbano", "Rural")), migrant_status = factor(migrant_status, levels = c("Nacidos en Chile", "Nacidos fuera de Chile")))
    p9 <- ggplot(area_plot, aes(x = year, y = share, fill = area_label)) +
      geom_col(position = "fill", width = 0.6) +
      geom_text(aes(label = pct_fmt(share)), position = position_fill(vjust = 0.5), size = 3, color = 'white') +
      facet_wrap(~migrant_status) +
      scale_fill_manual(values = c("Urbano" = censo_palette$migrant, "Rural" = censo_palette$olive)) +
      scale_y_continuous(labels = pct_fmt) +
      labs(title = "Figura 9. Distribución urbano-rural por lugar de nacimiento", subtitle = "Comparación entre 2017 y 2024", x = NULL, y = "Porcentaje", caption = "Fuente: Elaboración propia con microdatos oficiales del Censo.") +
      base_theme()
    save_plot(p9, file.path(fig_compare, "figura_20_urbano_rural_2017_2024.png"), 9.5, 5.8)
    write_csv(cmp$area, file.path(tab_compare, "figura_20_urbano_rural_2017_2024.csv"))

    sex_cmp <- cmp$sex %>% mutate(year = factor(year))
    p10 <- ggplot(sex_cmp, aes(x = year, y = share, fill = sex)) +
      geom_col(position = "fill", width = 0.58) +
      geom_text(aes(label = pct_fmt(share)), position = position_fill(vjust = 0.5), size = 3, color = 'white') +
      scale_fill_manual(values = c("Hombres" = censo_palette$migrant, "Mujeres" = censo_palette$accent)) +
      scale_y_continuous(labels = pct_fmt) +
      labs(title = "Figura 10. Composición por sexo de la población migrante", subtitle = "Comparación entre 2017 y 2024", x = NULL, y = "Porcentaje", caption = "Fuente: Elaboración propia con microdatos oficiales del Censo.") +
      base_theme()
    save_plot(p10, file.path(fig_compare, "figura_10_sexo_migrantes_2017_2024.png"), 7.5, 5.5)
    write_csv(cmp$sex, file.path(tab_compare, "figura_10_sexo_migrantes_2017_2024.csv"))

    age_cmp <- cmp$age_group %>% mutate(year = factor(year), age_group = factor(age_group, levels = c("0 a 14", "15 a 29", "30 a 44", "45 a 64", "65 y más")))
    p11 <- ggplot(age_cmp, aes(x = year, y = share, fill = age_group)) +
      geom_col(position = "fill", width = 0.58) +
      geom_text(aes(label = pct_fmt(share)), position = position_fill(vjust = 0.5), size = 2.8, color = 'white') +
      scale_fill_manual(values = c("0 a 14" = censo_palette$olive, "15 a 29" = censo_palette$teal, "30 a 44" = censo_palette$migrant, "45 a 64" = censo_palette$local, "65 y más" = censo_palette$accent)) +
      scale_y_continuous(labels = pct_fmt) +
      labs(title = "Figura 11. Estructura etaria agregada de la población migrante", subtitle = "Comparación entre 2017 y 2024", x = NULL, y = "Porcentaje", caption = "Fuente: Elaboración propia con microdatos oficiales del Censo.") +
      base_theme()
    save_plot(p11, file.path(fig_compare, "figura_11_estructura_etaria_migrante_2017_2024.png"), 8.5, 5.8)
    write_csv(cmp$age_group, file.path(tab_compare, "figura_11_estructura_etaria_migrante_2017_2024.csv"))

    cmp_region_change <- cmp_region %>%
      select(year, region, region_label, share_migrantes) %>%
      arrange(match(region, region_lookup_tbl$region)) %>%
      mutate(region_label = as.character(region_label)) %>%
      tidyr::pivot_wider(names_from = year, values_from = share_migrantes, names_prefix = "share_") %>%
      mutate(
        change_pp = 100 * (`share_2024` - `share_2017`),
        region_plot = factor(wrap_label(region_label, width = 18), levels = rev(region_plot_levels))
      )
    p12 <- ggplot(cmp_region_change, aes(y = region_plot)) +
      geom_segment(aes(x = `share_2017`, xend = `share_2024`, yend = region_plot), colour = "#B9C2CC", linewidth = 1.1) +
      geom_point(aes(x = `share_2017`), colour = censo_palette$grey, size = 2.4) +
      geom_point(aes(x = `share_2024`), colour = censo_palette$accent, size = 2.8) +
      scale_x_continuous(labels = pct_fmt) +
      labs(title = "Figura 12. Cambio en la participación migrante por región", subtitle = "Comparación entre Censo 2017 y Censo 2024, ordenada de norte a sur", x = "Porcentaje de la población regional", y = NULL, caption = "Fuente: Elaboración propia con microdatos oficiales del Censo.") +
      base_theme()
    save_plot(p12, file.path(fig_compare, "figura_03_cambio_share_region_2017_2024.png"), 10, 7)
    write_csv(cmp_region_change, file.path(tab_compare, "figura_03_cambio_share_region_2017_2024.csv"))

    s17_region <- region_lookup_tbl %>%
      left_join(s17$region %>% select(-any_of('region_label')), by = 'region') %>%
      mutate(migrantes = dplyr::coalesce(migrantes, 0), region_plot = factor(wrap_label(region_label, width = 18), levels = rev(region_plot_levels)))

    plot_pyramid(s17$pyramid, "Pirámide poblacional de la población migrante", "Censo 2017", file.path(fig_2017, "figura_24_piramide_migrante_2017.png"))
    write_csv(s17$pyramid, file.path(tab_2017, "figura_24_piramide_migrante_2017.csv"))

    s17_country_plot <- build_country_ranking(s17$country, top_n = 9L)
    p17_2 <- ggplot(s17_country_plot, aes(x = n, y = country)) +
      geom_col(width = 0.72, show.legend = FALSE, fill = censo_palette$migrant) + scale_x_continuous(labels = num_fmt) +
      labs(title = "Figura 2. Principales países de nacimiento", subtitle = "Población nacida fuera de Chile, Censo 2017", x = "Personas", y = NULL, caption = "Fuente: Elaboración propia con microdatos oficiales del Censo 2017.") + base_theme()
    save_plot(p17_2, file.path(fig_2017, "figura_22_principales_paises_2017.png"), 9.8, 6.8)
    write_csv(s17_country_plot, file.path(tab_2017, "figura_22_principales_paises_2017.csv"))

    p17_3 <- ggplot(s17$child_chile %>% mutate(migrant_status = factor(migrant_status, levels = c("Nacidos en Chile", "Nacidos fuera de Chile"))), aes(x = migrant_status, y = prop_hijo_chile, fill = migrant_status)) +
      geom_col(width = 0.58) + geom_text(aes(label = pct_fmt(prop_hijo_chile)), vjust = -0.35, size = 3.2, color = "#425466") +
      scale_fill_manual(values = c("Nacidos en Chile" = censo_palette$local, "Nacidos fuera de Chile" = censo_palette$migrant)) + scale_y_continuous(labels = pct_fmt, expand = expansion(mult = c(0, 0.08))) +
      labs(title = "Figura 3. Mujeres de 18 a 50 años con al menos un hijo nacido en Chile", subtitle = "Censo 2017", x = NULL, y = "Porcentaje", caption = "Fuente: Elaboración propia con microdatos oficiales del Censo 2017.") + base_theme()
    save_plot(p17_3, file.path(fig_2017, "figura_19_hijo_nacido_en_chile_2017.png"), 8.2, 5.8)
    write_csv(s17$child_chile, file.path(tab_2017, "figura_19_hijo_nacido_en_chile_2017.csv"))

    p17_4 <- ggplot(s17$school %>% mutate(migrant_status = factor(migrant_status, levels = c("Nacidos en Chile", "Nacidos fuera de Chile"))), aes(x = migrant_status, y = promedio_escolaridad, fill = migrant_status)) +
      geom_col(width = 0.58) + geom_text(aes(label = number(promedio_escolaridad, accuracy = 0.1, decimal.mark = ",")), vjust = -0.35, size = 3.2, color = "#425466") +
      scale_fill_manual(values = c("Nacidos en Chile" = censo_palette$local, "Nacidos fuera de Chile" = censo_palette$migrant)) +
      labs(title = "Figura 4. Años promedio de escolaridad en población de 25 a 50 años", subtitle = "Censo 2017", x = NULL, y = "Años", caption = "Fuente: Elaboración propia con microdatos oficiales del Censo 2017.") + base_theme()
    save_plot(p17_4, file.path(fig_2017, "figura_25_escolaridad_25_50_2017.png"), 8.2, 5.8)
    write_csv(s17$school, file.path(tab_2017, "figura_25_escolaridad_25_50_2017.csv"))

    p17_5 <- ggplot(s17$higher %>% mutate(migrant_status = factor(migrant_status, levels = c("Nacidos en Chile", "Nacidos fuera de Chile"))), aes(x = migrant_status, y = prop_superior, fill = migrant_status)) +
      geom_col(width = 0.58) + geom_text(aes(label = pct_fmt(prop_superior)), vjust = -0.35, size = 3.2, color = "#425466") +
      scale_fill_manual(values = c("Nacidos en Chile" = censo_palette$local, "Nacidos fuera de Chile" = censo_palette$migrant)) + scale_y_continuous(labels = pct_fmt, expand = expansion(mult = c(0, 0.08))) +
      labs(title = "Figura 5. Población de 25 a 50 años con educación superior", subtitle = "Censo 2017", x = NULL, y = "Porcentaje", caption = "Fuente: Elaboración propia con microdatos oficiales del Censo 2017.") + base_theme()
    save_plot(p17_5, file.path(fig_2017, "figura_26_educacion_superior_2017.png"), 8.2, 5.8)
    write_csv(s17$higher, file.path(tab_2017, "figura_26_educacion_superior_2017.csv"))

    s24_region <- region_lookup_tbl %>%
      left_join(s24$region %>% select(-any_of('region_label')), by = 'region') %>%
      mutate(migrantes = dplyr::coalesce(migrantes, 0), region_plot = factor(wrap_label(region_label, width = 18), levels = rev(region_plot_levels)))
    p24_1 <- ggplot(s24_region, aes(x = migrantes, y = region_plot)) + geom_col(fill = censo_palette$migrant, width = 0.72) + scale_x_continuous(labels = num_fmt) + scale_y_discrete(limits = rev(region_plot_levels), drop = FALSE) + labs(title = "Figura 1. Población nacida fuera de Chile por región", subtitle = "Censo 2024", x = "Personas", y = NULL, caption = "Fuente: Elaboración propia con microdatos oficiales del Censo 2024.") + base_theme()
    save_plot(p24_1, file.path(fig_2024, "figura_04_region_migrantes_2024.png"), 10, 7)
    write_csv(s24_region, file.path(tab_2024, "figura_04_region_migrantes_2024.csv"))

    comuna_plot <- s24$comuna %>%
      filter(!is.na(comuna_label)) %>%
      left_join(region_lookup_tbl %>% select(region, region_display = region_label), by = "region") %>%
      mutate(region_label = dplyr::coalesce(region_display, region_label)) %>%
      mutate(region_short = if_else(region_label == 'Metropolitana', 'RM', region_label)) %>%
      arrange(desc(migrantes)) %>%
      slice_head(n = 15) %>%
      mutate(label_txt = paste0(comuna_label, ' (', region_short, ')'), label = factor(label_txt, levels = rev(label_txt)))
    p24_2 <- ggplot(comuna_plot, aes(x = migrantes, y = label)) + geom_col(fill = censo_palette$migrant, width = 0.72) + scale_x_continuous(labels = num_fmt) + labs(title = "Figura 2. Comunas con mayor población nacida fuera de Chile", subtitle = "Censo 2024", x = "Personas", y = NULL, caption = "Fuente: Elaboración propia con microdatos oficiales del Censo 2024.") + base_theme()
    save_plot(p24_2, file.path(fig_2024, "figura_05_top_comunas_2024.png"), 10, 7)
    write_csv(comuna_plot %>% select(region, comuna, migrantes, year, comuna_label, region_label, label_txt), file.path(tab_2024, "figura_05_top_comunas_2024.csv"))

    plot_pyramid(s24$pyramid, "Pirámide poblacional de la población migrante", "Censo 2024", file.path(fig_2024, "figura_06_piramide_migrante_2024.png"))
    write_csv(s24$pyramid, file.path(tab_2024, "figura_06_piramide_migrante_2024.csv"))

    s24_country_plot <- build_country_ranking(s24$country, top_n = 9L)
    p24_4 <- ggplot(s24_country_plot, aes(x = n, y = country)) + geom_col(fill = censo_palette$migrant, width = 0.72) + scale_x_continuous(labels = num_fmt) + labs(title = "Figura 4. Principales países de nacimiento", subtitle = "Población nacida fuera de Chile, Censo 2024", x = "Personas", y = NULL, caption = "Fuente: Elaboración propia con microdatos oficiales del Censo 2024.") + base_theme()
    save_plot(p24_4, file.path(fig_2024, "figura_23_principales_paises_2024.png"), 9.8, 6.8)
    write_csv(s24_country_plot, file.path(tab_2024, "figura_23_principales_paises_2024.csv"))

    arrival_plot <- s24$arrival %>%
      mutate(
        label = clean_country_label(label),
        order_id = match(
          label,
          c(
            "Antes de 1990",
            "Entre 1990 y 1999",
            "Entre 2000 y 2009",
            "Entre 2010 y 2013",
            "Entre 2014 y 2016",
            "Entre 2017 y 2019",
            "Entre 2020 y 2022",
            "Entre 2023 y 2024"
          )
        )
      ) %>%
      arrange(order_id)
    p24_5 <- ggplot(arrival_plot %>% mutate(label = factor(label, levels = rev(label))), aes(x = n, y = label)) + geom_col(fill = censo_palette$accent, width = 0.72) + scale_x_continuous(labels = num_fmt) + labs(title = "Figura 5. Periodo de llegada a Chile", subtitle = "Población nacida fuera de Chile, Censo 2024, orden cronológico", x = "Personas", y = NULL, caption = "Fuente: Elaboración propia con microdatos oficiales del Censo 2024.") + base_theme()
    save_plot(p24_5, file.path(fig_2024, "figura_09_periodo_llegada_2024.png"), 10, 6.5)
    write_csv(arrival_plot, file.path(tab_2024, "figura_09_periodo_llegada_2024.csv"))

    p24_6 <- ggplot(s24$sex %>% mutate(sex = factor(sex, levels = c("Hombres", "Mujeres"))), aes(x = sex, y = share, fill = sex)) + geom_col(width = 0.58) + geom_text(aes(label = pct_fmt(share)), vjust = -0.35, size = 3.2, color = "#425466") + scale_fill_manual(values = c("Hombres" = censo_palette$migrant, "Mujeres" = censo_palette$accent)) + scale_y_continuous(labels = pct_fmt, expand = expansion(mult = c(0, 0.08))) + labs(title = "Figura 6. Composición por sexo de la población migrante", subtitle = "Censo 2024", x = NULL, y = "Porcentaje", caption = "Fuente: Elaboración propia con microdatos oficiales del Censo 2024.") + base_theme()
    save_plot(p24_6, file.path(fig_2024, "figura_07_sexo_migrantes_2024.png"), 7.2, 5.6)
    write_csv(s24$sex, file.path(tab_2024, "figura_07_sexo_migrantes_2024.csv"))

    p24_7 <- ggplot(s24$age_group %>% mutate(age_group = factor(age_group, levels = c("0 a 14", "15 a 29", "30 a 44", "45 a 64", "65 y más"))), aes(x = age_group, y = share, fill = age_group)) + geom_col(width = 0.62, show.legend = FALSE) + geom_text(aes(label = pct_fmt(share)), vjust = -0.35, size = 3.0, color = "#425466") + scale_fill_manual(values = c("0 a 14" = censo_palette$olive, "15 a 29" = censo_palette$teal, "30 a 44" = censo_palette$migrant, "45 a 64" = censo_palette$local, "65 y más" = censo_palette$accent)) + scale_y_continuous(labels = pct_fmt, expand = expansion(mult = c(0, 0.08))) + labs(title = "Figura 7. Estructura etaria agregada de la población migrante", subtitle = "Censo 2024", x = NULL, y = "Porcentaje", caption = "Fuente: Elaboración propia con microdatos oficiales del Censo 2024.") + base_theme()
    save_plot(p24_7, file.path(fig_2024, "figura_08_estructura_etaria_migrante_2024.png"), 8.8, 5.8)
    write_csv(s24$age_group, file.path(tab_2024, "figura_08_estructura_etaria_migrante_2024.csv"))

    p24_8 <- ggplot(s24$school %>% mutate(migrant_status = factor(migrant_status, levels = c("Nacidos en Chile", "Nacidos fuera de Chile"))), aes(x = migrant_status, y = promedio_escolaridad, fill = migrant_status)) + geom_col(width = 0.58) + geom_text(aes(label = number(promedio_escolaridad, accuracy = 0.1, decimal.mark = ",")), vjust = -0.35, size = 3.2, color = "#425466") + scale_fill_manual(values = c("Nacidos en Chile" = censo_palette$local, "Nacidos fuera de Chile" = censo_palette$migrant)) + labs(title = "Figura 8. Años promedio de escolaridad en población de 25 a 50 años", subtitle = "Censo 2024", x = NULL, y = "Años", caption = "Fuente: Elaboración propia con microdatos oficiales del Censo 2024.") + base_theme()
    save_plot(p24_8, file.path(fig_2024, "figura_14_escolaridad_25_50_2024.png"), 8.2, 5.8)
    write_csv(s24$school, file.path(tab_2024, "figura_14_escolaridad_25_50_2024.csv"))

    p24_9 <- ggplot(s24$higher %>% mutate(migrant_status = factor(migrant_status, levels = c("Nacidos en Chile", "Nacidos fuera de Chile"))), aes(x = migrant_status, y = prop_superior, fill = migrant_status)) + geom_col(width = 0.58) + geom_text(aes(label = pct_fmt(prop_superior)), vjust = -0.35, size = 3.2, color = "#425466") + scale_fill_manual(values = c("Nacidos en Chile" = censo_palette$local, "Nacidos fuera de Chile" = censo_palette$migrant)) + scale_y_continuous(labels = pct_fmt, expand = expansion(mult = c(0, 0.08))) + labs(title = "Figura 9. Población de 25 a 50 años con educación superior", subtitle = "Censo 2024", x = NULL, y = "Porcentaje", caption = "Fuente: Elaboración propia con microdatos oficiales del Censo 2024.") + base_theme()
    save_plot(p24_9, file.path(fig_2024, "figura_15_educacion_superior_2024.png"), 8.2, 5.8)
    write_csv(s24$higher, file.path(tab_2024, "figura_15_educacion_superior_2024.csv"))
  }

  if (build %in% c("all", "prepare")) {
    objects <- build_summaries()
  }
  if (build %in% c("all", "figures")) {
    objects <- if (exists("objects", inherits = FALSE)) objects else readRDS(file.path(final_dir, "censo_summary_objects.rds"))
    make_figures(objects)
  }

  message("CENSO: pipeline completado.")
  invisible(TRUE)
}

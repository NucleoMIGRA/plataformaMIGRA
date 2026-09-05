# EPE_INE_SNM
# Fuente de datos: https://www.ine.gob.cl/estadisticas-por-tema/demografia-y-poblacion/demografia
# Unidad de observación: estimación agregada por año, territorio y característica demográfica.
# Espera insumos en `data/raw/epe_ine/` y construye datos procesados y productos públicos.
# Aunque la carpeta publica no distribuye esas bases, aqui queda documentada la logica general del flujo:
# 1. lectura de datos originales;
# 2. procesamiento hacia `data/interim/`;
# 3. construccion de paneles en `data/final/`;
# 4. generacion de tablas espejo;
# 5. generacion de figuras publicables;
# 6. exportación hacia `figures/epe_ine_snm/` y `tables/epe_ine_snm/`.

run_epe_ine <- function(build = c("all", "data", "figures"), force_rebuild = FALSE) {
  build <- match.arg(build)

  required_packages <- c(
    "here", "readr", "readxl", "dplyr", "tidyr", "ggplot2", "forcats", "scales", "purrr", "ragg", "tibble"
  )
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(
      "EPE/INE: faltan paquetes requeridos: ",
      paste(missing_packages, collapse = ", "),
      ". Instala antes de correr el pipeline.",
      call. = FALSE
    )
  }

  suppressPackageStartupMessages({
    library(here)
    library(readr)
    library(readxl)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(forcats)
    library(scales)
    library(purrr)
    library(ragg)
    library(tibble)
  })

  for (loc in c("en_US.UTF-8", "es_CL.UTF-8", "C.UTF-8")) {
    ok <- tryCatch(Sys.setlocale("LC_CTYPE", loc), warning = function(w) NA_character_, error = function(e) NA_character_)
    if (!is.na(ok)) break
  }

  raw_root <- here::here("data", "raw", "epe_ine")
  raw_ine <- file.path(raw_root, "ine")
  raw_epe2023 <- file.path(raw_root, "estimacion_extranjeros_2023")

  interim_dir <- here::here("data", "interim", "epe_ine")
  final_dir <- here::here("data", "final", "epe_ine")
  fig_dir <- here::here("figures", "epe_ine_snm")
  tab_dir <- here::here("tables", "epe_ine_snm")
  sheet_dir <- file.path(interim_dir, "cuadros_2023_sheets")

  dir.create(interim_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(sheet_dir, recursive = TRUE, showWarnings = FALSE)

  region_final <- file.path(final_dir, "epe_ine_region_panel.rds")
  comuna_final <- file.path(final_dir, "epe_ine_comuna_panel.rds")
  workbook_final <- file.path(final_dir, "epe_ine_workbook_tables.rds")

  region_order <- c(
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

  clean_names_ascii <- function(x) {
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    x[is.na(x)] <- ""
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("(^_+|_+$)", "", x)
    x
  }

  clean_label <- function(x) {
    x <- as.character(x)
    x <- iconv(x, from = "", to = "UTF-8", sub = "")
    x[is.na(x)] <- NA_character_
    x <- gsub("[[:space:]]+", " ", x)
    x <- trimws(x)
    x[x %in% c("", "NA")] <- NA_character_
    x
  }

  key_ascii <- function(x) {
    x <- clean_label(x)
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    x <- tolower(x)
    x
  }

  parse_num <- function(x) {
    raw <- clean_label(x)
    direct <- suppressWarnings(as.numeric(raw))
    comma_decimal <- suppressWarnings(as.numeric(gsub(",", ".", raw, fixed = TRUE)))
    parsed <- readr::parse_number(raw, locale = locale(decimal_mark = ",", grouping_mark = "."))
    dplyr::coalesce(direct, comma_decimal, parsed)
  }

  normalize_region <- function(x) {
    y <- clean_label(x)
    k <- key_ascii(x)
    dplyr::case_when(
      is.na(k) ~ NA_character_,
      grepl("total pais|total nacional", k) ~ "Total nacional",
      grepl("arica", k) ~ "Arica y Parinacota",
      grepl("tarapaca", k) ~ "Tarapac\u00e1",
      grepl("antofagasta", k) ~ "Antofagasta",
      grepl("atacama", k) ~ "Atacama",
      grepl("coquimbo", k) ~ "Coquimbo",
      grepl("valparaiso", k) ~ "Valpara\u00edso",
      grepl("metropolitana", k) ~ "Metropolitana",
      grepl("o'higgins|ohiggins|libertador", k) ~ "O'Higgins",
      grepl("maule", k) ~ "Maule",
      grepl("nuble", k) ~ "\u00d1uble",
      grepl("biobio|biobo", k) ~ "Biob\u00edo",
      grepl("araucania", k) ~ "La Araucan\u00eda",
      grepl("rios", k) ~ "Los R\u00edos",
      grepl("lagos", k) ~ "Los Lagos",
      grepl("aysen|aysn", k) ~ "Ays\u00e9n",
      grepl("magallanes", k) ~ "Magallanes",
      grepl("ignorad", k) ~ "Región ignorada",
      TRUE ~ y
    )
  }

  normalize_comuna <- function(x) {
    y <- clean_label(x)
    k <- key_ascii(x)
    dplyr::case_when(
      is.na(k) ~ NA_character_,
      grepl("^otras comunas$", k) ~ "Otras comunas",
      grepl("^ignorada$", k) ~ "Ignorada",
      grepl("^uoa$", k) ~ "UOA",
      grepl("^santiago$", k) ~ "Santiago",
      grepl("^antofagasta$", k) ~ "Antofagasta",
      grepl("^estacin central$|^estacion central$", k) ~ "Estación Central",
      grepl("^independencia$", k) ~ "Independencia",
      grepl("^iquique$", k) ~ "Iquique",
      grepl("^recoleta$", k) ~ "Recoleta",
      grepl("^las condes$", k) ~ "Las Condes",
      grepl("^calama$", k) ~ "Calama",
      grepl("^quinta normal$", k) ~ "Quinta Normal",
      grepl("^arica$", k) ~ "Arica",
      grepl("^san miguel$", k) ~ "San Miguel",
      grepl("^la florida$", k) ~ "La Florida",
      grepl("^quilicura$", k) ~ "Quilicura",
      grepl("^maip$|^maipu$", k) ~ "Maipú",
      TRUE ~ enc2utf8(tools::toTitleCase(tolower(y)))
    )
  }

  normalize_country <- function(x) {
    y <- clean_label(x)
    k <- key_ascii(x)
    dplyr::case_when(
      is.na(k) ~ NA_character_,
      grepl("total pais|total nacional", k) ~ "Total país",
      grepl("pais ignorado|pas ignorado", k) ~ "País ignorado",
      grepl("otro pais|otro pas", k) ~ "Otros países",
      grepl("r\\.? dominicana|republica dominicana", k) ~ "República Dominicana",
      grepl("haiti|hait$", k) ~ "Haití",
      grepl("peru|per$", k) ~ "Perú",
      grepl("espana", k) ~ "España",
      grepl("mexico", k) ~ "México",
      TRUE ~ y
    )
  }

  normalize_age <- function(x) {
    y <- clean_label(x)
    k <- key_ascii(x)
    dplyr::case_when(
      is.na(k) ~ NA_character_,
      grepl("total", k) ~ "Total",
      grepl("0 a 4|00 a 04", k) ~ "0 a 4",
      grepl("5 a 9|05 a 09", k) ~ "5 a 9",
      grepl("10 a 14", k) ~ "10 a 14",
      grepl("15 a 19", k) ~ "15 a 19",
      grepl("20 a 24", k) ~ "20 a 24",
      grepl("25 a 29", k) ~ "25 a 29",
      grepl("30 a 34", k) ~ "30 a 34",
      grepl("35 a 39", k) ~ "35 a 39",
      grepl("40 a 44", k) ~ "40 a 44",
      grepl("45 a 49", k) ~ "45 a 49",
      grepl("50 a 54", k) ~ "50 a 54",
      grepl("55 a 59", k) ~ "55 a 59",
      grepl("60 a 64", k) ~ "60 a 64",
      grepl("65 a 69", k) ~ "65 a 69",
      grepl("70 a 74", k) ~ "70 a 74",
      grepl("75 a 79", k) ~ "75 a 79",
      grepl("80|80 o mas|80 y mas", k) ~ "80 o más",
      TRUE ~ y
    )
  }

  detect_delim <- function(path) if (grepl("2021", basename(path))) ";" else ","
  detect_encoding <- function(path) if (grepl("2021", basename(path))) "Latin1" else "UTF-8"

  read_epe_csv <- function(path, level = c("region", "comuna")) {
    level <- match.arg(level)
    raw <- readr::read_delim(
      path,
      delim = detect_delim(path),
      locale = locale(encoding = detect_encoding(path)),
      show_col_types = FALSE,
      name_repair = "minimal"
    )
    release_year <- suppressWarnings(as.integer(sub(".*(2021|2022|2023).*", "\\1", path)))

    if (level == "region") {
      if (ncol(raw) == 8) {
        names(raw) <- c("sexo", "edad", "pais", "ano_estimacion", "region", "censo_ajustado", "rraa_total", "estimacion")
        raw$codregeo <- NA_integer_
        raw$rraa_regular <- NA_real_
        raw$rraa_irregular <- NA_real_
      } else {
        names(raw) <- c("sexo", "edad", "pais", "ano_estimacion", "codregeo", "region", "censo_ajustado", "rraa_regular", "rraa_irregular", "rraa_total", "estimacion")
      }
    }

    if (level == "comuna") {
      if (ncol(raw) == 9) {
        names(raw) <- c("sexo", "edad", "pais", "ano_estimacion", "region", "comuna", "censo_ajustado", "rraa_total", "estimacion")
        raw$rraa_regular <- NA_real_
        raw$rraa_irregular <- NA_real_
      } else {
        names(raw) <- c("sexo", "edad", "pais", "ano_estimacion", "region", "comuna", "censo_ajustado", "rraa_regular", "rraa_irregular", "rraa_total", "estimacion")
      }
    }

    raw %>%
      transmute(
        release_year = release_year,
        sexo = clean_label(sexo),
        edad = normalize_age(edad),
        pais = normalize_country(pais),
        ano_estimacion = suppressWarnings(as.integer(ano_estimacion)),
        codregeo = if ("codregeo" %in% names(raw)) suppressWarnings(as.integer(codregeo)) else NA_integer_,
        region = normalize_region(region),
        comuna = if ("comuna" %in% names(raw)) clean_label(comuna) else NA_character_,
        censo_ajustado = parse_num(censo_ajustado),
        rraa_regular = if ("rraa_regular" %in% names(raw)) parse_num(rraa_regular) else NA_real_,
        rraa_irregular = if ("rraa_irregular" %in% names(raw)) parse_num(rraa_irregular) else NA_real_,
        rraa_total = parse_num(rraa_total),
        estimacion = parse_num(estimacion)
      ) %>%
      filter(!is.na(ano_estimacion))
  }

  extract_workbook_sheets <- function(workbook_path, out_dir) {
    tmp_wb <- tempfile(fileext = ".xlsx")
    file.copy(workbook_path, tmp_wb, overwrite = TRUE)
    sheets <- readxl::excel_sheets(tmp_wb)
    sheet_index <- tibble::tibble(sheet_id = seq_along(sheets), sheet_name = sheets)
    readr::write_csv(sheet_index, file.path(out_dir, "sheet_index.csv"))
    purrr::walk2(sheet_index$sheet_id, sheet_index$sheet_name, function(i, s) {
      raw <- readxl::read_excel(tmp_wb, sheet = i, col_names = FALSE, .name_repair = "minimal")
      nm <- paste0(sprintf("%02d", i), "_", clean_names_ascii(s), ".csv")
      readr::write_csv(raw, file.path(out_dir, nm), na = "")
    })
    invisible(sheet_index)
  }

  parse_group_sheet <- function(tmp_wb, sheet_index, id_cols, metrics, normalizer = NULL, normalizer2 = NULL) {
    raw <- readxl::read_excel(tmp_wb, sheet = sheet_index, col_names = FALSE, .name_repair = "minimal")
    body <- raw[-c(1, 2), ]
    years <- 2018:2023
    group_size <- length(metrics)
    start_col <- id_cols + 1

    out <- purrr::map2_dfr(years, seq(0, by = group_size, length.out = length(years)), function(year, offset) {
      cols <- start_col + offset + seq_len(group_size) - 1
      tmp <- body[, c(seq_len(id_cols), cols)]
      names(tmp) <- c(paste0("id", seq_len(id_cols)), metrics)
      tmp$ano <- year
      tmp
    })

    if (id_cols >= 1) out$id1 <- clean_label(out$id1)
    if (id_cols >= 2) out$id2 <- clean_label(out$id2)
    if (!is.null(normalizer)) out$id1 <- normalizer(out$id1)
    if (!is.null(normalizer2) && id_cols >= 2) out$id2 <- normalizer2(out$id2)
    out <- out %>% mutate(across(all_of(metrics), parse_num))
    out
  }

  label_big_n <- scales::label_number(big.mark = ".", decimal.mark = ",", accuracy = 1)
  label_pct <- scales::label_percent(accuracy = 0.1, decimal.mark = ",")
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

  theme_datamigra <- function() {
    theme_minimal(base_size = 12, base_family = font_family) +
      theme(
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        axis.title = element_text(colour = "#102a43"),
        axis.text = element_text(colour = "#243b53"),
        legend.title = element_blank(),
        legend.text = element_text(colour = "#243b53"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "#d9e2ec", linewidth = 0.35),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(10, 16, 10, 10),
        legend.position = "top"
      )
  }

  public_name <- function(name) {
    names <- c(
      fig_1_stock_nacional = "figura_01_stock_nacional", fig_4_stock_por_sexo = "figura_02_stock_por_sexo",
      fig_6_principales_nacionalidades_2023 = "figura_03_principales_nacionalidades_2023", fig_7_series_principales_paises = "figura_04_series_principales_paises",
      fig_2_distribucion_regional_2023 = "figura_05_distribucion_regional_2023", fig_3_cambio_regional = "figura_06_cambio_regional",
      fig_9_top_comunas_2023 = "figura_07_top_comunas_2023", fig_12_concentracion_comunal_2023 = "figura_08_concentracion_comunal_2023",
      fig_5_edad_sexo_2023 = "figura_09_edad_sexo_2023", fig_8_nna_por_pais_2023 = "figura_10_nna_por_pais_2023",
      fig_11_irregular_pais_2023 = "figura_11_irregular_pais_2023", fig_10_irregular_region_2023 = "figura_12_irregular_region_2023"
    )
    unname(names[[name]])
  }

  save_plot <- function(plot, name, width = 10, height = 6) {
    plot_clean <- plot +
      labs(title = NULL, subtitle = NULL, caption = NULL) +
      theme(
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank()
      )
    file_out <- file.path(fig_dir, paste0(public_name(name), ".png"))
    if (requireNamespace("ragg", quietly = TRUE)) {
      ragg::agg_png(file_out, width = width, height = height, units = "in", res = 320, background = "white")
      print(plot_clean)
      grDevices::dev.off()
    } else {
      grDevices::png(file_out, width = width, height = height, units = "in", res = 320, type = "cairo")
      print(plot_clean)
      grDevices::dev.off()
    }
    message("EPE/INE: figura exportada -> ", name)
  }

  save_table <- function(df, name) {
    readr::write_csv(df, file.path(tab_dir, paste0(public_name(name), ".csv")))
    message("EPE/INE: tabla exportada -> ", name)
  }

  order_for_flip <- function(x) {
    rev(x)
  }

  workbook_path <- list.files(raw_epe2023, pattern = "^6\\..*xlsx$", full.names = TRUE)[1]

  if (build %in% c("all", "data") || force_rebuild || !file.exists(region_final) || !file.exists(comuna_final) || !file.exists(workbook_final)) {
    message("EPE/INE: procesando workbook 2023 y CSV territoriales.")

    extract_workbook_sheets(workbook_path, sheet_dir)
    tmp_wb <- tempfile(fileext = ".xlsx")
    file.copy(workbook_path, tmp_wb, overwrite = TRUE)

    tab1_components <- parse_group_sheet(tmp_wb, 3, id_cols = 1, metrics = c("censo_ajustado", "rraa_total", "estimacion"), normalizer = normalize_region) %>%
      rename(region = id1)

    tab2_region <- parse_group_sheet(tmp_wb, 4, id_cols = 1, metrics = c("estimacion", "share_pct"), normalizer = normalize_region) %>%
      rename(region = id1) %>%
      mutate(
        share_pct = if_else(share_pct > 1, share_pct / 100, share_pct),
        share = share_pct
      )

    tab3_sex <- parse_group_sheet(tmp_wb, 5, id_cols = 1, metrics = c("hombres", "mujeres", "total", "indice_masculinidad"), normalizer = normalize_region) %>%
      rename(region = id1)

    tab5_age_sex <- parse_group_sheet(tmp_wb, 7, id_cols = 2, metrics = c("hombres", "mujeres", "total"), normalizer = normalize_region, normalizer2 = normalize_age) %>%
      rename(region = id1, edad = id2)

    tab7_country <- parse_group_sheet(tmp_wb, 10, id_cols = 1, metrics = c("estimacion", "share_pct"), normalizer = normalize_country) %>%
      rename(pais = id1) %>%
      mutate(
        share_pct = if_else(share_pct > 1, share_pct / 100, share_pct),
        share = share_pct
      )

    tab8_nna <- parse_group_sheet(tmp_wb, 11, id_cols = 1, metrics = c("pob_nna", "pob_20ymas", "estimacion", "share_nna", "indice_nna"), normalizer = normalize_country) %>%
      rename(pais = id1) %>%
      mutate(share_nna = if_else(share_nna > 1, share_nna / 100, share_nna))

    region_files <- c(
      file.path(raw_ine, "base-2021-regiones.csv"),
      file.path(raw_ine, "base-2022-regiones.csv"),
      file.path(raw_ine, "base-2023-regiones-epe2023.csv")
    )
    comuna_files <- c(
      file.path(raw_ine, "base-2021-comunas.csv"),
      file.path(raw_ine, "base-2022-comunas.csv"),
      file.path(raw_epe2023, "10. basecomunas.csv")
    )

    region_all <- purrr::map_dfr(region_files, read_epe_csv, level = "region")
    comuna_all <- purrr::map_dfr(comuna_files, read_epe_csv, level = "comuna")

    region_latest <- region_all %>%
      arrange(release_year) %>%
      group_by(ano_estimacion, codregeo, sexo, edad, pais, region) %>%
      slice_tail(n = 1) %>%
      ungroup()

    comuna_latest <- comuna_all %>%
      arrange(release_year) %>%
      group_by(ano_estimacion, sexo, edad, pais, region, comuna) %>%
      slice_tail(n = 1) %>%
      ungroup()

    workbook_tables <- list(
      tab1_components = tab1_components,
      tab2_region = tab2_region,
      tab3_sex = tab3_sex,
      tab5_age_sex = tab5_age_sex,
      tab7_country = tab7_country,
      tab8_nna = tab8_nna
    )

    readr::write_rds(region_latest, region_final)
    readr::write_rds(comuna_latest, comuna_final)
    readr::write_rds(workbook_tables, workbook_final)
  }

  region <- readr::read_rds(region_final)
  comuna <- readr::read_rds(comuna_final)
  wb <- readr::read_rds(workbook_final)


  if (build %in% c("all", "figures")) {
    message("EPE/INE: generando figuras limpias.")
    unlink(Sys.glob(file.path(fig_dir, "figura_*.png")), force = TRUE)
    unlink(Sys.glob(file.path(tab_dir, "figura_*.csv")), force = TRUE)

    fix_share <- function(x) {
      dplyr::case_when(
        is.na(x) ~ NA_real_,
        x > 1000 ~ x / 1e17,
        x > 1 ~ x / 100,
        TRUE ~ x
      )
    }

    fix_share_nna <- function(x) {
      dplyr::case_when(
        is.na(x) ~ NA_real_,
        x > 1000 ~ x / 1e16,
        x > 1 ~ x / 100,
        TRUE ~ x
      )
    }

    country_display <- function(x) {
      vapply(x, function(one) {
        if (is.na(one)) return(NA_character_)
        k <- key_ascii(one)
        if (grepl("otro pais|otro pas", k)) return("Otros países")
        if (grepl("pais ignorado|pas ignorado", k)) return("Pa\u00eds ignorado")
        if (grepl("republica dominicana|r\\.? dominicana", k)) return("Rep\u00fablica Dominicana")
        if (grepl("estados unidos", k)) return("Estados Unidos")
        if (grepl("haiti", k)) return("Hait\u00ed")
        if (grepl("peru|per$", k)) return("Per\u00fa")
        if (grepl("espana", k)) return("Espa\u00f1a")
        if (grepl("mexico", k)) return("M\u00e9xico")
        out <- tools::toTitleCase(tolower(clean_label(one)))
        enc2utf8(out)
      }, character(1))
    }

    tab2_clean <- wb$tab2_region %>%
      mutate(
        region = normalize_region(region),
        share = fix_share(share_pct)
      ) %>%
      filter(!is.na(region), !grepl("fuente|volver|nota", key_ascii(region)))

    tab3_clean <- wb$tab3_sex %>%
      mutate(region = normalize_region(region)) %>%
      filter(!is.na(region), !grepl("fuente|volver|nota", key_ascii(region)))

    tab5_clean <- wb$tab5_age_sex %>%
      mutate(
        region = normalize_region(region),
        edad = normalize_age(edad)
      ) %>%
      filter(
        !is.na(region),
        !is.na(edad),
        !grepl("fuente|volver|nota", key_ascii(region)),
        !grepl("fuente|volver|nota", key_ascii(edad))
      )

    tab7_clean <- wb$tab7_country %>%
      mutate(
        pais = normalize_country(pais),
        pais_display = country_display(pais),
        share = fix_share(share_pct)
      ) %>%
      filter(!is.na(pais), !grepl("fuente|volver|nota", key_ascii(pais)))
    tab7_display <- tab7_clean %>%
      group_by(ano, pais_display) %>%
      summarise(
        estimacion = sum(estimacion, na.rm = TRUE),
        share = sum(share, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        pais_display = case_when(
          grepl("^total", key_ascii(pais_display)) ~ "Total pa\u00eds",
          grepl("otro pais|otro pas", key_ascii(pais_display)) ~ "Otros países",
          grepl("pais ignorado|pas ignorado", key_ascii(pais_display)) ~ "Pa\u00eds ignorado",
          TRUE ~ pais_display
        )
      )

    tab8_clean <- wb$tab8_nna %>%
      mutate(
        pais = normalize_country(pais),
        pais_display = country_display(pais),
        share_nna = fix_share_nna(share_nna)
      ) %>%
      filter(!is.na(pais), !grepl("fuente|volver|nota", key_ascii(pais)))

    region_clean <- region %>%
      mutate(
        region = normalize_region(region),
        pais = normalize_country(pais)
      )
    pais_map <- tibble(pais = unique(region_clean$pais)) %>%
      mutate(pais_display = country_display(pais))
    region_clean <- region_clean %>%
      left_join(pais_map, by = "pais")

    comuna_clean <- comuna %>%
      mutate(comuna = normalize_comuna(comuna))

    latest_year <- max(tab2_clean$ano, na.rm = TRUE)
    first_year <- min(tab2_clean$ano, na.rm = TRUE)

    fig1 <- tab2_clean %>%
      filter(grepl("^total", key_ascii(region))) %>%
      select(region, ano, estimacion, share)
    p1 <- ggplot(fig1, aes(ano, estimacion)) +
      geom_line(linewidth = 1.25, colour = "#2F6C8F") +
      geom_point(size = 2.6, colour = "#2F6C8F") +
      scale_x_continuous(breaks = sort(unique(fig1$ano))) +
      scale_y_continuous(labels = label_big_n) +
      labs(title = "Figura 1. Evolución de la población extranjera estimada en Chile", subtitle = "Serie anual 2018-2023", x = "Año", y = "Personas", caption = "Fuente: elaboración propia con cuadros EPE 2023 de INE-SNM.") +
      theme_datamigra() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    save_plot(p1, "fig_1_stock_nacional")
    save_table(fig1, "fig_1_stock_nacional")

    fig2 <- tab2_clean %>%
      filter(ano == latest_year, region %in% region_order) %>%
      mutate(region_plot = factor(region, levels = order_for_flip(region_order), labels = wrap_label(order_for_flip(region_order), width = 24)))
    p2 <- ggplot(fig2, aes(region_plot, estimacion)) +
      geom_col(fill = "#D58A4E", width = 0.72) +
      geom_text(aes(label = paste0(label_big_n(estimacion), " (", label_pct(share), ")")), hjust = -0.05, size = 3.1, colour = "#24364A") +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = label_big_n, expand = expansion(mult = c(0, 0.12))) +
      labs(title = paste0("Figura 2. Distribución regional de la población extranjera, ", latest_year), subtitle = "Regiones ordenadas de norte a sur", x = NULL, y = "Personas", caption = "Fuente: elaboración propia con cuadros EPE 2023 de INE-SNM.") +
      theme_datamigra()
    save_plot(p2, "fig_2_distribucion_regional_2023", width = 12, height = 8)
    save_table(fig2, "fig_2_distribucion_regional_2023")

    fig3 <- tab2_clean %>%
      filter(region %in% region_order, !grepl("ignorad|no reporta", key_ascii(region)), ano %in% c(first_year, latest_year)) %>%
      select(region, ano, estimacion) %>%
      pivot_wider(names_from = ano, values_from = estimacion) %>%
      mutate(
        cambio = .[[as.character(latest_year)]] - .[[as.character(first_year)]],
        etiqueta = label_big_n(cambio)
      ) %>%
      mutate(region_plot = factor(region, levels = order_for_flip(region_order), labels = wrap_label(order_for_flip(region_order), width = 24)))
    p3 <- ggplot(fig3, aes(region_plot, cambio)) +
      geom_col(fill = "#5A8F3D", width = 0.72) +
      geom_text(aes(label = etiqueta), hjust = -0.05, size = 3.0, colour = "#24364A") +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = label_big_n, expand = expansion(mult = c(0, 0.12))) +
      labs(title = paste0("Figura 3. Cambio acumulado por región, ", first_year, "-", latest_year), subtitle = "Todas las regiones, ordenadas de norte a sur", x = NULL, y = "Cambio absoluto", caption = "Fuente: elaboración propia con cuadros EPE 2023 de INE-SNM.") +
      theme_datamigra()
    save_plot(p3, "fig_3_cambio_regional", width = 12, height = 8)
    save_table(fig3, "fig_3_cambio_regional")

    fig4 <- tab3_clean %>%
      filter(grepl("^total", key_ascii(region))) %>%
      select(ano, hombres, mujeres) %>%
      pivot_longer(-ano, names_to = "sexo", values_to = "valor") %>%
      mutate(sexo = recode(sexo, hombres = "Hombres", mujeres = "Mujeres"))
    p4 <- ggplot(fig4, aes(ano, valor, colour = sexo)) +
      geom_line(linewidth = 1.15) +
      geom_point(size = 2.3) +
      scale_x_continuous(breaks = sort(unique(fig4$ano))) +
      scale_y_continuous(labels = label_big_n) +
      scale_colour_manual(values = c("Hombres" = "#4E79A7", "Mujeres" = "#9DC4E2")) +
      labs(title = "Figura 4. Evolución de la población extranjera estimada por sexo", subtitle = "Serie anual 2018-2023", x = "Año", y = "Personas", colour = NULL, caption = "Fuente: elaboración propia con cuadros EPE 2023 de INE-SNM.") +
      theme_datamigra() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    save_plot(p4, "fig_4_stock_por_sexo")
    save_table(fig4, "fig_4_stock_por_sexo")

    age_levels <- c("0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24", "25 a 29", "30 a 34", "35 a 39", "40 a 44", "45 a 49", "50 a 54", "55 a 59", "60 a 64", "65 a 69", "70 a 74", "75 a 79", "80 o más")
    fig5 <- tab5_clean %>%
      filter(grepl("^total", key_ascii(region)), ano == latest_year, edad %in% age_levels) %>%
      select(edad, hombres, mujeres) %>%
      pivot_longer(cols = c(hombres, mujeres), names_to = "sexo", values_to = "valor") %>%
      mutate(
        sexo = recode(sexo, hombres = "Hombres", mujeres = "Mujeres"),
        valor = if_else(sexo == "Hombres", -valor, valor),
        edad = factor(edad, levels = age_levels)
      )
    p5 <- ggplot(fig5, aes(edad, valor, fill = sexo)) +
      geom_col(width = 0.78) +
      coord_flip() +
      scale_fill_manual(values = c("Hombres" = "#537CA6", "Mujeres" = "#9CC2DD")) +
      scale_y_continuous(labels = function(x) label_big_n(abs(x)), expand = expansion(mult = c(0.05, 0.05))) +
      labs(title = paste0("Figura 5. Distribución por sexo y tramo de edad, ", latest_year), subtitle = "Hombres a la izquierda y mujeres a la derecha", x = NULL, y = "Personas", fill = NULL, caption = "Fuente: elaboración propia con cuadros EPE 2023 de INE-SNM.") +
      theme_datamigra() +
      theme(legend.position = "top")
    save_plot(p5, "fig_5_edad_sexo_2023", width = 12, height = 8)
    save_table(fig5, "fig_5_edad_sexo_2023")

    fig6_main <- tab7_display %>%
      mutate(pais_display = normalize_country(pais_display)) %>%
      filter(
        ano == latest_year,
        !grepl("^total", key_ascii(pais_display)),
        !grepl("pais ignorado|pas ignorado", key_ascii(pais_display)),
        !grepl("otros? pais|otro pas", key_ascii(pais_display)),
        !is.na(estimacion)
      ) %>%
      arrange(desc(estimacion)) %>%
      slice_head(n = 10)
    fig6_other <- tab7_display %>%
      mutate(pais_display = normalize_country(pais_display)) %>%
      filter(ano == latest_year, grepl("otros? pais|otro pas", key_ascii(pais_display))) %>%
      summarise(
        pais_display = "Otros países",
        estimacion = sum(estimacion, na.rm = TRUE),
        share = sum(share, na.rm = TRUE),
        ano = latest_year,
        .groups = "drop"
      ) %>%
      filter(estimacion > 0)
    fig6 <- bind_rows(fig6_main, fig6_other) %>%
      mutate(pais_display = if_else(grepl("otro", key_ascii(pais_display)), "Otros países", pais_display)) %>%
      distinct(pais_display, .keep_all = TRUE) %>%
      mutate(pais_display = factor(pais_display, levels = c("Otros países", rev(fig6_main$pais_display))))
    p6 <- ggplot(fig6, aes(pais_display, estimacion)) +
      geom_col(fill = "#5C87A6", width = 0.72) +
      geom_text(aes(label = paste0(label_big_n(estimacion), " (", label_pct(share), ")")), hjust = -0.05, size = 3.1, colour = "#24364A") +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = label_big_n, expand = expansion(mult = c(0, 0.12))) +
      labs(title = paste0("Figura 6. Principales nacionalidades de la población extranjera, ", latest_year), subtitle = "Población estimada y porcentaje del total; Otros países se muestra al final", x = NULL, y = "Personas", caption = "Fuente: elaboración propia con cuadros EPE 2023 de INE-SNM.") +
      theme_datamigra()
    save_plot(p6, "fig_6_principales_nacionalidades_2023", width = 12, height = 8)
    save_table(fig6, "fig_6_principales_nacionalidades_2023")

    top_paises <- tab7_clean %>%
      filter(!grepl("^total|otro pais|otro pas|pais ignorado|pas ignorado", key_ascii(pais)), !is.na(estimacion)) %>%
      group_by(pais_display) %>%
      summarise(total = sum(estimacion, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      slice_head(n = 5) %>%
      pull(pais_display)
    fig7 <- tab7_clean %>%
      filter(pais_display %in% top_paises, !grepl("^total|otro pais|otro pas|pais ignorado|pas ignorado", key_ascii(pais))) %>%
      mutate(pais_display = factor(pais_display, levels = top_paises))
    p7 <- ggplot(fig7, aes(ano, estimacion, colour = pais_display)) +
      geom_line(linewidth = 1.15) +
      geom_point(size = 2.2) +
      scale_x_continuous(breaks = sort(unique(fig7$ano))) +
      scale_y_continuous(labels = label_big_n) +
      scale_colour_manual(values = c("Venezuela" = "#1D6996", "Perú" = "#CC503E", "Colombia" = "#E17C05", "Haití" = "#6F4070", "Bolivia" = "#5E9B44")) +
      labs(title = "Figura 7. Evolución de las principales nacionalidades", subtitle = "Series del top 5 acumulado, en un mismo plano y sin incluir Total país", x = "Año", y = "Personas", colour = NULL, caption = "Fuente: elaboración propia con cuadros EPE 2023 de INE-SNM.") +
      theme_datamigra() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "bottom")
    save_plot(p7, "fig_7_series_principales_paises", width = 12, height = 8)
    save_table(fig7, "fig_7_series_principales_paises")

    fig8 <- tab8_clean %>%
      filter(
        ano == latest_year,
        !grepl("^total|pais ignorado|pas ignorado|otro pais|otro pas", key_ascii(pais)),
        !grepl("ignor", tolower(pais_display)),
        !is.na(share_nna)
      ) %>%
      arrange(desc(share_nna)) %>%
      slice_head(n = 10) %>%
      mutate(etiqueta = paste0(label_pct(share_nna), " · ", label_big_n(estimacion)))
    fig8$pais_display <- factor(fig8$pais_display, levels = order_for_flip(fig8$pais_display))
    p8 <- ggplot(fig8, aes(pais_display, share_nna)) +
      geom_col(fill = "#BC5090", width = 0.72) +
      geom_text(aes(label = etiqueta), hjust = -0.05, size = 3.0, colour = "#24364A") +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = label_pct, expand = expansion(mult = c(0, 0.14))) +
      labs(title = paste0("Figura 8. Proporción de niñas, niños y adolescentes por nacionalidad, ", latest_year), subtitle = "Top 10 según participación de población de 0 a 19 años dentro de cada nacionalidad", x = NULL, y = "% de 0 a 19 años dentro de cada nacionalidad", caption = "Fuente: elaboración propia con cuadros EPE 2023 de INE-SNM.") +
      theme_datamigra()
    save_plot(p8, "fig_8_nna_por_pais_2023", width = 12, height = 8)
    save_table(fig8, "fig_8_nna_por_pais_2023")

    comuna_latest <- comuna_clean %>%
      filter(
        ano_estimacion == latest_year,
        !is.na(comuna),
        !is.na(estimacion),
        !key_ascii(comuna) %in% c("na", "n/a", "nan", "no informa")
      ) %>%
      group_by(comuna) %>%
      summarise(estimacion = sum(estimacion, na.rm = TRUE), .groups = "drop")
    otras_row <- comuna_latest %>% filter(key_ascii(comuna) == "otras comunas")
    fig9_main <- comuna_latest %>%
      filter(!key_ascii(comuna) %in% c("ignorada", "otras comunas", "uoa", "na", "n/a", "nan", "no informa")) %>%
      arrange(desc(estimacion)) %>%
      slice_head(n = 14)
    fig9 <- bind_rows(fig9_main, otras_row)
    fig9$comuna <- factor(fig9$comuna, levels = order_for_flip(c(fig9_main$comuna, intersect("Otras comunas", otras_row$comuna))))
    p9 <- ggplot(fig9, aes(comuna, estimacion)) +
      geom_col(fill = "#7E8EA3", width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = label_big_n) +
      labs(title = paste0("Figura 9. Comunas con mayor población extranjera estimada, ", latest_year), subtitle = "Se excluyen las categorías Ignorada y UOA; Otras comunas queda al final", x = NULL, y = "Personas", caption = "Fuente: elaboración propia con base comunal EPE/INE-SNM.") +
      theme_datamigra()
    save_plot(p9, "fig_9_top_comunas_2023", width = 12, height = 8)
    save_table(fig9, "fig_9_top_comunas_2023")

    fig10 <- region_clean %>%
      filter(ano_estimacion == latest_year, region %in% region_order, !grepl("ignorad|no reporta", key_ascii(region))) %>%
      group_by(region) %>%
      summarise(
        rraa_regular = sum(rraa_regular, na.rm = TRUE),
        rraa_irregular = sum(rraa_irregular, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      right_join(tibble(region = region_order), by = "region") %>%
      mutate(
        total_migrantes = rraa_regular + rraa_irregular,
        has_estimation = !is.na(rraa_regular) & !is.na(rraa_irregular) & total_migrantes > 0,
        share_irregular = if_else(has_estimation, rraa_irregular / total_migrantes, NA_real_),
        etiqueta = if_else(has_estimation, paste0(label_big_n(rraa_irregular), " (", label_pct(share_irregular), ")"), NA_character_),
        region_plot = factor(region, levels = order_for_flip(region_order), labels = wrap_label(order_for_flip(region_order), width = 24))
      )
    missing_regions_irregular <- fig10 %>%
      filter(!has_estimation) %>%
      pull(region)
    fig10_plot <- fig10 %>%
      filter(has_estimation)
    p10 <- ggplot(fig10_plot, aes(region_plot, share_irregular)) +
      geom_col(fill = "#C26D4B", width = 0.72) +
      geom_text(aes(label = etiqueta), hjust = -0.05, size = 3.0, colour = "#24364A") +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = label_pct, expand = expansion(mult = c(0, 0.14))) +
      labs(title = paste0("Figura 10. Participación y volumen de población irregular por región, ", latest_year), subtitle = if (length(missing_regions_irregular) > 0) paste0("Se omiten regiones sin estimación disponible en la base territorial: ", paste(missing_regions_irregular, collapse = ", ")) else "Regiones con estimación disponible, ordenadas de norte a sur", x = NULL, y = "Porcentaje irregular sobre el total estimado", caption = "Fuente: elaboración propia con base regional EPE/INE-SNM.") +
      theme_datamigra()
    save_plot(p10, "fig_10_irregular_region_2023", width = 12, height = 8)
    save_table(fig10, "fig_10_irregular_region_2023")

    fig11_all <- region_clean %>%
      mutate(pais_std = normalize_country(pais)) %>%
      filter(ano_estimacion == latest_year, !is.na(pais_std)) %>%
      group_by(pais_std) %>%
      summarise(
        rraa_regular = sum(rraa_regular, na.rm = TRUE),
        rraa_irregular = sum(rraa_irregular, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        pais_display = country_display(pais_std),
        total_migrantes = rraa_regular + rraa_irregular,
        share_irregular = if_else(total_migrantes > 0, rraa_irregular / total_migrantes, 0),
        share_total_irregular = rraa_irregular / sum(rraa_irregular, na.rm = TRUE)
      ) %>%
      filter(!grepl("ignor", tolower(pais_display)))
    fig11_other <- fig11_all %>% filter(pais_display == "Otros países")
    fig11_main <- fig11_all %>%
      filter(pais_display != "Otros países") %>%
      arrange(desc(rraa_irregular)) %>%
      slice_head(n = 10)
    fig11 <- bind_rows(fig11_main, fig11_other) %>%
      mutate(
        etiqueta = paste0(label_big_n(rraa_irregular), " (", label_pct(share_total_irregular), ")"),
        pais_display = factor(pais_display, levels = c("Otros países", rev(fig11_main$pais_display)))
      )
    p11 <- ggplot(fig11, aes(pais_display, rraa_irregular)) +
      geom_col(fill = "#B56576", width = 0.72) +
      geom_text(aes(label = etiqueta), hjust = -0.05, size = 3.0, colour = "#24364A") +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = label_big_n, expand = expansion(mult = c(0, 0.12))) +
      labs(title = paste0("Figura 11. Población extranjera en situación irregular por nacionalidad, ", latest_year), subtitle = "Top 10 por volumen irregular estimado; Otros países se muestra al final", x = NULL, y = "Personas en situación irregular", caption = "Fuente: elaboración propia con base regional EPE/INE-SNM.") +
      theme_datamigra()
    save_plot(p11, "fig_11_irregular_pais_2023", width = 12, height = 8)
    save_table(fig11, "fig_11_irregular_pais_2023")

    fig12 <- comuna_clean %>%
      filter(ano_estimacion == latest_year, !is.na(comuna), !is.na(estimacion)) %>%
      group_by(comuna) %>%
      summarise(estimacion = sum(estimacion, na.rm = TRUE), .groups = "drop") %>%
      filter(!toupper(comuna) %in% c("IGNORADA", "UOA", "OTRAS COMUNAS")) %>%
      arrange(desc(estimacion)) %>%
      mutate(
        rank = row_number(),
        share = estimacion / sum(estimacion, na.rm = TRUE),
        share_acumulado = cumsum(share)
      ) %>%
      slice_head(n = 25)
    p12 <- ggplot(fig12, aes(rank, share_acumulado)) +
      geom_area(fill = "#DDE8F0", alpha = 0.9) +
      geom_line(linewidth = 1.15, colour = "#2F6C8F") +
      geom_point(size = 2.1, colour = "#2F6C8F") +
      geom_hline(yintercept = sum(head(fig12$share, 5)), linetype = "dashed", linewidth = 0.5, colour = "#C26D4B") +
      geom_hline(yintercept = sum(head(fig12$share, 10)), linetype = "dashed", linewidth = 0.5, colour = "#7E8EA3") +
      annotate("text", x = 5.4, y = sum(head(fig12$share, 5)) + 0.025, label = paste0("Top 5: ", label_pct(sum(head(fig12$share, 5)))), hjust = 0, size = 3.2, colour = "#C26D4B") +
      annotate("text", x = 10.4, y = sum(head(fig12$share, 10)) + 0.025, label = paste0("Top 10: ", label_pct(sum(head(fig12$share, 10)))), hjust = 0, size = 3.2, colour = "#66768A") +
      scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25)) +
      scale_y_continuous(labels = label_pct, expand = expansion(mult = c(0, 0.06))) +
      labs(title = paste0("Figura 12. Concentración comunal de la población extranjera estimada, ", latest_year), subtitle = "Participación acumulada al ordenar comunas por volumen estimado", x = "Ranking de comunas", y = "Participación acumulada", caption = "Fuente: elaboración propia con base comunal EPE/INE-SNM.") +
      theme_datamigra()
    save_plot(p12, "fig_12_concentracion_comunal_2023", width = 10.5, height = 6.8)
    save_table(fig12, "fig_12_concentracion_comunal_2023")

  }

  invisible(list(region = region_final, comuna = comuna_final, workbook = workbook_final, figures = fig_dir, tables = tab_dir))
}

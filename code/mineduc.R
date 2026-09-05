# MINEDUC
# Fuente de datos: Solicitud via Transparencia (MINEDUC y JUNJI).
# Unidad de observación: estudiante-año en matrícula escolar y párvulo-año en JUNJI.
# Espera insumos en `data/raw/mineduc/` y construye datos procesados y productos públicos.
# Aunque la carpeta publica no distribuye esas bases, aqui queda documentada la logica general del flujo:
# 1. lectura de datos originales;
# 2. procesamiento hacia `data/interim/`;
# 3. construccion de paneles en `data/final/`;
# 4. generacion de tablas espejo;
# 5. generacion de figuras publicables;
# 6. exportación hacia `figures/mineduc/` y `tables/mineduc/`.

run_mineduc <- function(build = c("all", "data", "figures"),
                        years = NULL,
                        sources = c("matricula", "junji"),
                        force_rebuild = FALSE) {
  build <- match.arg(build)
  sources <- unique(match.arg(sources, choices = c("matricula", "junji"), several.ok = TRUE))

  for (loc in c("es_CL.UTF-8", "en_US.UTF-8", "C.UTF-8")) {
    ok <- tryCatch(Sys.setlocale("LC_CTYPE", loc), warning = function(w) NA_character_, error = function(e) NA_character_)
    if (!is.na(ok)) break
  }

  suppressPackageStartupMessages({
    library(arrow)
    library(data.table)
    library(dplyr)
    library(forcats)
    library(ggplot2)
    library(here)
    library(janitor)
    library(patchwork)
    library(purrr)
    library(ragg)
    library(readr)
    library(readxl)
    library(scales)
    library(stringr)
    library(tibble)
    library(tidyr)
  })

  `%||%` <- function(x, y) if (is.null(x)) y else x

  raw_dir <- here::here("data", "raw", "mineduc")
  interim_dir <- here::here("data", "interim", "mineduc")
  final_dir <- here::here("data", "final", "mineduc")
  out_fig <- here::here("figures", "mineduc")
  out_tab <- here::here("tables", "mineduc")

  purrr::walk(
    c(
      interim_dir, final_dir, out_fig, out_tab,
      file.path(interim_dir, "matricula"), file.path(interim_dir, "junji")
    ),
    ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
  )

  if (!dir.exists(raw_dir)) {
    stop("MINEDUC: no existe `data/raw/mineduc/`.")
  }

  sanitize_names <- function(x) {
    x |>
      stringr::str_replace("^\ufeff", "") |>
      janitor::make_clean_names()
  }

  trim_na <- function(x) {
    if (!is.character(x)) {
      return(x)
    }
    x <- stringr::str_squish(x)
    x[x %in% c("", "NA", "N/A", "NULL")] <- NA_character_
    x
  }

  as_integer_safe <- function(x) {
    if (inherits(x, "Date")) {
      return(as.integer(format(x, "%Y%m%d")))
    }
    readr::parse_integer(as.character(x), na = c("", "NA", "N/A", "NULL", " ", "NO APLICA", "No aplica"))
  }

  as_numeric_safe <- function(x) {
    readr::parse_number(as.character(x), na = c("", "NA", "N/A", "NULL", " ", "NO APLICA", "No aplica"))
  }

  as_character_safe <- function(x) {
    trim_na(as.character(x))
  }

  write_parquet_safe <- function(x, path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(x, sink = path)
    invisible(path)
  }

  select_existing <- function(df, columns) {
    dplyr::select(df, any_of(columns))
  }

  ensure_columns <- function(df, columns) {
    missing_cols <- setdiff(columns, names(df))
    if (length(missing_cols) > 0) {
      for (col in missing_cols) {
        df[[col]] <- NA
      }
    }
    df[, columns, drop = FALSE]
  }

  read_delim_header <- function(path, delim = ";") {
    line <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
    if (length(line) == 0) {
      stop("Archivo vacio: ", path)
    }
    header <- strsplit(line, delim, fixed = TRUE)[[1]]
    sanitize_names(header)
  }

  to_ascii_upper <- function(x) {
    iconv(as.character(x), from = "", to = "ASCII//TRANSLIT") |>
      stringr::str_to_upper()
  }

  parse_flexible_date <- function(x) {
    x <- stringr::str_replace_all(as.character(x), "[^0-9]", "")
    x[x == ""] <- NA_character_
    out <- rep(as.Date(NA), length(x))
    idx8 <- !is.na(x) & nchar(x) == 8L
    idx6 <- !is.na(x) & nchar(x) == 6L
    idx4 <- !is.na(x) & nchar(x) == 4L
    out[idx8] <- suppressWarnings(as.Date(x[idx8], format = "%Y%m%d"))
    out[idx6] <- suppressWarnings(as.Date(paste0(x[idx6], "01"), format = "%Y%m%d"))
    out[idx4] <- suppressWarnings(as.Date(paste0(x[idx4], "0101"), format = "%Y%m%d"))
    out
  }

  age_on_reference <- function(fec_nac_alu, agno, month = 8L, day = 31L) {
    birth_date <- parse_flexible_date(fec_nac_alu)
    ref_date <- as.Date(sprintf("%s-%02d-%02d", agno, month, day))
    age <- as.numeric(ref_date - birth_date) / 365.25
    age[age < 0] <- NA_real_
    age
  }

  maybe_filter_active_establishments <- function(df) {
    if (!"estado_estab" %in% names(df)) {
      return(df)
    }
    df |>
      filter((is.na(estado_estab) & agno < 2015L) | (!is.na(estado_estab) & estado_estab == 1L))
  }

  build_availability_table <- function(meta_dir, pattern, source_label) {
    files <- list.files(meta_dir, pattern = pattern, full.names = TRUE)
    if (length(files) == 0) {
      return(tibble())
    }
    bind_rows(lapply(files, readr::read_csv, show_col_types = FALSE)) |>
      mutate(source = source_label) |>
      distinct(year, source, column) |>
      arrange(year, column)
  }

  region_short_map <- c(
    "AYP" = "Arica y Parinacota",
    "TPCA" = "Tarapaca",
    "ANTOF" = "Antofagasta",
    "ATCMA" = "Atacama",
    "COQ" = "Coquimbo",
    "VALPO" = "Valparaiso",
    "RM" = "Metropolitana",
    "LGBO" = "O'Higgins",
    "MAULE" = "Maule",
    "NUBLE" = "Nuble",
    "BBIO" = "Biobio",
    "ARAUC" = "La Araucania",
    "RIOS" = "Los Rios",
    "LAGOS" = "Los Lagos",
    "AYSEN" = "Aysen",
    "MAG" = "Magallanes"
  )

  region_label_map <- c(
    "Tarapaca" = "Tarapacá",
    "Valparaiso" = "Valparaíso",
    "Nuble" = "Ñuble",
    "Biobio" = "Biobío",
    "La Araucania" = "La Araucanía",
    "Los Rios" = "Los Ríos",
    "Aysen" = "Aysén"
  )

  region_order <- c(
    "Arica y Parinacota", "Tarapaca", "Antofagasta", "Atacama", "Coquimbo",
    "Valparaiso", "Metropolitana", "O'Higgins", "Maule", "Nuble", "Biobio",
    "La Araucania", "Los Rios", "Los Lagos", "Aysen", "Magallanes"
  )

  normalize_region <- function(x) {
    out <- dplyr::recode(as.character(x), !!!region_short_map, .default = as.character(x))
    factor(out, levels = region_order)
  }

  display_region <- function(x) {
    dplyr::recode(as.character(x), !!!region_label_map, .default = as.character(x))
  }

  clean_rank_label <- function(x) {
    label <- stringr::str_squish(as.character(x))
    label <- stringr::str_replace_all(label, "^MAIP$", "Maipú")
    label
  }

  pin_special_last <- function(label, special_regex = "^(otros?|otro pais|otro país|otro pais dentro|no informa|sin informacion|sin información|na)$") {
    x <- clean_rank_label(label)
    tibble(label = x) |>
      mutate(
        label_ascii = to_ascii_upper(label),
        special_flag = stringr::str_detect(label_ascii, special_regex),
        label_out = label
      )
  }

  process_country_label <- function(x) {
    dplyr::recode(
      as.character(x),
      "Republica Dominicana" = "República Dominicana",
      "Espana" = "España",
      "Haiti" = "Haití",
      "Japon" = "Japón",
      "Mexico" = "México",
      "Panama" = "Panamá",
      "Peru" = "Perú",
      "Apatrida" = "Apátrida",
      "Otro pais" = "Otro país",
      "Sin informacion" = "Sin información",
      .default = as.character(x)
    )
  }

  collapse_top_categories <- function(df, label_col, value_col, top_n = 8L, others_label = "Otros") {
    label_col <- rlang::ensym(label_col)
    value_col <- rlang::ensym(value_col)

    ranked <- df |>
      arrange(desc(!!value_col), !!label_col)

    if (nrow(ranked) <= top_n) {
      return(ranked)
    }

    top <- ranked |>
      slice_head(n = top_n)
    rest <- ranked |>
      slice(-(seq_len(top_n)))

    bind_rows(
      top,
      tibble(!!label_col := others_label, !!value_col := sum(dplyr::pull(rest, !!value_col), na.rm = TRUE))
    )
  }

  base_family <- "Helvetica"
  base_palette <- c(
    primary = "#1D6996",
    accent = "#CC503E",
    accent_2 = "#E17C05",
    green = "#5E9B44",
    purple = "#6F4070",
    dark = "#243B53",
    light = "#D9E2EC",
    muted = "#94A3B8"
  )

  theme_datamigra <- function() {
    theme_minimal(base_family = base_family) +
      theme(
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = base_palette["light"], linewidth = 0.35),
        axis.title = element_text(color = base_palette["dark"], size = 11),
        axis.text = element_text(color = base_palette["dark"], size = 10),
        legend.title = element_blank(),
        legend.text = element_text(color = base_palette["dark"], size = 10),
        legend.position = "top",
        strip.text = element_text(color = base_palette["dark"], face = "bold"),
        plot.margin = margin(10, 16, 10, 10)
      )
  }

  public_name <- function(file_stem) {
    names <- c(
      fig_1_evolucion_matricula_migrante_2016_2025 = "figura_01_evolucion_matricula_migrante_2016_2025",
      fig_6_matricula_migrante_dependencia_2016_2025 = "figura_02_matricula_migrante_dependencia_2016_2025",
      fig_7_matricula_migrante_niveles_2016_2025 = "figura_03_matricula_migrante_niveles_2016_2025",
      fig_4_porcentaje_matricula_migrante_region_2025 = "figura_04_porcentaje_matricula_migrante_region_2025",
      fig_5_comunas_mayor_matricula_migrante_2025 = "figura_05_comunas_mayor_matricula_migrante_2025",
      fig_8_concentracion_escolar_migrante_2025 = "figura_06_concentracion_escolar_migrante_2025",
      fig_2_principales_paises_2025 = "figura_07_principales_paises_2025",
      fig_3_composicion_nacionalidad_2016_2025 = "figura_08_composicion_nacionalidad_2016_2025",
      fig_9_piramide_educativa_2025 = "figura_09_piramide_educativa_2025",
      fig_10_junji_crecimiento_matricula_migrante_2011_2025 = "figura_10_junji_crecimiento_matricula_migrante_2011_2025",
      fig_11_junji_evolucion_matricula_migrante_2011_2025 = "figura_11_junji_evolucion_matricula_migrante_2011_2025",
      fig_13_junji_porcentaje_migrante_region_2025 = "figura_12_junji_porcentaje_migrante_region_2025",
      fig_12_junji_principales_nacionalidades_2025 = "figura_13_junji_principales_nacionalidades_2025",
      fig_14_junji_distribucion_migrante_nivel_2025 = "figura_14_junji_distribucion_migrante_nivel_2025"
    )
    names[[file_stem]] %||% file_stem
  }

  export_plot <- function(plot, dir_path, file_stem, width = 9, height = 5.8, dpi = 320) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    png_path <- file.path(dir_path, paste0(public_name(file_stem), ".png"))

    ragg::agg_png(png_path, width = width, height = height, units = "in", res = dpi, scaling = 1)
    print(plot)
    dev.off()

    invisible(png_path)
  }

  write_note <- function(...) invisible(NULL)

  save_table <- function(x, dir_path, file_stem) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(x, file.path(dir_path, paste0(public_name(file_stem), ".csv")))
  }

  country_map <- tibble::tribble(
    ~pais_origen_alu, ~pais,
    "0", "Sin informacion",
    "1", "Alemania",
    "2", "Argentina",
    "3", "Australia",
    "4", "Bolivia",
    "5", "Brasil",
    "6", "Chile",
    "7", "China",
    "8", "Colombia",
    "9", "Corea",
    "10", "Cuba",
    "11", "Republica Dominicana",
    "12", "Ecuador",
    "13", "Espana",
    "14", "Estados Unidos",
    "15", "Francia",
    "16", "Haiti",
    "17", "Italia",
    "18", "Japon",
    "19", "Mexico",
    "20", "Panama",
    "21", "Paraguay",
    "22", "Peru",
    "23", "Rusia",
    "24", "Suecia",
    "25", "Suiza",
    "26", "Uruguay",
    "27", "Venezuela",
    "28", "Apatrida",
    "29", "Otro pais"
  )

  dependency_map <- tibble::tribble(
    ~cod_depe2, ~dependency_group, ~dependency_order,
    1L, "Publica", 1L,
    5L, "Publica", 1L,
    2L, "Particular subvencionado", 2L,
    3L, "Particular pagado", 3L,
    4L, "Administracion delegada", 4L
  )

  concentration_breaks <- c(-Inf, 0, 0.05, 0.10, 0.20, 0.40, Inf)
  concentration_labels <- c("0%", ">0% a 5%", "5% a 10%", "10% a 20%", "20% a 40%", "40% o mas")

  # ---------------------------------------------------------------------------
  # Matricula
  # ---------------------------------------------------------------------------

  matricula_keep_columns <- function() {
    c(
      "agno", "rbd", "dgv_rbd", "nom_rbd", "cod_reg_rbd", "nom_reg_rbd_a",
      "cod_pro_rbd", "cod_com_rbd", "nom_com_rbd", "cod_deprov_rbd", "nom_deprov_rbd",
      "cod_depe", "cod_depe2", "rural_rbd", "estado_estab", "nombre_slep",
      "cod_ense", "cod_ense2", "cod_ense3", "cod_grado", "cod_grado2", "let_cur",
      "cod_jor", "cod_tip_cur", "cod_des_cur", "tipo_aula", "mrun", "mrun_ipe",
      "gen_alu", "fec_nac_alu", "edad_alu", "cod_etnia_alu", "int_alu", "cod_int_alu",
      "cod_nac_alu", "pais_origen_alu", "cod_reg_alu", "cod_com_alu", "nom_com_alu",
      "cod_sec", "cod_espe", "cod_rama", "cod_men", "ens"
    )
  }

  discover_matricula_files <- function() {
    files <- list.files(
      file.path(raw_dir, "matricula"),
      pattern = "\\.csv$",
      full.names = TRUE,
      ignore.case = TRUE
    )

    tibble(path = files) |>
      mutate(
        file_name = basename(path),
        source = "matricula",
        year = dplyr::coalesce(
          stringr::str_match(file_name, "(?i)(?:unica|única|única)_(\\d{4})_")[, 2] |> as.integer(),
          stringr::str_match(file_name, "_(\\d{4})_\\d{8}")[, 2] |> as.integer()
        )
      ) |>
      filter(!is.na(year)) |>
      arrange(year)
  }

  standardize_matricula <- function(df, year) {
    names(df) <- sanitize_names(names(df))

    if ("nom_reg_rbd" %in% names(df) && !"nom_reg_rbd_a" %in% names(df)) {
      df <- rename(df, nom_reg_rbd_a = nom_reg_rbd)
    }

    keep <- matricula_keep_columns()
    out <- select_existing(df, keep)
    out <- ensure_columns(out, keep)

    if (!"agno" %in% names(out)) {
      out$agno <- year
    }

    out |>
      mutate(
        source_year = year,
        agno = coalesce(as_integer_safe(agno), year),
        rbd = as_integer_safe(rbd),
        dgv_rbd = as_character_safe(dgv_rbd),
        nom_rbd = as_character_safe(nom_rbd),
        cod_reg_rbd = as_integer_safe(cod_reg_rbd),
        nom_reg_rbd_a = as_character_safe(nom_reg_rbd_a),
        cod_pro_rbd = as_integer_safe(cod_pro_rbd),
        cod_com_rbd = as_integer_safe(cod_com_rbd),
        nom_com_rbd = as_character_safe(nom_com_rbd),
        cod_deprov_rbd = as_integer_safe(cod_deprov_rbd),
        nom_deprov_rbd = as_character_safe(nom_deprov_rbd),
        cod_depe = as_integer_safe(cod_depe),
        cod_depe2 = as_integer_safe(cod_depe2),
        rural_rbd = as_integer_safe(rural_rbd),
        estado_estab = as_integer_safe(estado_estab),
        nombre_slep = as_character_safe(nombre_slep),
        cod_ense = as_integer_safe(cod_ense),
        cod_ense2 = as_integer_safe(cod_ense2),
        cod_ense3 = as_integer_safe(cod_ense3),
        cod_grado = as_integer_safe(cod_grado),
        cod_grado2 = as_integer_safe(cod_grado2),
        let_cur = as_character_safe(let_cur),
        cod_jor = as_integer_safe(cod_jor),
        cod_tip_cur = as_integer_safe(cod_tip_cur),
        cod_des_cur = as_integer_safe(cod_des_cur),
        tipo_aula = as_character_safe(tipo_aula),
        mrun = as_character_safe(mrun),
        mrun_ipe = as_character_safe(mrun_ipe),
        gen_alu = as_character_safe(gen_alu),
        fec_nac_alu = as_character_safe(fec_nac_alu),
        edad_alu = as_integer_safe(edad_alu),
        cod_etnia_alu = as_integer_safe(cod_etnia_alu),
        int_alu = as_integer_safe(int_alu),
        cod_int_alu = as_integer_safe(cod_int_alu),
        cod_nac_alu = as_character_safe(cod_nac_alu),
        pais_origen_alu = as_character_safe(pais_origen_alu),
        cod_reg_alu = as_integer_safe(cod_reg_alu),
        cod_com_alu = as_integer_safe(cod_com_alu),
        nom_com_alu = as_character_safe(nom_com_alu),
        cod_sec = as_integer_safe(cod_sec),
        cod_espe = as_integer_safe(cod_espe),
        cod_rama = as_integer_safe(cod_rama),
        cod_men = as_integer_safe(cod_men),
        ens = as_integer_safe(ens),
        nationality_available = agno >= 2016L,
        mrun_ipe_available = agno >= 2018L,
        estado_estab_available = agno >= 2015L
      ) |>
      arrange(agno, rbd, mrun)
  }

  process_matricula_year <- function(path, year, force_rebuild = FALSE) {
    out_path <- file.path(interim_dir, "matricula", sprintf("matricula_student_year_%s.parquet", year))
    meta_path <- file.path(interim_dir, "matricula", sprintf("matricula_columns_%s.csv", year))

    if (file.exists(out_path) && !force_rebuild) {
      return(out_path)
    }

    header <- read_delim_header(path, delim = ";")
    select_idx <- which(header %in% matricula_keep_columns())
    write_csv(tibble(year = year, source = "matricula", column = header), meta_path)

    raw <- data.table::fread(
      path,
      sep = ";",
      select = select_idx,
      encoding = "UTF-8",
      na.strings = c("", "NA", "N/A", "NULL"),
      showProgress = FALSE,
      data.table = FALSE
    )

    out <- standardize_matricula(raw, year)
    write_parquet_safe(out, out_path)
    rm(raw, out)
    gc(verbose = FALSE)
    out_path
  }

  matricula_interim_files <- function(years = NULL) {
    files <- list.files(file.path(interim_dir, "matricula"), pattern = "matricula_student_year_\\d{4}\\.parquet$", full.names = TRUE)
    if (!is.null(years)) {
      year_pattern <- paste0("(", paste(years, collapse = "|"), ")")
      files <- files[str_detect(basename(files), year_pattern)]
    }
    if (length(files) == 0) {
      stop("MINEDUC matricula: no hay archivos intermedios para consolidar.")
    }
    sort(files)
  }

  build_mrun_ipe_bridge <- function(matricula_panel) {
    dt <- data.table::as.data.table(matricula_panel)
    out <- unique(dt[!is.na(mrun_ipe) & !is.na(mrun), .(mrun_ipe, mrun)])
    tibble::as_tibble(out) |>
      arrange(mrun_ipe, mrun)
  }

  build_matricula_school_year <- function(matricula_panel) {
    dt <- data.table::as.data.table(matricula_panel)
    out <- dt[
      ,
      .(
        n_records = .N,
        n_students = uniqueN(mrun[!is.na(mrun)]),
        n_students_with_mrun_ipe = uniqueN(mrun_ipe[!is.na(mrun_ipe)]),
        n_students_nationality_observed = uniqueN(mrun[!is.na(mrun) & !is.na(cod_nac_alu)])
      ),
      by = .(
        agno, rbd, nom_rbd, cod_reg_rbd, nom_reg_rbd_a, cod_com_rbd, nom_com_rbd,
        cod_depe2, rural_rbd, estado_estab
      )
    ]
    tibble::as_tibble(out) |>
      arrange(agno, rbd)
  }

  build_matricula_year_summary <- function(matricula_panel) {
    dt <- data.table::as.data.table(matricula_panel)
    out <- dt[
      ,
      .(
        n_records = .N,
        n_students = uniqueN(mrun[!is.na(mrun)]),
        n_students_missing_mrun = sum(is.na(mrun)),
        n_students_with_mrun_ipe = uniqueN(mrun_ipe[!is.na(mrun_ipe)]),
        n_rbd = uniqueN(rbd[!is.na(rbd)]),
        n_missing_rbd = sum(is.na(rbd)),
        n_active_records = sum(estado_estab == 1L, na.rm = TRUE),
        n_non_active_records = sum(!is.na(estado_estab) & estado_estab != 1L),
        n_missing_estado_estab = sum(is.na(estado_estab)),
        n_missing_nationality = sum(is.na(cod_nac_alu)),
        share_missing_nationality = mean(is.na(cod_nac_alu))
      ),
      by = .(agno)
    ]
    tibble::as_tibble(out) |>
      arrange(agno)
  }

  build_matricula_duplicate_summary <- function(matricula_panel) {
    dt <- data.table::as.data.table(matricula_panel)
    out <- rbindlist(lapply(split(dt, by = "agno", keep.by = TRUE), function(year_dt) {
      student_keys <- year_dt[!is.na(mrun), .(mrun, rbd)]
      duplicated_mrun <- uniqueN(student_keys$mrun[duplicated(student_keys$mrun)])
      rbd_pairs <- unique(student_keys[!is.na(rbd), .(mrun, rbd)])
      duplicated_mrun_multiple_rbd <- rbd_pairs[, .N, by = mrun][N > 1L, .N]

      data.table(
        agno = year_dt$agno[[1]],
        duplicated_mrun = duplicated_mrun,
        duplicated_mrun_multiple_rbd = duplicated_mrun_multiple_rbd
      )
    }))
    tibble::as_tibble(out) |>
      arrange(agno)
  }

  build_matricula_nationality_summary <- function(matricula_panel) {
    dt <- data.table::as.data.table(matricula_panel)
    out <- dt[
      !is.na(cod_nac_alu) & !is.na(mrun),
      .(n_students = uniqueN(mrun)),
      by = .(agno, cod_nac_alu, pais_origen_alu)
    ]
    tibble::as_tibble(out) |>
      arrange(agno, desc(n_students), cod_nac_alu, pais_origen_alu)
  }

  build_matricula_dependency_summary <- function(matricula_panel) {
    dt <- data.table::as.data.table(matricula_panel)
    out <- dt[
      ,
      .(
        n_students = uniqueN(mrun[!is.na(mrun)]),
        n_rbd = uniqueN(rbd[!is.na(rbd)])
      ),
      by = .(agno, cod_depe2)
    ]
    tibble::as_tibble(out) |>
      arrange(agno, cod_depe2)
  }

  build_matricula_quality_notes <- function(year_summary, duplicate_summary) {
    quality <- left_join(year_summary, duplicate_summary, by = "agno") |>
      mutate(share_missing_nationality = round(100 * share_missing_nationality, 2))

    lines <- c(
      "# Diagnostico de calidad: Matricula",
      "",
      "- El panel final filtra `ESTADO_ESTAB == 1` desde 2015 en adelante.",
      "- Para anios previos a 2015, `ESTADO_ESTAB` no esta disponible.",
      ""
    )

    for (i in seq_len(nrow(quality))) {
      row <- quality[i, ]
      lines <- c(
        lines,
        sprintf(
          "- %s: %s estudiantes, %s establecimientos, %s MRUN duplicados, %s MRUN con multiples RBD, %s%% missing en nacionalidad.",
          row$agno,
          format(row$n_students, big.mark = ","),
          format(row$n_rbd, big.mark = ","),
          format(row$duplicated_mrun, big.mark = ","),
          format(row$duplicated_mrun_multiple_rbd, big.mark = ","),
          format(row$share_missing_nationality, nsmall = 2)
        )
      )
    }

    lines
  }

  process_matricula_pipeline <- function(force_rebuild = FALSE, years = NULL) {
    inventory <- discover_matricula_files()
    if (!is.null(years)) {
      inventory <- filter(inventory, year %in% years)
    }
    if (nrow(inventory) == 0) {
      stop("MINEDUC matricula: no se detectaron archivos raw.")
    }

    purrr::walk2(
      inventory$path,
      inventory$year,
      ~ process_matricula_year(path = .x, year = .y, force_rebuild = force_rebuild)
    )

    files <- matricula_interim_files(years = inventory$year)
    school_parts <- list()
    bridge_parts <- list()
    year_summary_parts <- list()
    duplicate_parts <- list()
    nationality_parts <- list()
    dependency_parts <- list()

    for (i in seq_along(files)) {
      message("MINEDUC matricula: resumen anual ", basename(files[[i]]))
      year_df <- arrow::read_parquet(files[[i]]) |>
        maybe_filter_active_establishments()

      school_parts[[i]] <- build_matricula_school_year(year_df)
      bridge_parts[[i]] <- build_mrun_ipe_bridge(year_df)
      year_summary_parts[[i]] <- build_matricula_year_summary(year_df)
      duplicate_parts[[i]] <- build_matricula_duplicate_summary(year_df)
      nationality_parts[[i]] <- build_matricula_nationality_summary(year_df)
      dependency_parts[[i]] <- build_matricula_dependency_summary(year_df)

      rm(year_df)
      gc(verbose = FALSE)
    }

    filtered_dataset <- arrow::open_dataset(files) |>
      filter((is.na(estado_estab) & agno < 2015L) | (!is.na(estado_estab) & estado_estab == 1L))

    write_parquet_safe(
      arrow::as_record_batch_reader(filtered_dataset),
      file.path(final_dir, "matricula_student_year.parquet")
    )

    school_panel <- bind_rows(school_parts) |>
      arrange(agno, rbd)
    bridge <- bind_rows(bridge_parts) |>
      distinct(mrun_ipe, mrun) |>
      arrange(mrun_ipe, mrun)
    availability <- build_availability_table(file.path(interim_dir, "matricula"), "^matricula_columns_\\d{4}\\.csv$", "matricula")
    year_summary <- bind_rows(year_summary_parts) |>
      arrange(agno)
    duplicate_summary <- bind_rows(duplicate_parts) |>
      arrange(agno)
    nationality_summary <- bind_rows(nationality_parts) |>
      arrange(agno, desc(n_students), cod_nac_alu, pais_origen_alu)
    dependency_summary <- bind_rows(dependency_parts) |>
      arrange(agno, cod_depe2)
    quality_lines <- build_matricula_quality_notes(year_summary, duplicate_summary)

    write_parquet_safe(school_panel, file.path(final_dir, "matricula_school_year.parquet"))
    write_parquet_safe(bridge, file.path(final_dir, "mrun_ipe_bridge.parquet"))
    write_csv(availability, file.path(final_dir, "matricula_variable_availability.csv"))
    write_csv(year_summary, file.path(final_dir, "matricula_year_summary.csv"))
    write_csv(duplicate_summary, file.path(final_dir, "matricula_duplicate_summary.csv"))
    write_csv(nationality_summary, file.path(final_dir, "matricula_nationality_summary.csv"))
    write_csv(dependency_summary, file.path(final_dir, "matricula_dependency_summary.csv"))
    message(paste(quality_lines, collapse = "\n"))

    list(
      inventory = inventory,
      school = school_panel,
      bridge = bridge,
      availability = availability,
      year_summary = year_summary,
      duplicate_summary = duplicate_summary,
      nationality_summary = nationality_summary,
      dependency_summary = dependency_summary
    )
  }

  # ---------------------------------------------------------------------------
  # JUNJI
  # ---------------------------------------------------------------------------

  junji_expected_columns <- c(
    "agno", "cod_region", "region", "comuna", "nivel", "programa", "modalidad", "nacionalidad_raw"
  )

  junji_region_code_map <- c(
    `15` = "Arica y Parinacota",
    `1` = "Tarapaca",
    `2` = "Antofagasta",
    `3` = "Atacama",
    `4` = "Coquimbo",
    `5` = "Valparaiso",
    `13` = "Metropolitana",
    `6` = "O'Higgins",
    `7` = "Maule",
    `16` = "Nuble",
    `8` = "Biobio",
    `9` = "La Araucania",
    `14` = "Los Rios",
    `10` = "Los Lagos",
    `11` = "Aysen",
    `12` = "Magallanes"
  )

  discover_junji_files <- function() {
    base_dir <- file.path(raw_dir, "junji")
    if (!dir.exists(base_dir)) {
      return(tibble())
    }

    files <- list.files(base_dir, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    tibble(path = files) |>
      filter(
        !stringr::str_detect(basename(path), "^\\._"),
        stringr::str_detect(basename(path), "SAIP|transparencia|migr")
      ) |>
      mutate(
        file_name = basename(path),
        source = "junji",
        year = NA_integer_
      ) |>
      distinct(path, file_name, source, year) |>
      arrange(file_name)
  }

  standardize_junji_region <- function(region, cod_region) {
    region_raw <- trim_na(region)
    region_ascii <- to_ascii_upper(region_raw)

    inferred <- dplyr::recode(as.character(cod_region), !!!junji_region_code_map, .default = NA_character_)
    from_text <- case_when(
      is.na(region_ascii) ~ NA_character_,
      region_ascii %in% c("ARICA Y PARINACOTA") ~ "Arica y Parinacota",
      region_ascii %in% c("TARAPACA") ~ "Tarapaca",
      region_ascii %in% c("ANTOFAGASTA") ~ "Antofagasta",
      region_ascii %in% c("ATACAMA") ~ "Atacama",
      region_ascii %in% c("COQUIMBO") ~ "Coquimbo",
      region_ascii %in% c("VALPARAISO") ~ "Valparaiso",
      region_ascii %in% c("METROPOLITANA", "REGION METROPOLITANA", "REGION METROPOLITANA DE SANTIAGO") ~ "Metropolitana",
      region_ascii %in% c("O'HIGGINS", "OHIGGINS", "LIBERTADOR GENERAL BERNARDO O'HIGGINS") ~ "O'Higgins",
      region_ascii %in% c("MAULE") ~ "Maule",
      region_ascii %in% c("NUBLE", "NO_ENCONTRADO", "NO ENCONTRADO") & cod_region == 16L ~ "Nuble",
      region_ascii %in% c("BIO BIO", "BIOBIO", "BIO B?O", "BI O BIO") ~ "Biobio",
      region_ascii %in% c("LA ARAUCANIA") ~ "La Araucania",
      region_ascii %in% c("LOS RIOS") ~ "Los Rios",
      region_ascii %in% c("LOS LAGOS") ~ "Los Lagos",
      region_ascii %in% c("AYSEN") ~ "Aysen",
      region_ascii %in% c("MAGALLANES", "MAGALLANES Y ANTARTICA CHILENA") ~ "Magallanes",
      TRUE ~ NA_character_
    )

    coalesce(from_text, inferred)
  }

  standardize_junji_comuna <- function(x) {
    out <- trim_na(x)
    out <- stringr::str_to_title(stringr::str_to_lower(out))
    dplyr::recode(
      out,
      "Nunoa" = "Ñuñoa",
      "Penalolen" = "Peñalolén",
      "Vina Del Mar" = "Viña del Mar",
      "Maipu" = "Maipú",
      "Conchali" = "Conchalí",
      "Quinta Normal" = "Quinta Normal",
      "San Joaquin" = "San Joaquín",
      "San Ramon" = "San Ramón",
      "Padre Las Casas" = "Padre Las Casas",
      "Paine" = "Paine",
      .default = out
    )
  }

  standardize_junji_nationality <- function(x) {
    raw <- trim_na(x)
    ascii <- to_ascii_upper(raw)

    case_when(
      is.na(ascii) ~ "No informado",
      ascii %in% c("CHILE", "CHILENO/A", "CHILENA/O", "CHILENO", "CHILENA") ~ "Chile",
      ascii %in% c("NO REGISTRADO", "NO INFORMADO", "SIN INFORMACION", "NO REGISTRA") ~ "No informado",
      ascii %in% c("REPUBLICA DOMINICANA") ~ "República Dominicana",
      ascii %in% c("ESPANA") ~ "España",
      ascii %in% c("HAITI") ~ "Haití",
      ascii %in% c("MEXICO") ~ "México",
      ascii %in% c("PERU") ~ "Perú",
      ascii %in% c("OTROS SUDAMERICANOS") ~ "Otros sudamericanos",
      ascii %in% c("OTROS EUROPEOS") ~ "Otros europeos",
      ascii %in% c("OTROS NORTEAMERICANOS") ~ "Otros norteamericanos",
      ascii %in% c("ASIA") ~ "Asia",
      ascii %in% c("OCEANIA") ~ "Oceanía",
      TRUE ~ stringr::str_to_title(stringr::str_to_lower(raw))
    )
  }

  classify_junji_migrant_status <- function(nationality) {
    case_when(
      is.na(nationality) ~ "No informado",
      nationality == "Chile" ~ "Nacional",
      nationality == "No informado" ~ "No informado",
      TRUE ~ "Migrante"
    )
  }

  standardize_junji_level <- function(x) {
    raw <- trim_na(x)
    ascii <- to_ascii_upper(raw)

    case_when(
      ascii %in% c("SALA CUNA MENOR") ~ "Sala cuna menor",
      ascii %in% c("SALA CUNA MAYOR") ~ "Sala cuna mayor",
      ascii %in% c("SALA CUNA HETEROGENEA") ~ "Sala cuna heterogénea",
      ascii %in% c("NIVEL MEDIO MENOR") ~ "Nivel medio menor",
      ascii %in% c("NIVEL MEDIO MAYOR") ~ "Nivel medio mayor",
      ascii %in% c("NIVEL MEDIO MIXTO") ~ "Nivel medio mixto",
      ascii %in% c("TRANSICION I") ~ "Transición I",
      ascii %in% c("MEDIO Y TRANS. CONVENCIONAL", "MEDIO Y TRANS CONVENCIONAL") ~ "Medio y transición convencional",
      ascii %in% c("MEDIO Y TRANS. NO CONVENCIONAL", "MEDIO Y TRANS NO CONVENCIONAL") ~ "Medio y transición no convencional",
      TRUE ~ stringr::str_to_sentence(stringr::str_to_lower(raw))
    )
  }

  classify_junji_level_group <- function(level) {
    ascii <- to_ascii_upper(level)
    case_when(
      stringr::str_detect(ascii, "SALA CUNA") ~ "Sala cuna",
      stringr::str_detect(ascii, "NIVEL MEDIO") ~ "Nivel medio",
      stringr::str_detect(ascii, "TRANSIC") ~ "Transicion",
      stringr::str_detect(ascii, "MEDIO Y TRANS") ~ "Medio y transicion",
      TRUE ~ "Otros"
    )
  }

  standardize_junji_program <- function(x) {
    raw <- trim_na(x)
    ascii <- to_ascii_upper(raw)

    case_when(
      is.na(ascii) ~ "Sin informacion",
      stringr::str_detect(ascii, "CLASICO") & stringr::str_detect(ascii, "ADM") ~ "Jardín infantil clásico de administración directa",
      stringr::str_detect(ascii, "ALTERNATIVO") ~ "Jardín infantil alternativo",
      TRUE ~ stringr::str_to_sentence(stringr::str_to_lower(raw))
    )
  }

  classify_junji_program_group <- function(program) {
    ascii <- to_ascii_upper(program)
    case_when(
      stringr::str_detect(ascii, "CLASICO") ~ "Clasico administracion directa",
      stringr::str_detect(ascii, "ALTERNATIVO") ~ "Alternativo",
      TRUE ~ "Otros"
    )
  }

  standardize_junji_modality <- function(x) {
    raw <- trim_na(x)
    ascii <- to_ascii_upper(raw)

    case_when(
      is.na(ascii) ~ "Sin informacion",
      ascii %in% c("JARDIN INFANTIL") ~ "Jardín infantil",
      ascii %in% c("JARDIN LABORAL") ~ "Jardín laboral",
      ascii %in% c("JARDIN FAMILIAR") ~ "Jardín familiar",
      ascii %in% c("JARDIN ETNICO") ~ "Jardín étnico",
      ascii %in% c("CECI") ~ "CECI",
      ascii %in% c("PMI") ~ "PMI",
      TRUE ~ stringr::str_to_sentence(stringr::str_to_lower(raw))
    )
  }

  classify_junji_modality_group <- function(modality) {
    ascii <- to_ascii_upper(modality)
    case_when(
      stringr::str_detect(ascii, "JARDIN INFANTIL") ~ "Jardín infantil",
      stringr::str_detect(ascii, "JARDIN LABORAL") ~ "Jardín laboral",
      stringr::str_detect(ascii, "JARDIN FAMILIAR") ~ "Jardín familiar",
      stringr::str_detect(ascii, "JARDIN ETNICO") ~ "Jardín étnico",
      stringr::str_detect(ascii, "CECI") ~ "CECI",
      stringr::str_detect(ascii, "PMI") ~ "PMI",
      TRUE ~ "Otros"
    )
  }

  read_junji_sheet <- function(path, sheet) {
    out <- readxl::read_excel(
      path,
      sheet = sheet,
      col_names = FALSE,
      col_types = rep("text", 8),
      .name_repair = "minimal"
    )

    if (ncol(out) < 8) {
      stop("MINEDUC JUNJI: la hoja `", sheet, "` tiene menos de 8 columnas.")
    }

    out <- out[, seq_along(junji_expected_columns), drop = FALSE]
    names(out) <- junji_expected_columns
    out[-1, , drop = FALSE]
  }

  standardize_junji <- function(df, year) {
    out <- df |>
      mutate(across(everything(), as_character_safe)) |>
      filter(if_any(everything(), ~ !is.na(.x))) |>
      mutate(
        agno = coalesce(as_integer_safe(agno), year),
        cod_region = as_integer_safe(cod_region),
        region = standardize_junji_region(region, cod_region),
        comuna = standardize_junji_comuna(comuna),
        nivel = standardize_junji_level(nivel),
        programa = standardize_junji_program(programa),
        modalidad = standardize_junji_modality(modalidad),
        nacionalidad_raw = as_character_safe(nacionalidad_raw),
        nacionalidad = standardize_junji_nationality(nacionalidad_raw),
        migrant_status = classify_junji_migrant_status(nacionalidad),
        migrant_flag = case_when(
          migrant_status == "Migrante" ~ 1L,
          migrant_status == "Nacional" ~ 0L,
          TRUE ~ NA_integer_
        ),
        level_group = classify_junji_level_group(nivel),
        program_group = classify_junji_program_group(programa),
        modality_group = classify_junji_modality_group(modalidad),
        region_display = display_region(normalize_region(region))
      ) |>
      arrange(agno, cod_region, comuna, nacionalidad)

    out
  }

  process_junji_year <- function(path, year, force_rebuild = FALSE) {
    out_path <- file.path(interim_dir, "junji", sprintf("junji_student_year_%s.parquet", year))
    meta_path <- file.path(interim_dir, "junji", sprintf("junji_columns_%s.csv", year))

    if (file.exists(out_path) && !force_rebuild) {
      return(out_path)
    }

    raw <- read_junji_sheet(path, sheet = as.character(year))
    write_csv(tibble(year = year, source = "junji", column = names(raw)), meta_path)
    out <- standardize_junji(raw, year)
    write_parquet_safe(out, out_path)
    rm(raw, out)
    gc(verbose = FALSE)
    out_path
  }

  junji_interim_files <- function(years = NULL) {
    files <- list.files(file.path(interim_dir, "junji"), pattern = "junji_student_year_\\d{4}\\.parquet$", full.names = TRUE)
    if (!is.null(years)) {
      year_pattern <- paste0("(", paste(years, collapse = "|"), ")")
      files <- files[str_detect(basename(files), year_pattern)]
    }
    if (length(files) == 0) {
      stop("MINEDUC JUNJI: no hay archivos intermedios para consolidar.")
    }
    sort(files)
  }

  build_junji_center_year <- function(junji_panel) {
    dt <- data.table::as.data.table(junji_panel)
    out <- dt[
      ,
      .(
        n_children = .N,
        migrant_children = sum(migrant_flag == 1L, na.rm = TRUE),
        national_children = sum(migrant_flag == 0L, na.rm = TRUE),
        no_info_children = sum(is.na(migrant_flag))
      ),
      by = .(
        agno, cod_region, region, region_display, comuna, level_group, program_group, modality_group
      )
    ]

    tibble::as_tibble(out) |>
      mutate(migrant_share = migrant_children / if_else(national_children + migrant_children > 0, national_children + migrant_children, NA_real_)) |>
      arrange(agno, cod_region, comuna, level_group, modality_group)
  }

  build_junji_year_summary <- function(junji_panel) {
    dt <- data.table::as.data.table(junji_panel)

    out <- dt[
      ,
      .(
        n_records = .N,
        n_regions = uniqueN(region[!is.na(region)]),
        n_comunas = uniqueN(comuna[!is.na(comuna)]),
        migrant_children = sum(migrant_flag == 1L, na.rm = TRUE),
        national_children = sum(migrant_flag == 0L, na.rm = TRUE),
        no_info_children = sum(is.na(migrant_flag)),
        share_nationality_missing = mean(is.na(migrant_flag))
      ),
      by = .(agno)
    ]

    tibble::as_tibble(out) |>
      mutate(
        n_children = migrant_children + national_children + no_info_children,
        migrant_share = migrant_children / if_else(migrant_children + national_children > 0, migrant_children + national_children, NA_real_)
      ) |>
      arrange(agno) |>
      select(agno, n_records, n_children, n_regions, n_comunas, migrant_children, national_children, no_info_children, migrant_share, share_nationality_missing)
  }

  build_junji_nationality_summary <- function(junji_panel) {
    dt <- data.table::as.data.table(junji_panel)
    out <- dt[
      migrant_status == "Migrante" & !is.na(nacionalidad),
      .(n_children = .N),
      by = .(agno, nacionalidad)
    ]
    tibble::as_tibble(out) |>
      arrange(agno, desc(n_children), nacionalidad)
  }

  extract_junji_lookup_rows <- function(df, year) {
    bind_rows(
      df |>
        filter(!is.na(nivel)) |>
        count(domain = "nivel", label = nivel, name = "n") |>
        mutate(year = year),
      df |>
        filter(!is.na(level_group)) |>
        count(domain = "nivel_grupo", label = level_group, name = "n") |>
        mutate(year = year),
      df |>
        filter(!is.na(programa)) |>
        count(domain = "programa", label = programa, name = "n") |>
        mutate(year = year),
      df |>
        filter(!is.na(program_group)) |>
        count(domain = "programa_grupo", label = program_group, name = "n") |>
        mutate(year = year),
      df |>
        filter(!is.na(modalidad)) |>
        count(domain = "modalidad", label = modalidad, name = "n") |>
        mutate(year = year),
      df |>
        filter(!is.na(modality_group)) |>
        count(domain = "modalidad_grupo", label = modality_group, name = "n") |>
        mutate(year = year),
      df |>
        filter(!is.na(nacionalidad)) |>
        count(domain = "nacionalidad", label = nacionalidad, name = "n") |>
        mutate(year = year)
    )
  }

  build_junji_lookup_table <- function(lookup_parts) {
    bind_rows(lookup_parts) |>
      group_by(domain, label) |>
      summarise(n = sum(n), .groups = "drop") |>
      arrange(domain, desc(n), label)
  }

  build_junji_code_lookups <- function(lookup_long) {
    lookup_long |>
      group_by(domain) |>
      mutate(rank = row_number(desc(n))) |>
      ungroup() |>
      arrange(domain, rank, label)
  }

  build_junji_quality_notes <- function(year_summary) {
    lines <- c(
      "# Diagnostico de calidad: JUNJI",
      "",
      "- La serie raw detectada cubre 2011-2025 con una hoja por anio.",
      "- La unidad de observacion es un registro individual de matricula parvularia con region, comuna, nivel, programa, modalidad y nacionalidad.",
      "- La base no incluye identificador de establecimiento ni identificador de nino o nina, por lo que no permite trayectorias longitudinales ni concentracion por centro.",
      "- En 2025 aparece `No_encontrado` para region con codigo 16. Se homologa a Nuble usando el codigo regional.",
      ""
    )

    for (i in seq_len(nrow(year_summary))) {
      row <- year_summary[i, ]
      lines <- c(
        lines,
        sprintf(
          "- %s: %s registros, %s regiones, %s comunas, %s estudiantes migrantes y %s%% sin nacionalidad informativa.",
          row$agno,
          format(row$n_children, big.mark = ","),
          format(row$n_regions, big.mark = ","),
          format(row$n_comunas, big.mark = ","),
          format(row$migrant_children, big.mark = ","),
          format(round(100 * row$share_nationality_missing, 2), nsmall = 2)
        )
      )
    }

    lines
  }

  process_junji_pipeline <- function(force_rebuild = FALSE, years = NULL) {
    inventory <- discover_junji_files()
    if (nrow(inventory) == 0) {
      stop("MINEDUC JUNJI: no se detectaron archivos raw en `data/raw/mineduc/junji/`.")
    }

    workbook_path <- inventory$path[[1]]
    workbook_sheets <- readxl::excel_sheets(workbook_path)
    workbook_years <- suppressWarnings(as.integer(workbook_sheets))
    sheet_inventory <- tibble(sheet = workbook_sheets, year = workbook_years) |>
      filter(!is.na(year))

    if (!is.null(years)) {
      sheet_inventory <- filter(sheet_inventory, year %in% years)
    }
    if (nrow(sheet_inventory) == 0) {
      stop("MINEDUC JUNJI: no se detectaron hojas validas para los anios solicitados.")
    }

    purrr::walk2(
      rep(workbook_path, nrow(sheet_inventory)),
      sheet_inventory$year,
      ~ process_junji_year(path = .x, year = .y, force_rebuild = force_rebuild)
    )

    files <- junji_interim_files(years = sheet_inventory$year)
    center_parts <- list()
    year_summary_parts <- list()
    nationality_parts <- list()
    lookup_parts <- list()

    for (i in seq_along(files)) {
      message("MINEDUC JUNJI: resumen anual ", basename(files[[i]]))
      year_df <- arrow::read_parquet(files[[i]])
      center_parts[[i]] <- build_junji_center_year(year_df)
      year_summary_parts[[i]] <- build_junji_year_summary(year_df)
      nationality_parts[[i]] <- build_junji_nationality_summary(year_df)
      lookup_parts[[i]] <- extract_junji_lookup_rows(year_df, year = unique(year_df$agno))
      rm(year_df)
      gc(verbose = FALSE)
    }

    write_parquet_safe(
      arrow::as_record_batch_reader(arrow::open_dataset(files)),
      file.path(final_dir, "junji_student_year.parquet")
    )

    center_panel <- bind_rows(center_parts) |>
      arrange(agno, cod_region, comuna, level_group, modality_group)
    year_summary <- bind_rows(year_summary_parts) |>
      arrange(agno)
    nationality_summary <- bind_rows(nationality_parts) |>
      arrange(agno, desc(n_children), nacionalidad)
    availability <- build_availability_table(file.path(interim_dir, "junji"), "^junji_columns_\\d{4}\\.csv$", "junji")
    lookup_long <- build_junji_lookup_table(lookup_parts)
    code_lookups <- build_junji_code_lookups(lookup_long)
    quality_lines <- build_junji_quality_notes(year_summary)

    write_parquet_safe(center_panel, file.path(final_dir, "junji_center_year.parquet"))
    write_csv(year_summary, file.path(final_dir, "junji_year_summary.csv"))
    write_csv(nationality_summary, file.path(final_dir, "junji_nationality_summary.csv"))
    write_csv(availability, file.path(final_dir, "junji_variable_availability.csv"))
    write_csv(lookup_long, file.path(final_dir, "junji_lookup_long.csv"))
    write_csv(code_lookups, file.path(final_dir, "junji_code_lookups.csv"))
    message(paste(quality_lines, collapse = "\n"))

    list(
      inventory = inventory,
      center = center_panel,
      year_summary = year_summary,
      nationality_summary = nationality_summary,
      availability = availability,
      code_lookups = code_lookups
    )
  }

  # ---------------------------------------------------------------------------
  # Validaciones metodológicas complementarias
  # ---------------------------------------------------------------------------

  write_junji_diagnostic <- function(junji_summary, code_lookups) {
    years <- paste(range(junji_summary$agno, na.rm = TRUE), collapse = "-")
    top_domains <- code_lookups |>
      count(domain, name = "n_codes") |>
      arrange(domain)

    lines <- c(
      "# Diagnostico JUNJI",
      "",
      "## Que contienen las bases",
      "",
      sprintf("- El workbook raw detectado cubre `%s` y trae una hoja por anio.", years),
      "- La unidad de observacion es un registro individual de matricula parvularia.",
      "- La base contiene ocho variables base: anio, codigo regional, region, comuna, nivel, programa, modalidad y nacionalidad.",
      "- La informacion territorial es suficiente para analisis por region y comuna.",
      "- La base no contiene identificador de establecimiento ni identificador individual, por lo que no sirve para trayectorias ni concentracion por centro.",
      "",
      "## Que se puede construir",
      "",
      "- Panel `junji_student_year.parquet` con registros armonizados 2011-2025.",
      "- Panel `junji_center_year.parquet` como agregado territorial por region, comuna, nivel, programa y modalidad.",
      "- Series de matricula migrante, composicion por nacionalidad, distribucion regional y estructura por nivel.",
      "- Tablas de homologacion de nacionalidad, nivel, programa y modalidad para apoyar reproducibilidad.",
      "",
      "## Limitaciones",
      "",
      "- La base no identifica establecimientos, por lo que no permite medir concentracion por jardin ni hacer panel centro-anio estricto.",
      "- Algunas nacionalidades aparecen como agregados amplios (`Otros sudamericanos`, `Asia`, `Oceania`), por lo que el analisis de origen debe distinguir entre paises y categorias residuales.",
      "- En algunos registros la nacionalidad aparece como `No registrado`, que se homologa a `No informado`.",
      "- En 2025 la region con codigo 16 aparece como `No_encontrado`; se corrige usando el codigo regional y se asigna a Nuble.",
      "",
      "## Dialogo con matricula MINEDUC",
      "",
      "- Matricula por estudiante sigue siendo la fuente principal para analisis migratorio escolar.",
      "- JUNJI ahora complementa esa lectura con una capa de educacion parvularia migrante 2011-2025.",
      "- Ambas fuentes dialogan bien para mostrar continuidad descriptiva entre primera infancia, transicion y escolaridad regular, pero no admiten merge individual.",
      "",
      "## Estructura de categorias observada",
      ""
    )

    for (i in seq_len(nrow(top_domains))) {
      lines <- c(lines, sprintf("- `%s`: %s categorias observadas.", top_domains$domain[i], format(top_domains$n_codes[i], big.mark = ",")))
    }

    invisible(lines)
  }

  write_education_proposals <- function() {
    lines <- c(
      "# Análisis descriptivos disponibles: educación y primera infancia",
      "",
      "| Indicador | Utilidad | Factibilidad | Requerimientos de datos |",
      "|---|---|---|---|",
      "| Evolucion de matricula migrante total | Abre la seccion y resume crecimiento reciente | Alta | Matricula final |",
      "| Matricula migrante por nivel educativo | Permite comparar parvularia escolar, basica y media | Alta | Matricula final |",
      "| Piramide educativa por sexo y condicion migratoria | Resume diferencias de estructura entre migrantes y nacionales | Alta | Matricula final con sexo y grado |",
      "| Evolucion de matricula migrante JUNJI | Conecta primera infancia con la trayectoria escolar | Alta | JUNJI final |",
      "| Nacionalidades migrantes en JUNJI | Muestra composicion temprana de origen | Alta | JUNJI final |",
      "| Distribucion regional de matricula migrante JUNJI | Permite lectura territorial comparable con matricula escolar | Alta | JUNJI final |",
      "| Distribucion de matricula migrante JUNJI por nivel | Resume donde se concentra la primera infancia migrante | Alta | JUNJI final |",
      "| Matricula migrante por comuna y region, cruzada con nivel | Abre analisis territoriales mas finos | Media | Matricula final + agregaciones adicionales |",
      "| Continuidad descriptiva entre transicion y 1 basico | Potencial narrativo para futuros briefs | Media | Matricula final + JUNJI final, sin merge individual |",
      "| Cruce entre modalidad y nacionalidad en JUNJI | Puede revelar perfiles institucionales diferenciados | Media | JUNJI final + homologacion de modalidad |",
      "| Continuidad territorial entre JUNJI y escolaridad | Potencial para futuros briefs y comparaciones comunales | Media | JUNJI final + matricula final |"
    )
    invisible(lines)
  }

  write_policy_brief_candidates <- function(hallazgos = NULL) {
    lines <- c(
      "# Selección de figuras con potencial de comunicación pública",
      "",
      "## Figuras con mayor potencial comunicacional",
      "",
      "- **Evolucion de la matricula migrante en Chile**: muy fuerte para abrir un brief porque muestra volumen y cambio relativo en una sola lectura.",
      "- **Matricula migrante por nivel educativo**: permite mostrar si el crecimiento se concentra en tramos especificos de la trayectoria escolar.",
      "- **Concentracion escolar de estudiantes migrantes**: tiene alto potencial para discutir distribucion desigual entre establecimientos.",
      "- **Evolucion de la matricula migrante JUNJI**: conecta primera infancia y migracion en una serie larga 2011-2025.",
      "- **Principales nacionalidades migrantes en JUNJI**: sirve para comparar origenes tempranos con matricula escolar.",
      "- **Distribucion de la matricula migrante JUNJI por nivel**: ayuda a comunicar en que tramos se concentra la primera infancia migrante.",
      "- **Piramide educativa por sexo y condicion migratoria**: ofrece una visual fuerte para comparar estructura por curso y genero.",
      "",
      "## Mensajes centrales posibles",
      "",
      "- El crecimiento de la matricula migrante ya no es un fenomeno marginal dentro del sistema escolar.",
      "- La experiencia educativa migrante cambia segun nivel, territorio y tipo de establecimiento.",
      "- La primera infancia oficial y la escolaridad regular muestran arquitecturas distintas, pero conectadas en la trayectoria temprana.",
      "- La concentracion escolar puede convertirse en una entrada potente para discutir segregacion y distribucion territorial.",
      "",
      "## Limitaciones",
      "",
      "- No se recomienda mezclar mecanicamente registros de JUNJI y matricula escolar a nivel individual sin evaluacion adicional.",
      "- JUNJI no trae identificador individual ni identificador de establecimiento, por lo que varias lecturas deben quedar en clave descriptiva.",
      "- Algunas nacionalidades en JUNJI aparecen como agregados amplios (`Otros sudamericanos`, `Asia`, `Oceania`).",
      "",
      "## Analisis futuros recomendados",
      "",
      "- Cruces mas finos entre territorio, nivel y dependencia en matricula migrante.",
      "- Estudio comunal de parvularia escolar vs primera infancia JUNJI.",
      "- Exploracion de continuidad entre transicion, prekinder, kinder y 1 basico.",
      "- Cruces entre modalidad, programa y nacionalidad en JUNJI."
    )

    if (!is.null(hallazgos) && length(hallazgos)) {
      lines <- c(lines, "", "## Hallazgos descriptivos destacados", "", paste0("- ", hallazgos))
    }

    invisible(lines)
  }

  # ---------------------------------------------------------------------------
  # Figuras y tablas
  # ---------------------------------------------------------------------------

  regular_grade_labels <- c(
    "Prekinder", "Kinder",
    "1 basico", "2 basico", "3 basico", "4 basico", "5 basico", "6 basico", "7 basico", "8 basico",
    "1 medio", "2 medio", "3 medio", "4 medio"
  )

  build_regular_grade <- function(ens, cod_grado) {
    case_when(
      ens == 1L & cod_grado == 4L ~ "Prekinder",
      ens == 1L & cod_grado == 5L ~ "Kinder",
      ens == 3L & cod_grado %in% 1:8 ~ paste(cod_grado, "basico"),
      ens %in% c(5L, 6L, 7L, 8L) & cod_grado %in% 1:4 ~ paste(cod_grado, "medio"),
      TRUE ~ NA_character_
    )
  }

  build_education_tranche <- function(ens, cod_grado) {
    case_when(
      ens == 1L & cod_grado %in% c(4L, 5L) ~ "Parvularia escolar",
      ens == 3L ~ "Basica",
      ens %in% c(5L, 6L, 7L, 8L) ~ "Media",
      TRUE ~ NA_character_
    )
  }

  build_gender_label <- function(gen_alu) {
    case_when(
      gen_alu %in% c("1", "M", "m") ~ "Hombres",
      gen_alu %in% c("2", "F", "f") ~ "Mujeres",
      TRUE ~ "Sin informacion"
    )
  }

  generate_education_figures <- function() {
    student_path <- file.path(final_dir, "matricula_student_year.parquet")
    school_path <- file.path(final_dir, "matricula_school_year.parquet")
    junji_student_path <- file.path(final_dir, "junji_student_year.parquet")
    junji_center_path <- file.path(final_dir, "junji_center_year.parquet")

    if (!file.exists(student_path) || !file.exists(school_path)) {
      stop("MINEDUC: faltan paneles finales de matricula en `data/final/mineduc/`.", call. = FALSE)
    }

    has_junji <- file.exists(junji_student_path) && file.exists(junji_center_path)

    fig_series <- out_fig
    tab_series <- out_tab
    fig_snapshot <- out_fig
    tab_snapshot <- out_tab
    fig_pathways <- out_fig
    tab_pathways <- out_tab
    fig_early_series <- out_fig
    tab_early_series <- out_tab
    fig_early_snapshot <- out_fig
    tab_early_snapshot <- out_tab
    note_series <- final_dir
    note_snapshot <- final_dir
    note_pathways <- final_dir
    note_early_series <- final_dir
    note_early_snapshot <- final_dir

    purrr::walk(
      c(
        fig_series, tab_series, fig_snapshot, tab_snapshot,
        fig_pathways, tab_pathways, fig_early_series, tab_early_series,
        fig_early_snapshot, tab_early_snapshot
      ),
      ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
    )

    ds <- arrow::open_dataset(student_path)

    trend <- ds |>
      filter(agno >= 2016L, !is.na(cod_nac_alu)) |>
      mutate(migrant_flag = if_else(cod_nac_alu == "E", 1L, 0L)) |>
      group_by(agno) |>
      summarise(
        total_students = n(),
        migrant_students = sum(migrant_flag),
        .groups = "drop"
      ) |>
      collect() |>
      mutate(migrant_share = migrant_students / total_students)

    p_trend_n <- ggplot(trend, aes(x = agno, y = migrant_students)) +
      geom_line(color = base_palette["primary"], linewidth = 1.1) +
      geom_point(color = base_palette["primary"], size = 2.4) +
      scale_x_continuous(breaks = trend$agno) +
      scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
      labs(x = "Año", y = "Estudiantes migrantes") +
      theme_datamigra()

    p_trend_s <- ggplot(trend, aes(x = agno, y = migrant_share)) +
      geom_line(color = base_palette["accent"], linewidth = 1.1) +
      geom_point(color = base_palette["accent"], size = 2.4) +
      scale_x_continuous(breaks = trend$agno) +
      scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
      labs(x = "Año", y = "% sobre matrícula total") +
      theme_datamigra()

    plot_trend <- p_trend_n / p_trend_s + patchwork::plot_layout(heights = c(1, 1))
    export_plot(plot_trend, fig_series, "fig_1_evolucion_matricula_migrante_2016_2025", width = 9.2, height = 7.2)
    save_table(trend, tab_series, "fig_1_evolucion_matricula_migrante_2016_2025")
    write_note(
      note_series,
      "fig_1_evolucion_matricula_migrante_2016_2025",
      "Figura 1. Evolucion de la matricula migrante en Chile, 2016-2025",
      "Numero de estudiantes migrantes y porcentaje que representan sobre la matricula total desde que la variable de nacionalidad es usable en la base.",
      c(
        "Se usan registros de matricula estudiante-anio desde 2016, cuando nacionalidad y pais de origen quedan disponibles para analisis migratorio.",
        "El panel consolidado ya filtra establecimientos activos desde 2015.",
        "La figura combina volumen absoluto y peso relativo dentro de la matricula total."
      )
    )

    countries_latest <- ds |>
      filter(agno == 2025L, cod_nac_alu == "E", !is.na(pais_origen_alu), !pais_origen_alu %in% c("0", "6", "28", "29")) |>
      group_by(pais_origen_alu) |>
      summarise(n_students = n(), .groups = "drop") |>
      collect() |>
      left_join(country_map, by = "pais_origen_alu") |>
      mutate(
        pais = process_country_label(pais),
        special_flag = stringr::str_detect(to_ascii_upper(pais), "^OTRO"),
        pais = forcats::fct_reorder(pais, if_else(special_flag, -Inf, n_students))
      ) |>
      arrange(special_flag, desc(n_students)) |>
      slice_head(n = 10) |>
      mutate(pais = factor(pais, levels = rev(pais)))

    plot_countries_latest <- ggplot(countries_latest, aes(x = n_students, y = pais)) +
      geom_col(fill = base_palette["primary"], width = 0.72) +
      scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
      labs(x = "Estudiantes migrantes", y = NULL) +
      theme_datamigra()
    export_plot(plot_countries_latest, fig_snapshot, "fig_2_principales_paises_2025")
    save_table(countries_latest |> mutate(pais = as.character(pais)), tab_snapshot, "fig_2_principales_paises_2025")
    write_note(
      note_snapshot,
      "fig_2_principales_paises_2025",
      "Figura 2. Principales paises de origen de estudiantes migrantes, 2025",
      "Ranking de paises de origen de la matricula migrante en el ultimo anio disponible.",
      c(
        "Se excluyen `Sin informacion`, `Otro pais`, `Chile` y `Apatrida` del ranking principal.",
        "La figura ordena de mayor a menor segun numero de estudiantes.",
        "El pais de origen corresponde a la variable `PAIS_ORIGEN_ALU`."
      )
    )

    top_country_codes <- countries_latest |>
      arrange(desc(n_students)) |>
      slice_head(n = 8) |>
      pull(pais_origen_alu)

    country_compare <- ds |>
      filter(agno %in% c(2016L, 2025L), cod_nac_alu == "E", pais_origen_alu %in% top_country_codes) |>
      group_by(agno, pais_origen_alu) |>
      summarise(n_students = n(), .groups = "drop") |>
      collect() |>
      left_join(country_map, by = "pais_origen_alu") |>
      mutate(pais = process_country_label(pais)) |>
      group_by(agno) |>
      mutate(share_migrant = n_students / sum(n_students)) |>
      ungroup() |>
      mutate(
        anio = factor(as.character(agno), levels = c("2016", "2025")),
        pais = factor(pais, levels = rev(process_country_label(countries_latest$pais[match(top_country_codes, countries_latest$pais_origen_alu)]))),
        fill_hex = if_else(anio == "2016", base_palette["muted"], base_palette["accent"])
      )

    plot_country_compare <- ggplot(country_compare, aes(x = share_migrant, y = pais)) +
      geom_col(aes(fill = fill_hex), width = 0.68, show.legend = FALSE) +
      facet_wrap(~anio, ncol = 2) +
      scale_fill_identity() +
      scale_x_continuous(labels = label_percent(accuracy = 1)) +
      labs(x = "% dentro de la matrícula migrante", y = NULL) +
      theme_datamigra()
    export_plot(plot_country_compare, fig_series, "fig_3_composicion_nacionalidad_2016_2025", width = 9.2, height = 6.3)
    save_table(country_compare, tab_series, "fig_3_composicion_nacionalidad_2016_2025")
    write_note(
      note_series,
      "fig_3_composicion_nacionalidad_2016_2025",
      "Figura 3. Distribucion de estudiantes migrantes por nacionalidad, 2016 y 2025",
      "Comparacion de la composicion de la matricula migrante entre el primer y el ultimo anio disponible.",
      c(
        "Los paises comparados se fijan segun el top del anio 2025 para mantener una referencia estable.",
        "La metrica corresponde a participacion dentro del total de estudiantes migrantes observados en cada anio.",
        "La figura resume cambio composicional, no crecimiento absoluto."
      )
    )

    regional_2025 <- ds |>
      filter(agno == 2025L, !is.na(nom_reg_rbd_a), !is.na(cod_nac_alu)) |>
      mutate(migrant_flag = if_else(cod_nac_alu == "E", 1L, 0L)) |>
      group_by(cod_reg_rbd, nom_reg_rbd_a) |>
      summarise(total_students = n(), migrant_students = sum(migrant_flag), .groups = "drop") |>
      collect() |>
      mutate(
        migrant_share = migrant_students / total_students,
        region = normalize_region(nom_reg_rbd_a),
        region_label = factor(display_region(region), levels = display_region(region_order)),
        highlight_rm = as.character(region) == "Metropolitana"
      ) |>
      arrange(region)

    plot_region <- ggplot(regional_2025, aes(x = migrant_share, y = fct_rev(region_label), fill = highlight_rm)) +
      geom_col(width = 0.72) +
      scale_fill_manual(values = c("TRUE" = unname(base_palette["accent"]), "FALSE" = unname(base_palette["primary"])), guide = "none") +
      scale_x_continuous(labels = label_percent(accuracy = 0.1)) +
      labs(x = "% de matrícula migrante", y = NULL) +
      theme_datamigra()
    export_plot(plot_region, fig_snapshot, "fig_4_porcentaje_matricula_migrante_region_2025")
    save_table(regional_2025 |> mutate(region = as.character(region_label)), tab_snapshot, "fig_4_porcentaje_matricula_migrante_region_2025")
    write_note(
      note_snapshot,
      "fig_4_porcentaje_matricula_migrante_region_2025",
      "Figura 4. Porcentaje de matricula migrante por region, 2025",
      "Comparacion territorial del peso de la matricula migrante sobre la matricula total regional.",
      c(
        "Las regiones se ordenan de norte a sur.",
        "La Region Metropolitana se destaca solo como referencia visual, sin alterar el orden geografico.",
        "La metrica corresponde al porcentaje de estudiantes migrantes sobre la matricula total regional."
      )
    )

    comuna_2025 <- ds |>
      filter(agno == 2025L, !is.na(nom_com_rbd), !is.na(cod_nac_alu)) |>
      mutate(migrant_flag = if_else(cod_nac_alu == "E", 1L, 0L)) |>
      group_by(cod_reg_rbd, nom_reg_rbd_a, cod_com_rbd, nom_com_rbd) |>
      summarise(total_students = n(), migrant_students = sum(migrant_flag), .groups = "drop") |>
      collect() |>
      mutate(
        migrant_share = migrant_students / total_students,
        nom_com_rbd = clean_rank_label(nom_com_rbd)
      )

    commune_top <- comuna_2025 |>
      arrange(desc(migrant_students), desc(total_students)) |>
      slice_head(n = 15) |>
      mutate(comuna = forcats::fct_reorder(nom_com_rbd, migrant_students))

    plot_commune <- ggplot(commune_top, aes(x = migrant_students, y = comuna)) +
      geom_col(fill = base_palette["primary"], width = 0.72) +
      scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
      labs(x = "Estudiantes migrantes", y = NULL) +
      theme_datamigra()
    export_plot(plot_commune, fig_snapshot, "fig_5_comunas_mayor_matricula_migrante_2025")
    save_table(commune_top |> mutate(comuna = as.character(comuna)), tab_snapshot, "fig_5_comunas_mayor_matricula_migrante_2025")

    commune_large_share <- comuna_2025 |>
      filter(total_students >= 10000) |>
      arrange(desc(migrant_share), desc(migrant_students))
    write_note(
      note_snapshot,
      "fig_5_comunas_mayor_matricula_migrante_2025",
      "Figura 5. Comunas con mayor matricula migrante, 2025",
      "Ranking comunal segun numero absoluto de estudiantes migrantes en el ultimo anio disponible.",
      c(
        "La figura principal ordena comunas por numero absoluto de estudiantes migrantes.",
        "Como output secundario se exporta una tabla con porcentaje migrante para comunas con matricula total de al menos 10.000 estudiantes.",
        "La unidad territorial corresponde a la comuna del establecimiento."
      )
    )

    dependency_trend <- ds |>
      filter(agno >= 2016L, !is.na(cod_nac_alu), !is.na(cod_depe2)) |>
      mutate(migrant_flag = if_else(cod_nac_alu == "E", 1L, 0L)) |>
      group_by(agno, cod_depe2) |>
      summarise(total_students = n(), migrant_students = sum(migrant_flag), .groups = "drop") |>
      collect() |>
      left_join(dependency_map, by = "cod_depe2") |>
      filter(!is.na(dependency_group)) |>
      group_by(agno, dependency_group, dependency_order) |>
      summarise(
        total_students = sum(total_students),
        migrant_students = sum(migrant_students),
        .groups = "drop"
      ) |>
      group_by(agno) |>
      mutate(migrant_share = migrant_students / sum(migrant_students)) |>
      ungroup() |>
      mutate(
        dependency_group = factor(
          dependency_group,
          levels = c("Publica", "Particular subvencionado", "Particular pagado", "Administracion delegada")
        ),
        dependency_label = recode(
          as.character(dependency_group),
          "Publica" = "Pública",
          "Particular subvencionado" = "Particular subvencionado",
          "Particular pagado" = "Particular pagado",
          "Administracion delegada" = "Administración delegada"
        )
      )

    dep_palette <- c(
      "Pública" = unname(base_palette["primary"]),
      "Particular subvencionado" = unname(base_palette["accent"]),
      "Particular pagado" = unname(base_palette["green"]),
      "Administración delegada" = unname(base_palette["purple"])
    )

    p_dep_n <- ggplot(dependency_trend, aes(x = agno, y = migrant_students, color = dependency_label)) +
      geom_line(linewidth = 1.08) +
      geom_point(size = 2.1) +
      scale_x_continuous(breaks = sort(unique(dependency_trend$agno))) +
      scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
      scale_color_manual(values = dep_palette, drop = FALSE) +
      guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
      labs(x = "Año", y = "Estudiantes migrantes") +
      theme_datamigra()

    p_dep_s <- ggplot(dependency_trend, aes(x = agno, y = migrant_share, color = dependency_label)) +
      geom_line(linewidth = 1.08) +
      geom_point(size = 2.1) +
      scale_x_continuous(breaks = sort(unique(dependency_trend$agno))) +
      scale_y_continuous(labels = label_percent(accuracy = 1)) +
      scale_color_manual(values = dep_palette, drop = FALSE) +
      guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
      labs(x = "Año", y = "% dentro de la matrícula migrante") +
      theme_datamigra()

    plot_dep <- (p_dep_n / p_dep_s + patchwork::plot_layout(heights = c(1, 1), guides = "collect")) &
      theme(legend.position = "top")
    export_plot(plot_dep, fig_series, "fig_6_matricula_migrante_dependencia_2016_2025", width = 9.2, height = 7.4)
    save_table(dependency_trend, tab_series, "fig_6_matricula_migrante_dependencia_2016_2025")
    write_note(
      note_series,
      "fig_6_matricula_migrante_dependencia_2016_2025",
      "Figura 6. Matricula migrante por dependencia administrativa, 2016-2025",
      "Numero y participacion de estudiantes migrantes segun dependencia agregada del establecimiento.",
      c(
        "La categoria `Publica` agrupa `Municipal` y `Servicio Local de Educacion Publica`.",
        "Se mantienen separadas `Particular subvencionado`, `Particular pagado` y `Administracion delegada`.",
        "La figura combina volumen absoluto y peso relativo dentro de la matricula migrante anual."
      )
    )

    school_concentration <- ds |>
      filter(agno == 2025L, !is.na(rbd), !is.na(cod_nac_alu)) |>
      mutate(migrant_flag = if_else(cod_nac_alu == "E", 1L, 0L)) |>
      group_by(rbd, nom_rbd, cod_reg_rbd, nom_reg_rbd_a, cod_com_rbd, nom_com_rbd) |>
      summarise(total_students = n(), migrant_students = sum(migrant_flag), .groups = "drop") |>
      collect() |>
      mutate(
        migrant_share = migrant_students / total_students,
        concentration_bin = cut(migrant_share, breaks = concentration_breaks, labels = concentration_labels, right = TRUE, include.lowest = TRUE),
        concentration_bin = factor(concentration_bin, levels = concentration_labels)
      )

    concentration_distribution <- school_concentration |>
      count(concentration_bin, name = "n_schools") |>
      mutate(share_schools = n_schools / sum(n_schools))

    concentration_editorial <- tibble(
      threshold = c(">20%", ">40%"),
      migrant_students = c(
        sum(school_concentration$migrant_students[school_concentration$migrant_share > 0.20], na.rm = TRUE),
        sum(school_concentration$migrant_students[school_concentration$migrant_share > 0.40], na.rm = TRUE)
      ),
      share_migrant_students = c(
        sum(school_concentration$migrant_students[school_concentration$migrant_share > 0.20], na.rm = TRUE) / sum(school_concentration$migrant_students, na.rm = TRUE),
        sum(school_concentration$migrant_students[school_concentration$migrant_share > 0.40], na.rm = TRUE) / sum(school_concentration$migrant_students, na.rm = TRUE)
      )
    )

    plot_concentration <- ggplot(concentration_distribution, aes(x = concentration_bin, y = n_schools)) +
      geom_col(fill = base_palette["primary"], width = 0.72) +
      scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
      labs(x = "% de matrícula migrante en el establecimiento", y = "Establecimientos") +
      theme_datamigra() +
      theme(axis.text.x = element_text(size = 9))
    export_plot(plot_concentration, fig_snapshot, "fig_8_concentracion_escolar_migrante_2025")
    save_table(concentration_distribution, tab_snapshot, "fig_8_concentracion_escolar_migrante_2025")
    write_note(
      note_snapshot,
      "fig_8_concentracion_escolar_migrante_2025",
      "Figura 8. Concentracion escolar de estudiantes migrantes, 2025",
      "Distribucion de establecimientos segun el porcentaje de matricula migrante que concentran.",
      c(
        "Primero se calcula el porcentaje migrante de cada establecimiento y luego se agrupa en tramos de concentracion.",
        "Ademas se exporta una tabla editorial con la proporcion de estudiantes migrantes que asiste a establecimientos con mas de 20% y mas de 40% de concentracion migrante.",
        "La unidad de analisis son establecimientos con matricula observada en 2025."
      )
    )

    level_trend <- ds |>
      filter(agno >= 2016L, !is.na(cod_nac_alu)) |>
      mutate(
        level_group = build_education_tranche(ens, cod_grado),
        migrant_flag = if_else(cod_nac_alu == "E", 1L, 0L)
      ) |>
      filter(!is.na(level_group)) |>
      group_by(agno, level_group) |>
      summarise(total_students = n(), migrant_students = sum(migrant_flag), .groups = "drop") |>
      collect() |>
      mutate(
        migrant_share = migrant_students / total_students,
        level_group = factor(level_group, levels = c("Parvularia escolar", "Basica", "Media"))
      )

    level_palette <- c(
      "Parvularia escolar" = unname(base_palette["accent_2"]),
      "Basica" = unname(base_palette["primary"]),
      "Media" = unname(base_palette["purple"])
    )

    p_level_n <- ggplot(level_trend, aes(x = agno, y = migrant_students, color = level_group)) +
      geom_line(linewidth = 1.02) +
      geom_point(size = 1.9) +
      scale_x_continuous(breaks = sort(unique(level_trend$agno))) +
      scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
      scale_color_manual(values = level_palette, labels = c("Parvularia escolar", "Básica", "Media"), drop = FALSE) +
      guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
      labs(x = "Año", y = "Estudiantes migrantes") +
      theme_datamigra()

    p_level_s <- ggplot(level_trend, aes(x = agno, y = migrant_share, color = level_group)) +
      geom_line(linewidth = 1.02) +
      geom_point(size = 1.9) +
      scale_x_continuous(breaks = sort(unique(level_trend$agno))) +
      scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
      scale_color_manual(values = level_palette, labels = c("Parvularia escolar", "Básica", "Media"), drop = FALSE) +
      guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
      labs(x = "Año", y = "% sobre la matrícula del nivel") +
      theme_datamigra()

    plot_level_trend <- (p_level_n / p_level_s + patchwork::plot_layout(guides = "collect")) &
      theme(legend.position = "top")
    export_plot(plot_level_trend, fig_pathways, "fig_7_matricula_migrante_niveles_2016_2025", width = 9.4, height = 7.4)
    save_table(level_trend, tab_pathways, "fig_7_matricula_migrante_niveles_2016_2025")
    write_note(
      note_pathways,
      "fig_7_matricula_migrante_niveles_2016_2025",
      "Figura 7. Matricula migrante por nivel educativo, 2016-2025",
      "Evolucion del numero y del porcentaje de estudiantes migrantes en parvularia escolar, basica y media.",
      c(
        "La categoria `Parvularia escolar` usa los grados regulares de prekinder y kinder observados en matricula.",
        "La categoria `Basica` corresponde a `ENS = 3`.",
        "La categoria `Media` agrupa modalidades humanista-cientifica y tecnico-profesional."
      )
    )

    pyramid_2025 <- ds |>
      filter(agno == 2025L, !is.na(cod_nac_alu)) |>
      mutate(
        grade_label = build_regular_grade(ens, cod_grado),
        gender_label = build_gender_label(gen_alu),
        migrant_status = if_else(cod_nac_alu == "E", "Migrantes", "Nacionales")
      ) |>
      filter(!is.na(grade_label), gender_label %in% c("Hombres", "Mujeres")) |>
      group_by(migrant_status, grade_label, gender_label) |>
      summarise(n_students = n(), .groups = "drop") |>
      collect() |>
      group_by(migrant_status) |>
      mutate(
        share_group = n_students / sum(n_students),
        share_signed = if_else(gender_label == "Hombres", -share_group, share_group)
      ) |>
      ungroup() |>
      mutate(grade_label = factor(grade_label, levels = regular_grade_labels))

    plot_pyramid <- ggplot(pyramid_2025, aes(x = share_signed, y = grade_label, fill = gender_label)) +
      geom_col(width = 0.72) +
      facet_wrap(~migrant_status, ncol = 2) +
      scale_x_continuous(labels = function(x) paste0(abs(round(100 * x, 1)), "%")) +
      scale_fill_manual(values = c("Hombres" = unname(base_palette["primary"]), "Mujeres" = unname(base_palette["accent"]))) +
      labs(x = "% dentro de cada grupo", y = NULL) +
      theme_datamigra()
    export_plot(plot_pyramid, fig_pathways, "fig_9_piramide_educativa_2025", width = 10.2, height = 7.4)
    save_table(pyramid_2025, tab_pathways, "fig_9_piramide_educativa_2025")
    write_note(
      note_pathways,
      "fig_9_piramide_educativa_2025",
      "Figura 9. Piramide educativa por sexo y condicion migratoria, 2025",
      "Comparacion de la estructura por curso regular y sexo entre estudiantes migrantes y nacionales.",
      c(
        "La escala se expresa como porcentaje dentro de cada grupo para comparar forma y no volumen absoluto.",
        "La figura usa solo grados regulares, desde prekinder hasta 4 medio.",
        "Los hombres se muestran hacia la izquierda y las mujeres hacia la derecha."
      )
    )

    figure_catalog <- tibble::tribble(
      ~section, ~file_stem, ~title, ~subtitle, ~source_note,
      "series", "fig_1_evolucion_matricula_migrante_2016_2025", "Figura 1. Evolucion de la matricula migrante en Chile, 2016-2025", "Numero de estudiantes migrantes y porcentaje sobre la matricula total.", "Fuente: elaboracion propia con datos de matricula por estudiante del MINEDUC.",
      "snapshot", "fig_2_principales_paises_2025", "Figura 2. Principales paises de origen de estudiantes migrantes, 2025", "Ranking de paises de origen de la matricula migrante en el ultimo anio disponible.", "Fuente: elaboracion propia con datos de matricula por estudiante del MINEDUC.",
      "series", "fig_3_composicion_nacionalidad_2016_2025", "Figura 3. Distribucion de estudiantes migrantes por nacionalidad, 2016 y 2025", "Comparacion de la composicion de la matricula migrante entre el primer y el ultimo anio disponible.", "Fuente: elaboracion propia con datos de matricula por estudiante del MINEDUC.",
      "snapshot", "fig_4_porcentaje_matricula_migrante_region_2025", "Figura 4. Porcentaje de matricula migrante por region, 2025", "Peso de la matricula migrante sobre la matricula total regional.", "Fuente: elaboracion propia con datos de matricula por estudiante del MINEDUC.",
      "snapshot", "fig_5_comunas_mayor_matricula_migrante_2025", "Figura 5. Comunas con mayor matricula migrante, 2025", "Ranking comunal segun numero absoluto de estudiantes migrantes.", "Fuente: elaboracion propia con datos de matricula por estudiante del MINEDUC.",
      "series", "fig_6_matricula_migrante_dependencia_2016_2025", "Figura 6. Matricula migrante por dependencia administrativa, 2016-2025", "Numero y participacion de estudiantes migrantes segun dependencia agregada del establecimiento.", "Fuente: elaboracion propia con datos de matricula por estudiante del MINEDUC.",
      "pathways", "fig_7_matricula_migrante_niveles_2016_2025", "Figura 7. Matricula migrante por nivel educativo, 2016-2025", "Evolucion del numero y del porcentaje de estudiantes migrantes en parvularia escolar, basica y media.", "Fuente: elaboracion propia con datos de matricula por estudiante del MINEDUC.",
      "snapshot", "fig_8_concentracion_escolar_migrante_2025", "Figura 8. Concentracion escolar de estudiantes migrantes, 2025", "Distribucion de establecimientos segun porcentaje de matricula migrante.", "Fuente: elaboracion propia con datos de matricula por estudiante del MINEDUC.",
      "pathways", "fig_9_piramide_educativa_2025", "Figura 9. Piramide educativa por sexo y condicion migratoria, 2025", "Comparacion de la estructura por curso y sexo entre estudiantes migrantes y nacionales.", "Fuente: elaboracion propia con datos de matricula por estudiante del MINEDUC."
    )

    hallazgos <- c(
      sprintf("La matricula migrante pasa de `%s` estudiantes en 2016 a `%s` en 2025.", format(trend$migrant_students[trend$agno == 2016], big.mark = "."), format(trend$migrant_students[trend$agno == 2025], big.mark = ".")),
      sprintf("Su participacion sobre la matricula total sube de `%s%%` a `%s%%` en el mismo periodo.", format(round(100 * trend$migrant_share[trend$agno == 2016], 1), nsmall = 1, decimal.mark = ","), format(round(100 * trend$migrant_share[trend$agno == 2025], 1), nsmall = 1, decimal.mark = ",")),
      sprintf("En 2025, los principales paises de origen son `%s`, `%s`, `%s`, `%s` y `%s`.", countries_latest$pais[1], countries_latest$pais[2], countries_latest$pais[3], countries_latest$pais[4], countries_latest$pais[5]),
      sprintf("Las mayores participaciones regionales de matricula migrante en 2025 se observan en `%s`, `%s`, `%s` y `%s`.", regional_2025$region_label[order(-regional_2025$migrant_share)][1], regional_2025$region_label[order(-regional_2025$migrant_share)][2], regional_2025$region_label[order(-regional_2025$migrant_share)][3], regional_2025$region_label[order(-regional_2025$migrant_share)][4]),
      sprintf("`%s%%` de los estudiantes migrantes asiste a establecimientos con mas de 20%% de concentracion migrante, y `%s%%` a establecimientos con mas de 40%%.", format(round(100 * concentration_editorial$share_migrant_students[concentration_editorial$threshold == ">20%"], 1), nsmall = 1, decimal.mark = ","), format(round(100 * concentration_editorial$share_migrant_students[concentration_editorial$threshold == ">40%"], 1), nsmall = 1, decimal.mark = ",")),
      "La comparacion por nivel muestra que el crecimiento migrante se concentra en basica y media, aunque la parvularia escolar tambien gana peso dentro del sistema."
    )

    if (has_junji) {
      ds_junji <- arrow::open_dataset(junji_student_path)
      ds_junji_center <- arrow::open_dataset(junji_center_path)
      junji_series <- ds_junji |>
        filter(!is.na(migrant_flag)) |>
        group_by(agno) |>
        summarise(
          total_children = n(),
          migrant_children = sum(migrant_flag),
          .groups = "drop"
        ) |>
        collect() |>
        mutate(migrant_share = migrant_children / total_children)

      junji_synthesis_countries <- ds_junji |>
        filter(
          agno %in% c(2011L, 2025L),
          migrant_status == "Migrante",
          !is.na(nacionalidad),
          !nacionalidad %in% c("No informado")
        ) |>
        group_by(agno, nacionalidad) |>
        summarise(n_children = n(), .groups = "drop") |>
        collect()

      top_junji_synthesis_names <- junji_synthesis_countries |>
        group_by(nacionalidad) |>
        summarise(total_children = sum(n_children), .groups = "drop") |>
        arrange(desc(total_children), nacionalidad) |>
        slice_head(n = 5) |>
        pull(nacionalidad)

      junji_synthesis <- junji_synthesis_countries |>
        mutate(
          nacionalidad = if_else(nacionalidad %in% top_junji_synthesis_names, nacionalidad, "Otros")
        ) |>
        group_by(agno, nacionalidad) |>
        summarise(n_children = sum(n_children), .groups = "drop") |>
        left_join(select(junji_series, agno, total_children, migrant_children, migrant_share), by = "agno") |>
        mutate(
          share_within_migrant = n_children / migrant_children,
          share_total = n_children / total_children
        ) |>
        ungroup()

      synthesis_order <- junji_synthesis |>
        group_by(nacionalidad) |>
        summarise(total_children = sum(n_children), .groups = "drop") |>
        mutate(order_value = if_else(nacionalidad == "Otros", -Inf, total_children)) |>
        arrange(desc(order_value), nacionalidad) |>
        pull(nacionalidad)

      junji_synthesis <- junji_synthesis |>
        mutate(
          nacionalidad = factor(nacionalidad, levels = c(setdiff(synthesis_order, "Otros"), "Otros")),
          agno = factor(as.character(agno), levels = c("2011", "2025"))
        )

      junji_synthesis_labels <- junji_series |>
        filter(agno %in% c(2011L, 2025L)) |>
        mutate(
          agno = factor(as.character(agno), levels = c("2011", "2025")),
          label = paste0(format(round(100 * migrant_share, 2), nsmall = 2, decimal.mark = ","), "%")
        )

      synthesis_levels <- levels(junji_synthesis$nacionalidad)
      synthesis_base_colors <- c(
        unname(base_palette["primary"]),
        unname(base_palette["accent"]),
        unname(base_palette["accent_2"]),
        unname(base_palette["green"]),
        unname(base_palette["purple"])
      )
      junji_synthesis_palette <- stats::setNames(
        rep(synthesis_base_colors, length.out = length(setdiff(synthesis_levels, "Otros"))),
        setdiff(synthesis_levels, "Otros")
      )
      junji_synthesis_palette <- c(junji_synthesis_palette, "Otros" = unname(base_palette["muted"]))

      plot_junji_synthesis <- ggplot(junji_synthesis, aes(x = agno, y = share_total, fill = nacionalidad)) +
        geom_col(width = 0.62, color = "white", linewidth = 0.35) +
        geom_text(
          data = junji_synthesis_labels,
          aes(x = agno, y = migrant_share, label = label),
          inherit.aes = FALSE,
          vjust = -0.5,
          family = base_family,
          size = 4.2,
          fontface = "bold",
          color = "#163047"
        ) +
        scale_fill_manual(values = junji_synthesis_palette, drop = FALSE) +
        scale_y_continuous(
          limits = c(0, 0.05),
          breaks = seq(0, 0.05, by = 0.01),
          labels = label_percent(accuracy = 1),
          expand = expansion(mult = c(0, 0.06))
        ) +
        labs(x = NULL, y = "% sobre matrícula total", fill = "Nacionalidad") +
        theme_datamigra() +
        theme(legend.position = "top")
      export_plot(plot_junji_synthesis, fig_early_series, "fig_10_junji_crecimiento_matricula_migrante_2011_2025", width = 8.6, height = 6.4)
      save_table(junji_synthesis, tab_early_series, "fig_10_junji_crecimiento_matricula_migrante_2011_2025")
      write_note(
        note_early_series,
        "fig_10_junji_crecimiento_matricula_migrante_2011_2025",
        "Figura 10. Crecimiento de la matricula migrante en JUNJI, 2011 y 2025",
        "Porcentaje de parvulos migrantes sobre la matricula total y composicion por nacionalidad en los anos inicial y final de la serie.",
        c(
          "La altura de cada barra representa la participacion migrante sobre la matricula total observada en JUNJI.",
          "Los segmentos internos muestran la composicion por nacionalidad dentro de la matricula migrante de cada anio.",
          "Las categorias fuera del grupo principal se agrupan en `Otros`; `No informado` se excluye del calculo porcentual."
        )
      )

      p_junji_n <- ggplot(junji_series, aes(x = agno, y = migrant_children)) +
        geom_line(color = base_palette["primary"], linewidth = 1.08) +
        geom_point(color = base_palette["primary"], size = 2.2) +
        scale_x_continuous(breaks = sort(unique(junji_series$agno))) +
        scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
        labs(x = "Año", y = "Párvulos migrantes") +
        theme_datamigra()

      p_junji_s <- ggplot(junji_series, aes(x = agno, y = migrant_share)) +
        geom_line(color = base_palette["accent"], linewidth = 1.08) +
        geom_point(color = base_palette["accent"], size = 2.2) +
        scale_x_continuous(breaks = sort(unique(junji_series$agno))) +
        scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
        labs(x = "Año", y = "% sobre matrícula total") +
        theme_datamigra()

      plot_junji_series <- p_junji_n / p_junji_s + patchwork::plot_layout(heights = c(1, 1))
      export_plot(plot_junji_series, fig_early_series, "fig_11_junji_evolucion_matricula_migrante_2011_2025", width = 9.2, height = 7.2)
      save_table(junji_series, tab_early_series, "fig_11_junji_evolucion_matricula_migrante_2011_2025")
      write_note(
        note_early_series,
        "fig_11_junji_evolucion_matricula_migrante_2011_2025",
        "Figura 11. Evolucion de la matricula migrante JUNJI, 2011-2025",
        "Numero de parvulos migrantes y porcentaje que representan sobre la matricula total observada en la base.",
        c(
          "La serie usa exclusivamente registros con nacionalidad informativa.",
          "Se clasifica `Chile` y `Chileno/a` como nacional, y el resto de nacionalidades como migrante.",
          "Los casos `No informado` se excluyen del denominador para el porcentaje."
        )
      )

      junji_countries_2025 <- ds_junji |>
        filter(agno == 2025L, migrant_status == "Migrante", !is.na(nacionalidad)) |>
        group_by(nacionalidad) |>
        summarise(n_children = n(), .groups = "drop") |>
        collect() |>
        arrange(desc(n_children), nacionalidad) |>
        collapse_top_categories(nacionalidad, n_children, top_n = 8L, others_label = "Otros") |>
        mutate(nacionalidad = factor(nacionalidad, levels = rev(nacionalidad)))

      plot_junji_countries <- ggplot(junji_countries_2025, aes(x = n_children, y = nacionalidad)) +
        geom_col(fill = base_palette["primary"], width = 0.72) +
        scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
        labs(x = "Párvulos migrantes", y = NULL) +
        theme_datamigra()
      export_plot(plot_junji_countries, fig_early_snapshot, "fig_12_junji_principales_nacionalidades_2025", width = 8.8, height = 6.2)
      save_table(junji_countries_2025 |> mutate(nacionalidad = as.character(nacionalidad)), tab_early_snapshot, "fig_12_junji_principales_nacionalidades_2025")
      write_note(
        note_early_snapshot,
        "fig_12_junji_principales_nacionalidades_2025",
        "Figura 12. Principales nacionalidades migrantes en JUNJI, 2025",
        "Ranking de nacionalidades migrantes observadas en la matricula parvularia del ultimo anio disponible.",
        c(
          "La figura usa nacionalidad declarada en la respuesta de Transparencia de JUNJI.",
          "Las categorias residuales se agrupan en `Otros` y se muestran al final del ranking.",
          "Se excluye `No informado` del ranking principal."
        )
      )

      junji_region_2025 <- ds_junji_center |>
        filter(agno == 2025L, !is.na(region), !is.na(migrant_share)) |>
        group_by(cod_region, region, region_display) |>
        summarise(
          total_children = sum(migrant_children + national_children, na.rm = TRUE),
          migrant_children = sum(migrant_children, na.rm = TRUE),
          .groups = "drop"
        ) |>
        collect() |>
        mutate(
          migrant_share = migrant_children / total_children,
          region_factor = factor(region_display, levels = display_region(region_order))
        ) |>
        filter(!is.na(region_factor)) |>
        arrange(match(as.character(region_factor), display_region(region_order)))

      plot_junji_region <- ggplot(junji_region_2025, aes(x = migrant_share, y = forcats::fct_rev(region_factor))) +
        geom_col(fill = base_palette["primary"], width = 0.72) +
        scale_x_continuous(labels = label_percent(accuracy = 0.1)) +
        labs(x = "% de matrícula migrante", y = NULL) +
        theme_datamigra()
      export_plot(plot_junji_region, fig_early_snapshot, "fig_13_junji_porcentaje_migrante_region_2025", width = 8.8, height = 6.6)
      save_table(junji_region_2025 |> mutate(region = as.character(region_factor)), tab_early_snapshot, "fig_13_junji_porcentaje_migrante_region_2025")
      write_note(
        note_early_snapshot,
        "fig_13_junji_porcentaje_migrante_region_2025",
        "Figura 13. Porcentaje de matricula migrante JUNJI por region, 2025",
        "Comparacion territorial del peso de la matricula migrante dentro de la matricula parvularia observada en JUNJI.",
        c(
          "Las regiones se ordenan de norte a sur.",
          "La region de Nuble se recupera usando codigo regional cuando la etiqueta textual aparece como `No_encontrado`.",
          "La metrica corresponde al porcentaje de migrantes sobre la matricula con nacionalidad informativa."
        )
      )

      junji_levels_2025 <- ds_junji |>
        filter(agno == 2025L, migrant_status == "Migrante", !is.na(level_group)) |>
        group_by(level_group) |>
        summarise(n_children = n(), .groups = "drop") |>
        collect() |>
        mutate(
          level_group = factor(
            level_group,
            levels = c("Sala cuna", "Nivel medio", "Transicion", "Medio y transicion", "Otros")
          )
        ) |>
        arrange(level_group)

      level_fill_junji <- c(
        "Sala cuna" = unname(base_palette["primary"]),
        "Nivel medio" = unname(base_palette["accent_2"]),
        "Transicion" = unname(base_palette["green"]),
        "Medio y transicion" = unname(base_palette["purple"]),
        "Otros" = unname(base_palette["muted"])
      )

      plot_junji_levels <- ggplot(junji_levels_2025, aes(x = n_children, y = forcats::fct_rev(level_group), fill = level_group)) +
        geom_col(width = 0.72, show.legend = FALSE) +
        scale_fill_manual(values = level_fill_junji, drop = FALSE) +
        scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
        labs(x = "Párvulos migrantes", y = NULL) +
        theme_datamigra()
      export_plot(plot_junji_levels, fig_early_snapshot, "fig_14_junji_distribucion_migrante_nivel_2025", width = 8.6, height = 5.8)
      save_table(junji_levels_2025 |> mutate(level_group = as.character(level_group)), tab_early_snapshot, "fig_14_junji_distribucion_migrante_nivel_2025")
      write_note(
        note_early_snapshot,
        "fig_14_junji_distribucion_migrante_nivel_2025",
        "Figura 14. Distribucion de la matricula migrante JUNJI por nivel, 2025",
        "Composicion de la matricula migrante de JUNJI segun grandes grupos de nivel parvulario.",
        c(
          "Se agrupan niveles especificos en `Sala cuna`, `Nivel medio`, `Transicion`, `Medio y transicion` y `Otros`.",
          "Se prioriza nivel por sobre modalidad porque dialoga mejor con la trayectoria educativa posterior.",
          "La figura resume volumen absoluto y evita categorias redundantes por ortografia o formato."
        )
      )

      figure_catalog <- bind_rows(
        figure_catalog,
        tibble::tribble(
          ~section, ~file_stem, ~title, ~subtitle, ~source_note,
          "early_series", "fig_10_junji_crecimiento_matricula_migrante_2011_2025", "Figura 10. Crecimiento de la matricula migrante en JUNJI, 2011 y 2025", "Porcentaje de parvulos migrantes sobre la matricula total y composicion por nacionalidad.", "Fuente: elaboracion propia con datos JUNJI obtenidos via Transparencia.",
          "early_series", "fig_11_junji_evolucion_matricula_migrante_2011_2025", "Figura 11. Evolucion de la matricula migrante JUNJI, 2011-2025", "Numero de parvulos migrantes y porcentaje sobre la matricula total observada.", "Fuente: elaboracion propia con datos JUNJI obtenidos via Transparencia.",
          "early_snapshot", "fig_12_junji_principales_nacionalidades_2025", "Figura 12. Principales nacionalidades migrantes en JUNJI, 2025", "Ranking de nacionalidades migrantes observadas en 2025.", "Fuente: elaboracion propia con datos JUNJI obtenidos via Transparencia.",
          "early_snapshot", "fig_13_junji_porcentaje_migrante_region_2025", "Figura 13. Porcentaje de matricula migrante JUNJI por region, 2025", "Peso relativo de la matricula migrante dentro de la matricula parvularia observada por region.", "Fuente: elaboracion propia con datos JUNJI obtenidos via Transparencia.",
          "early_snapshot", "fig_14_junji_distribucion_migrante_nivel_2025", "Figura 14. Distribucion de la matricula migrante JUNJI por nivel, 2025", "Composicion de la matricula migrante segun grandes grupos de nivel parvulario.", "Fuente: elaboracion propia con datos JUNJI obtenidos via Transparencia."
        )
      )

      hallazgos <- c(
        hallazgos,
        sprintf("La matricula migrante JUNJI sube de `%s` parvulos en %s a `%s` en 2025.", format(junji_series$migrant_children[junji_series$agno == min(junji_series$agno, na.rm = TRUE)], big.mark = "."), min(junji_series$agno, na.rm = TRUE), format(junji_series$migrant_children[junji_series$agno == 2025], big.mark = ".")),
        sprintf("En 2025, las principales nacionalidades migrantes en JUNJI son `%s`, `%s`, `%s` y `%s`.", as.character(junji_countries_2025$nacionalidad[1]), as.character(junji_countries_2025$nacionalidad[2]), as.character(junji_countries_2025$nacionalidad[3]), as.character(junji_countries_2025$nacionalidad[4])),
        "La distribucion por nivel muestra que la matricula migrante en primera infancia se concentra en sala cuna y nivel medio, reforzando la continuidad temprana con la trayectoria escolar."
      )
    }

    list(
      trend = trend,
      countries_latest = countries_latest,
      regional_2025 = regional_2025,
      figure_catalog = figure_catalog,
      hallazgos = hallazgos
    )
  }

  write_status <- function(inventory, matricula_year_summary = NULL, matricula_school = NULL, junji_year_summary = NULL, junji_center = NULL) {
    raw_matricula <- if ("source" %in% names(inventory) && any(inventory$source == "matricula", na.rm = TRUE)) {
      sum(inventory$source == "matricula", na.rm = TRUE)
    } else {
      NA_integer_
    }
    raw_junji <- if ("source" %in% names(inventory) && any(inventory$source == "junji", na.rm = TRUE)) {
      sum(inventory$source == "junji", na.rm = TRUE)
    } else {
      NA_integer_
    }

    lines <- c(
      "# Resumen de validación MINEDUC",
      "",
      "- Estado: fuente activa con matricula escolar y educacion parvularia oficial (JUNJI).",
      "- Matricula: solicitud via Transparencia.",
      "- JUNJI: respuesta via Ley de Transparencia sobre matricula parvularia.",
      sprintf("- Archivos originales de matrícula detectados: `%s`.", ifelse(is.na(raw_matricula), "no procesados", raw_matricula)),
      sprintf("- Archivos originales de JUNJI detectados: `%s`.", ifelse(is.na(raw_junji), "no procesados", raw_junji)),
      "- Regla metodologica obligatoria para matricula: desde `2015`, filtrar `ESTADO_ESTAB == 1`."
    )

    if (!is.null(matricula_year_summary)) {
      lines <- c(
        lines,
        sprintf(
          "- Panel final matricula consolidado: `%s` anios (`%s` a `%s`).",
          nrow(matricula_year_summary),
          min(matricula_year_summary$agno, na.rm = TRUE),
          max(matricula_year_summary$agno, na.rm = TRUE)
        )
      )
    }

    if (!is.null(matricula_school)) {
      lines <- c(
        lines,
        sprintf("- Panel establecimiento-anio de matricula generado: `%s` filas.", format(nrow(matricula_school), big.mark = ","))
      )
    }

    if (!is.null(junji_year_summary)) {
      lines <- c(
        lines,
        sprintf(
          "- Panel JUNJI consolidado: `%s` anios (`%s` a `%s`).",
          nrow(junji_year_summary),
          min(junji_year_summary$agno, na.rm = TRUE),
          max(junji_year_summary$agno, na.rm = TRUE)
        )
      )
    }

    if (!is.null(junji_center)) {
      lines <- c(
        lines,
        sprintf("- Panel territorial JUNJI generado: `%s` filas agregadas.", format(nrow(junji_center), big.mark = ","))
      )
    }

    lines <- c(
      lines,
      "",
      "## Análisis adicionales posibles",
      "",
      "- Explorar cruces adicionales entre territorio, nivel y modalidad dentro de JUNJI.",
      "- Definir una seleccion corta de 4 a 6 figuras para futuros policy briefs de educacion y primera infancia."
    )

    invisible(lines)
  }

  inventory_files <- function(selected_sources = sources) {
    parts <- list()
    if ("matricula" %in% selected_sources) {
      parts[["matricula"]] <- discover_matricula_files()
    }
    if ("junji" %in% selected_sources) {
      parts[["junji"]] <- discover_junji_files()
    }
    bind_rows(parts) |>
      mutate(size_mb = round(file.size(path) / 1024^2, 2))
  }

  inventory <- NULL
  if (build %in% c("all", "data")) {
    inventory <- inventory_files(sources)
    write_csv(inventory, file.path(final_dir, "raw_inventory.csv"))
  } else if (file.exists(file.path(final_dir, "raw_inventory.csv"))) {
    inventory <- readr::read_csv(file.path(final_dir, "raw_inventory.csv"), show_col_types = FALSE)
  } else {
    inventory <- tibble()
  }

  results <- list(build = build, raw_inventory = inventory)

  if (build %in% c("all", "data")) {
    if ("matricula" %in% sources) {
      results$matricula <- process_matricula_pipeline(force_rebuild = force_rebuild, years = years)
    }
    if ("junji" %in% sources) {
      results$junji <- process_junji_pipeline(force_rebuild = force_rebuild, years = years)
    }
  }

  if (build %in% c("all", "figures")) {
    results$education_figures <- generate_education_figures()
  }

  invisible(results)
}

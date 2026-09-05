# CASEN
# Fuente de datos: https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen
# Unidad de observación: persona encuestada por año CASEN.
# Espera insumos en `data/raw/casen/` y construye datos procesados y productos públicos.
# 1. lectura de datos originales;
# 2. procesamiento hacia `data/interim/`;
# 3. construccion de paneles en `data/final/`;
# 4. generacion de tablas espejo;
# 5. generacion de figuras publicables;
# 6. exportación hacia `figures/casen/` y `tables/casen/`.

run_casen <- function(build = c("all", "panel", "figures"), force_rebuild = FALSE) {
  build <- match.arg(build)
  suppressPackageStartupMessages({
    library(here)
    library(dplyr)
    library(readr)
    library(haven)
    library(tibble)
    library(purrr)
    library(ggplot2)
    library(scales)
  })

  log_info <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
  ensure_dir <- function(path) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  pick_base_family <- function() {
    available_fonts <- tryCatch(systemfonts::system_fonts()$family, error = function(e) character())
    available_fonts <- unique(available_fonts)
    dplyr::case_when(
      "Helvetica" %in% available_fonts ~ "Helvetica",
      "Arial" %in% available_fonts ~ "Arial",
      TRUE ~ "sans"
    )
  }
  wrap_label <- function(x, width = 26) {
    vapply(x, function(label) paste(strwrap(label, width = width), collapse = "\n"), character(1))
  }

  first_existing <- function(df, candidates) {
    hit <- candidates[candidates %in% names(df)]
    if (length(hit) == 0) return(rep(NA_real_, nrow(df)))
    as_num(df[[hit[1]]])
  }

  recode_binary <- function(x, yes = 1, no = 0) {
    out <- rep(NA_real_, length(x))
    out[x == yes] <- 1
    out[x == no] <- 0
    out
  }

  weighted_prop <- function(df, num_var, den_var, w_var = "expr") {
    den <- sum(df[[den_var]] * df[[w_var]], na.rm = TRUE)
    if (is.na(den) || den == 0) return(NA_real_)
    num <- sum(df[[num_var]] * df[[w_var]], na.rm = TRUE)
    100 * num / den
  }

  inflation_factor <- function(year) {
    # Factores de actualización monetaria con base 2006.
    map <- c(`2006` = 2.05, `2009` = 1.772, `2011` = 1.648, `2013` = 1.576,
             `2015` = 1.443, `2017` = 1.374, `2020` = 1.263, `2022` = 1.045)
    # La edición 2024 se expresa en moneda corriente del año y usa factor 1.
    if (as.character(year) %in% names(map)) return(unname(map[as.character(year)]))
    if (year == 2024) return(1.0)
    NA_real_
  }

  build_panel <- function(force_rebuild = FALSE) {
    # ------------------------------------------------------------------------
    # 1) Paths
    # ------------------------------------------------------------------------
    raw_root <- here::here("data", "raw", "casen")
    interim_dir <- here::here("data", "interim", "casen")
    ensure_dir(interim_dir)

    # ------------------------------------------------------------------------
    # 2) Crosswalk base por año (nombres en raw -> estandar panel)
    # ------------------------------------------------------------------------
    year_specs <- tribble(
      ~year, ~raw_file, ~raw_file_alt, ~mig_var, ~educ_var, ~sexo_var, ~edad_var, ~numper_var, ~casam2_var, ~horas_var, ~ing_act_var, ~ing_trab_var, ~contrato_var, ~region_var, ~comuna_var, ~zona_var, ~hacin_var, ~expr_var, ~expc_var, ~esc_var, ~lp_var, ~pobreza_var, ~pobreza_multi_var, ~yaut_var, ~ysub_var, ~qaut_var,
      2006, "2006/casen2006.dta", NA, "t7", "educ", "sexo", "edad", "numper", "v6", "o15", "yopraj", "ytrabaj", NA, "r", "comuna", "z", NA, "expr", "expc", "esc", NA, NA, NA, "yautaj", "ysubaj", "qaut",
      2009, "2009/casen2009stata.dta", NA, "t8cod", "educ", "sexo", "edad", "numper", "v6", "o16", "yopraj", "ytrabaj", "o25", "region", "comuna", "zona", "hacinami1", "expr_p", "expc_p", "esc", NA, NA, NA, "yautaj", "ysubaj", "qaut",
      2011, "2011/casen2011_octubre2011_enero2012_principal_08032013stata.dta", NA, "h11", "educ", "sexo", "edad", "numper", "v5m", "o10", "yopraj", "ytrabaj", "o17", "region", "comuna", "zona", "hacinamiento", "expr_full", "expc_full", "esc", NA, "corte", NA, "yautaj", "ysubaj", "qaut",
      2013, "2013/casen_2013_mn_b_principal.dta", NA, "r1a", "educ", "sexo", "edad", "numper", "v11m", "o10", "yoprcor", "ytrabajocor", "o17", "region", "comuna", "zona", "hacinamiento", "expr", "expc", "esc", NA, "pobreza_mn", "pobreza_multi", "yautcor", "ysub", "qaut_mn",
      2015, "2015/Casen 2015.dta", NA, "r1a", "educ", "sexo", "edad", "numper", "v11", "o10", "yoprcor", "ytrabajocor", "o17", "region", "comuna", "zona", "hacinamiento", "expr", "expc", "esc", NA, "pobreza", "pobreza_multi_4d", "yautcor", "ysub", "qaut",
      2017, "2017/Casen 2017.dta", NA, "r1a", "educ", "sexo", "edad", "numper", "v12mt", "o10", "yoprcor", "ytrabajocor", "o17", "region", "comuna", "zona", "hacinamiento", "expr", "expc", "esc", "lp", "pobreza", "pobreza_multi_4d", "yautcor", "ysub", "qaut",
      2020, "2020/casen_en_pandemia_2020.dta", "2020/casen_en_pandemia_2020.RData", "r2", "educc", "sexo", "edad", "numper", NA, "y2_hrs", "yoprcor", "ytrabajocor", NA, "region", "r2_c_cod", "area", "ind_hacina", "expr", NA, "esc", "lp", "pobreza", NA, "yautcor", "ysub", "qaut",
      2022, "2022/Base de datos Casen 2022 STATA_18 marzo 2024.dta", "2022/casen_2022.RData", "r1a", "educc", "sexo", "edad", "numper", "v12mt", "o10", "yoprcor", "ytrabajocor", "contrato", "region", "comuna", "area", "ind_hacina", "expr", "expc", "esc", "lp", "pobreza", "pobreza_multi_4d", "yautcor", "ysub", "qaut",
      2024, "2024/casen_2024.dta", NA, "r1a", "educc", "sexo", "edad", "numper", "v12mt", "o10", "yoprcor", "ytrabajocor", "contrato", "region", "estrato", "area", "ind_hacina", "expr", NA, "esc", "lp", "pobreza", "pobreza_multi", "yautcor", "ysub", "qaut"
    )

    crosswalk_out <- year_specs |>
      mutate(
        raw_path = file.path(raw_root, raw_file),
        raw_path_alt = ifelse(is.na(raw_file_alt), NA, file.path(raw_root, raw_file_alt)),
        available = file.exists(raw_path),
        available_alt = ifelse(is.na(raw_file_alt), FALSE, file.exists(raw_path_alt))
      )

    readr::write_csv(crosswalk_out, file.path(interim_dir, "casen_crosswalk_variables.csv"))

    # ------------------------------------------------------------------------
    # 3) Limpieza por año
    # ------------------------------------------------------------------------
    clean_one_year <- function(spec_row) {
      y <- spec_row$year[[1]]
      p <- file.path(raw_root, spec_row$raw_file[[1]])
      p_alt <- if (!is.na(spec_row$raw_file_alt[[1]])) file.path(raw_root, spec_row$raw_file_alt[[1]]) else NA_character_

      if (!file.exists(p) && (is.na(p_alt) || !file.exists(p_alt))) {
        log_info(sprintf("CASEN %s: archivo no encontrado -> %s", y, p))
        return(NULL)
      }

      raw <- NULL
      source_used <- NULL

      if (file.exists(p)) {
        log_info(sprintf("CASEN %s: cargando %s", y, p))
        raw <- tryCatch(
          haven::read_dta(p, encoding = "latin1"),
          error = function(e) {
            log_info(sprintf("CASEN %s: error de lectura dta (%s)", y, e$message))
            NULL
          }
        )
        if (!is.null(raw)) source_used <- basename(p)
      }

      if (is.null(raw) && !is.na(p_alt) && file.exists(p_alt)) {
        log_info(sprintf("CASEN %s: cargando fallback %s", y, p_alt))
        env <- new.env(parent = emptyenv())
        load(p_alt, envir = env)
        objs <- ls(env)
        if (length(objs) == 0) return(NULL)
        idx <- which(vapply(objs, function(nm) inherits(env[[nm]], c("data.frame", "tbl_df")), logical(1)))[1]
        if (is.na(idx)) return(NULL)
        raw <- env[[objs[idx]]]
        source_used <- basename(p_alt)
      }

      if (is.null(raw)) return(NULL)

      n0 <- nrow(raw)

      mig_raw <- first_existing(raw, c(spec_row$mig_var[[1]]))
      educ_raw <- first_existing(raw, c(spec_row$educ_var[[1]]))
      sexo_raw <- first_existing(raw, c(spec_row$sexo_var[[1]]))

      edad <- first_existing(raw, c(spec_row$edad_var[[1]]))
      numper <- first_existing(raw, c(spec_row$numper_var[[1]]))
      horas_trabajo <- if (!is.na(spec_row$horas_var[[1]]) && spec_row$horas_var[[1]] %in% names(raw)) as_num(raw[[spec_row$horas_var[[1]]]]) else NA_real_
      ing_act_prin <- if (!is.na(spec_row$ing_act_var[[1]]) && spec_row$ing_act_var[[1]] %in% names(raw)) as_num(raw[[spec_row$ing_act_var[[1]]]]) else NA_real_
      ing_del_trabajo <- if (!is.na(spec_row$ing_trab_var[[1]]) && spec_row$ing_trab_var[[1]] %in% names(raw)) as_num(raw[[spec_row$ing_trab_var[[1]]]]) else NA_real_

      region <- if (!is.na(spec_row$region_var[[1]]) && spec_row$region_var[[1]] %in% names(raw)) as_num(raw[[spec_row$region_var[[1]]]]) else NA_real_
      comuna <- if (!is.na(spec_row$comuna_var[[1]]) && spec_row$comuna_var[[1]] %in% names(raw)) as_num(raw[[spec_row$comuna_var[[1]]]]) else NA_real_
      zona <- if (!is.na(spec_row$zona_var[[1]]) && spec_row$zona_var[[1]] %in% names(raw)) as_num(raw[[spec_row$zona_var[[1]]]]) else NA_real_
      hacinamiento <- if (!is.na(spec_row$hacin_var[[1]]) && spec_row$hacin_var[[1]] %in% names(raw)) as_num(raw[[spec_row$hacin_var[[1]]]]) else NA_real_

      expr <- if (!is.na(spec_row$expr_var[[1]]) && spec_row$expr_var[[1]] %in% names(raw)) as_num(raw[[spec_row$expr_var[[1]]]]) else NA_real_
      expc <- if (!is.na(spec_row$expc_var[[1]]) && spec_row$expc_var[[1]] %in% names(raw)) as_num(raw[[spec_row$expc_var[[1]]]]) else NA_real_
      esc <- if (!is.na(spec_row$esc_var[[1]]) && spec_row$esc_var[[1]] %in% names(raw)) as_num(raw[[spec_row$esc_var[[1]]]]) else NA_real_
      lp <- if (!is.na(spec_row$lp_var[[1]]) && spec_row$lp_var[[1]] %in% names(raw)) as_num(raw[[spec_row$lp_var[[1]]]]) else NA_real_
      pobreza <- if (!is.na(spec_row$pobreza_var[[1]]) && spec_row$pobreza_var[[1]] %in% names(raw)) as_num(raw[[spec_row$pobreza_var[[1]]]]) else NA_real_
      pobreza_multi <- if (!is.na(spec_row$pobreza_multi_var[[1]]) && spec_row$pobreza_multi_var[[1]] %in% names(raw)) as_num(raw[[spec_row$pobreza_multi_var[[1]]]]) else NA_real_

      yaut_core <- if (!is.na(spec_row$yaut_var[[1]]) && spec_row$yaut_var[[1]] %in% names(raw)) as_num(raw[[spec_row$yaut_var[[1]]]]) else NA_real_
      ysub_core <- if (!is.na(spec_row$ysub_var[[1]]) && spec_row$ysub_var[[1]] %in% names(raw)) as_num(raw[[spec_row$ysub_var[[1]]]]) else NA_real_
      qaut_core <- if (!is.na(spec_row$qaut_var[[1]]) && spec_row$qaut_var[[1]] %in% names(raw)) as_num(raw[[spec_row$qaut_var[[1]]]]) else NA_real_

      yautaj_alt <- first_existing(raw, c("yautaj", "yautcor"))
      ysubaj_alt <- first_existing(raw, c("ysubaj", "ysub"))
      qaut_alt <- first_existing(raw, c("qaut", "qaut_mn"))

      extranjero <- rep(NA_real_, length(mig_raw))
      if (y == 2006) {
        extranjero[mig_raw == 3] <- 1
        extranjero[mig_raw %in% c(1, 2, 0)] <- 0
        extranjero[mig_raw == 9] <- NA
      } else if (y == 2009) {
        t8n <- suppressWarnings(as_num(raw[["t8cod"]]))
        t8n[is.na(t8n)] <- 0
        extranjero[t8n > 16000] <- 1
        extranjero[t8n <= 16000] <- 0
        extranjero[t8n == 99999] <- NA
      } else if (y == 2011) {
        extranjero[mig_raw == 3] <- 1
        extranjero[mig_raw == 1] <- 0
        extranjero[mig_raw == 2] <- NA
      } else if (y %in% c(2013, 2015, 2017, 2022, 2024)) {
        extranjero[mig_raw == 3] <- 1
        extranjero[mig_raw == 1] <- 0
        extranjero[mig_raw %in% c(2, 9)] <- NA
      } else if (y == 2020) {
        # En 2006, el código 4 de la variable r2 identifica población migrante.
        extranjero[mig_raw == 4] <- 1
        extranjero[!is.na(mig_raw) & mig_raw != 4] <- 0
      }

      mujer <- recode_binary(sexo_raw, yes = 2, no = 1)

      educ_sup <- rep(NA_real_, length(educ_raw))
      if (y == 2006) {
        educ_sup[educ_raw == 8] <- 1
        educ_sup[educ_raw != 8 | is.na(educ_raw)] <- 0
        educ_sup[educ_raw == 99] <- NA
      } else if (y == 2009) {
        educ_sup[educ_raw == 9] <- 1
        educ_sup[!is.na(educ_raw) & educ_raw < 9] <- 0
        educ_sup[educ_raw == 99] <- NA
      } else if (y == 2011) {
        educ_sup[educ_raw == 8] <- 1
        educ_sup[!is.na(educ_raw) & educ_raw < 8] <- 0
        educ_sup[educ_raw == 99] <- NA
      } else if (y %in% c(2013, 2015, 2017)) {
        educ_sup[educ_raw %in% c(8, 10, 11, 12)] <- 1
        educ_sup[educ_raw %in% c(0, 1, 2, 3, 4, 5, 6, 7, 9)] <- 0
        educ_sup[educ_raw %in% c(99, -88, -99)] <- NA
      } else if (y %in% c(2020, 2022, 2024)) {
        # Para CASEN 2020+ se usa educc (agregado MDSF)
        educ_sup[educ_raw %in% c(5, 6)] <- 1
        educ_sup[educ_raw %in% c(0, 1, 2, 3, 4)] <- 0
        educ_sup[educ_raw %in% c(99, -88, -99)] <- NA
      }

      o1 <- first_existing(raw, c("o1"))
      o2 <- first_existing(raw, c("o2"))
      o3 <- first_existing(raw, c("o3"))
      o6 <- first_existing(raw, c("o6"))
      lfp <- if_else((o1 == 1 | o2 == 1 | o3 == 1 | o6 == 1), 1, 0, missing = NA_real_)
      unemployed <- if_else((o1 == 2 & o2 == 2 & o3 == 2 & o6 == 1), 1, 0, missing = NA_real_)

      e6a <- first_existing(raw, c("e6a"))
      e6b <- first_existing(raw, c("e6b"))
      e6c <- first_existing(raw, c("e6c"))
      any_media_4p <- rep(NA_real_, nrow(raw))
      any_college <- rep(NA_real_, nrow(raw))
      if (y == 2011) {
        valid <- !is.na(e6a) & !is.na(e6c) & e6a < 99 & e6c < 99
        any_media_4p[valid] <- as.numeric((e6a[valid] >= 11) | (e6a[valid] >= 7 & e6a[valid] <= 10 & e6c[valid] >= 4))
        any_college[!is.na(e6a) & e6a < 99] <- as.numeric(e6a[!is.na(e6a) & e6a < 99] >= 12)
      } else if (y %in% c(2013, 2015, 2017)) {
        valid <- !is.na(e6a) & !is.na(e6b) & e6a < 99 & e6b < 99
        any_media_4p[valid] <- as.numeric((e6a[valid] >= 12) | (e6a[valid] >= 8 & e6a[valid] <= 11 & e6b[valid] >= 4))
        any_college[!is.na(e6a) & e6a < 99] <- as.numeric(e6a[!is.na(e6a) & e6a < 99] >= 14)
      }

      o17 <- if (!is.na(spec_row$contrato_var[[1]]) && spec_row$contrato_var[[1]] %in% names(raw)) as_num(raw[[spec_row$contrato_var[[1]]]]) else NA_real_
      horas_trabajo <- ifelse(!is.na(horas_trabajo) & horas_trabajo >= 999, NA, horas_trabajo)

      if (y == 2024 && "estrato" %in% names(raw)) comuna <- floor(as_num(raw$estrato) / 100)

      clean <- tibble(
        year = as.integer(y),
        extranjero = as.integer(extranjero),
        mujer = as.integer(mujer),
        edad = as.numeric(edad),
        numper = as.numeric(numper),
        educ_sup = as.integer(educ_sup),
        any_media_4p = as.integer(any_media_4p),
        any_college = as.integer(any_college),
        lfp = as.integer(lfp),
        unemployed = as.integer(unemployed),
        horas_trabajo = as.numeric(horas_trabajo),
        ing_act_prin = as.numeric(ing_act_prin),
        ing_del_trabajo = as.numeric(ing_del_trabajo),
        o17 = as.numeric(o17),
        region = as.numeric(region),
        comuna = as.numeric(comuna),
        zona = as.numeric(zona),
        hacinamiento = as.numeric(hacinamiento),
        expr = as.numeric(expr),
        expc = as.numeric(expc),
        esc = as.numeric(esc),
        lp = as.numeric(lp),
        pobreza = as.numeric(pobreza),
        pobreza_multi = as.numeric(pobreza_multi),
        yautaj = as.numeric(ifelse(is.na(yaut_core), yautaj_alt, yaut_core)),
        ysubaj = as.numeric(ifelse(is.na(ysub_core), ysubaj_alt, ysub_core)),
        qaut = as.numeric(ifelse(is.na(qaut_core), qaut_alt, qaut_core)),
        source_file = source_used
      )

      log_info(sprintf("CASEN %s: n raw=%s | n clean=%s", y, n0, nrow(clean)))
      out_year <- file.path(interim_dir, sprintf("casen_%s_clean.rds", y))
      saveRDS(clean, out_year)
      log_info(sprintf("CASEN %s: guardado %s", y, out_year))
      clean
    }

    cleaned_list <- vector("list", nrow(year_specs))
    for (i in seq_len(nrow(year_specs))) {
      y <- year_specs$year[i]
      cache_path <- file.path(interim_dir, sprintf("casen_%s_clean.rds", y))
      if (!isTRUE(force_rebuild) && file.exists(cache_path)) {
        log_info(sprintf("CASEN %s: usando cache %s", y, cache_path))
        cleaned_list[[i]] <- readRDS(cache_path)
      } else {
        cleaned_list[[i]] <- clean_one_year(year_specs[i, ])
      }
    }
    cleaned_list <- cleaned_list[!vapply(cleaned_list, is.null, logical(1))]
    if (length(cleaned_list) == 0) stop("No se pudo construir ningún año CASEN")

    panel <- bind_rows(cleaned_list)

    panel_counts <- panel |>
      count(year, name = "n") |>
      arrange(year)

    na_checks <- panel |>
      group_by(year) |>
      summarise(
        n = n(),
        na_extranjero = sum(is.na(extranjero)),
        na_edad = sum(is.na(edad)),
        na_expr = sum(is.na(expr)),
        na_educ_sup = sum(is.na(educ_sup)),
        na_horas = sum(is.na(horas_trabajo)),
        .groups = "drop"
      )

    readr::write_csv(panel_counts, file.path(interim_dir, "casen_panel_n_by_year.csv"))
    readr::write_csv(na_checks, file.path(interim_dir, "casen_panel_na_checks.csv"))

    # Validacion especifica de incorporacion 2020
    val_2020 <- panel |>
      filter(year %in% c(2017, 2020)) |>
      group_by(year) |>
      summarise(
        n_total = n(),
        n_expr_valid = sum(!is.na(expr) & expr > 0),
        n_fig1_stock = sum(!is.na(extranjero) & !is.na(mujer)),
        n_fig2_educ_25_50 = sum(!is.na(extranjero) & edad >= 25 & edad <= 50 & !is.na(educ_sup), na.rm = TRUE),
        n_fig3_hacin = sum(!is.na(extranjero) & !is.na(hacinamiento) & hacinamiento %in% c(1,2,3), na.rm = TRUE),
        n_fig4_horas = sum(!is.na(extranjero) & !is.na(horas_trabajo) & horas_trabajo <= 80, na.rm = TRUE),
        n_fig5_contrato = sum(!is.na(extranjero) & !is.na(o17), na.rm = TRUE),
        n_fig6_salario = sum(!is.na(extranjero) & !is.na(ing_del_trabajo) & ing_del_trabajo <= 5000000 & !is.na(horas_trabajo) & horas_trabajo > 0, na.rm = TRUE),
        n_fig7_rural = sum(!is.na(extranjero) & !is.na(zona), na.rm = TRUE),
        n_fig8_escolaridad = sum(!is.na(extranjero) & edad >= 25 & edad <= 50 & !is.na(esc), na.rm = TRUE),
        n_fig9_pobreza_multi = sum(!is.na(extranjero) & !is.na(pobreza_multi), na.rm = TRUE),
        share_migrante_w = weighted.mean(as.numeric(extranjero == 1), w = expr, na.rm = TRUE),
        .groups = "drop"
      )

    var_missing_2020 <- panel |>
      filter(year == 2020) |>
      summarise(
        zona_all_na = all(is.na(zona)),
        hacin_all_na = all(is.na(hacinamiento)),
        contrato_all_na = all(is.na(o17)),
        pobreza_multi_all_na = all(is.na(pobreza_multi)),
        expc_all_na = all(is.na(expc))
      )

    readr::write_csv(val_2020, file.path(interim_dir, "casen_2020_vs_2017_validation.csv"))
    readr::write_csv(var_missing_2020, file.path(interim_dir, "casen_2020_missingness_flags.csv"))

    # Validacion de codigos por año
    code_rows <- list()
    for (i in seq_len(nrow(year_specs))) {
      y <- year_specs$year[i]
      p <- file.path(raw_root, year_specs$raw_file[i])
      p_alt <- year_specs$raw_file_alt[i]
      mig <- year_specs$mig_var[i]
      edu <- year_specs$educ_var[i]

      x <- NULL
      if (file.exists(p)) {
        x <- tryCatch(read_dta(p, col_select = all_of(unique(c(mig, edu))), encoding = "latin1"), error = function(e) NULL)
      }
      if (is.null(x) && !is.na(p_alt) && file.exists(file.path(raw_root, p_alt)) && file.info(file.path(raw_root, p_alt))$size > 0) {
        env <- new.env(parent = emptyenv())
        load(file.path(raw_root, p_alt), envir = env)
        obj <- env[[ls(env)[1]]]
        if (inherits(obj, c("data.frame", "tbl_df"))) x <- obj
      }

      if (is.null(x)) {
        code_rows[[length(code_rows) + 1]] <- tibble(year = y, variable = "status", code = NA_character_, n = NA_integer_, note = "read_error_or_missing")
        next
      }

      m <- x |>
        count(code = .data[[mig]], name = "n") |>
        mutate(year = y, variable = paste0("mig_", mig), note = "", code = as.character(code))
      e <- x |>
        count(code = .data[[edu]], name = "n") |>
        mutate(year = y, variable = paste0("edu_", edu), note = "", code = as.character(code))
      code_rows[[length(code_rows) + 1]] <- bind_rows(m, e)
    }
    readr::write_csv(bind_rows(code_rows) |> select(year, variable, code, n, note), file.path(interim_dir, "casen_code_consistency.csv"))

    final_dir <- here::here("data", "final", "casen")
    ensure_dir(final_dir)

    saveRDS(panel, file.path(final_dir, "casen_panel.rds"))
    log_info(sprintf("Panel CASEN guardado en %s", file.path(final_dir, "casen_panel.rds")))

    readme_lines <- c(
      "# CASEN Panel Harmonizado",
      "",
      sprintf("Fecha de generacion: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      "",
      "## Ubicacion",
      "- `data/final/casen/casen_panel.rds`",
      "",
      "## Variables incluidas",
      "- year, extranjero, mujer, edad, numper",
      "- educ_sup, any_media_4p, any_college",
      "- lfp, unemployed",
      "- horas_trabajo, ing_act_prin, ing_del_trabajo, o17",
      "- region, comuna, zona, hacinamiento",
      "- expr, expc, esc, lp, pobreza, pobreza_multi",
      "- yautaj, ysubaj, qaut, source_file",
      "",
      "## Criterios de harmonizacion",
      "- Basado en los libros de códigos oficiales disponibles para cada edición.",
      "- Migracion y educacion tienen reglas especificas por año.",
      "- 2022 usa fallback a RData si falla lectura del DTA.",
      "- 2024 usa educc (categoria agregada) para educ_sup.",
      "",
      "## Diferencias por año",
      "- 2020 no disponible en raw.",
      "- 2022 DTA falla en este entorno; se usa casen_2022.RData.",
      "- 2024 comuna se aproxima con r1b (revisar con libro de codigos).",
      "",
      "## Archivos de validacion",
      "- casen_crosswalk_variables.csv",
      "- casen_code_consistency.csv",
      "- casen_panel_n_by_year.csv",
      "- casen_panel_na_checks.csv"
    )
    readr::write_lines(readme_lines, file.path(final_dir, "README_panel.md"))

    panel
  }

  build_figures <- function(panel) {
    # ------------------------------------------------------------------------
    # 4) Preparar carpetas de salida para comparacion y publicacion
    # ------------------------------------------------------------------------
    fig_root <- here::here("figures", "casen")
    tbl_root <- here::here("tables", "casen")
    fig_historical <- fig_root
    fig_update <- fig_root
    tbl_compare <- tbl_root
    tbl_explore <- tbl_root
    tbl_update <- tbl_root

    ensure_dir(fig_historical)
    ensure_dir(fig_update)
    ensure_dir(tbl_compare)
    ensure_dir(tbl_explore)
    ensure_dir(tbl_update)

    color_chilenos <- "#315b7c"
    color_migrantes <- "#d26a4a"
    color_hacin_sin <- "#7ea3c9"
    color_hacin_medio <- "#f1bf72"
    color_hacin_crit <- "#c95b5b"
    color_region <- "#4f7c82"
    color_quintil <- "#8d6cab"
    base_family <- pick_base_family()

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
    region_order_norte_sur <- c(15, 1, 2, 3, 4, 5, 13, 6, 7, 16, 8, 9, 14, 10, 11, 12)

    panel <- panel |>
      mutate(
        expr = if_else(is.na(expr) | expr <= 0, 1, expr),
        contrato_bin = case_when(
          year == 2009 & o17 %in% c(1, 2) ~ 1,
          year == 2009 & o17 %in% c(3, 4) ~ 0,
          year %in% c(2011, 2013, 2015, 2017) & o17 %in% c(1, 2) ~ 1,
          year %in% c(2011, 2013, 2015, 2017) & o17 %in% c(3, 4) ~ 0,
          year == 2022 & o17 == 1 ~ 1,
          year == 2022 & o17 == 2 ~ 0,
          year == 2024 & o17 == 1 ~ 1,
          year == 2024 & o17 == 0 ~ 0,
          TRUE ~ NA_real_
        ),
        inflacion = vapply(year, inflation_factor, numeric(1)),
        salario_hora = if_else(
          !is.na(horas_trabajo) & horas_trabajo > 0,
          (ing_act_prin * inflacion) / (4 * horas_trabajo),
          NA_real_
        ),
        grupo = if_else(extranjero == 1, "Migrantes", "Chilenos")
      )

    public_name <- function(name_base) {
      names <- c(
        fig_1_stock_migrantes = "figura_01_stock_migrantes", fig_11_participacion_laboral = "figura_02_participacion_laboral",
        fig_14_tasa_desocupacion = "figura_03_tasa_desocupacion", fig_15_ingreso_laboral_total = "figura_04_ingreso_laboral_total",
        fig_4_horas = "figura_05_horas", fig_5_contrato = "figura_06_contrato", fig_6_salario_hora = "figura_07_salario_hora",
        fig_8_escolaridad = "figura_08_escolaridad", fig_9_pobreza_multi = "figura_09_pobreza_multi",
        fig_12_migrantes_region = "figura_10_migrantes_region", fig_14_mapa_comunal_migrantes = "figura_11_mapa_comunal_migrantes",
        fig_3_hacinamiento = "figura_12_hacinamiento", fig_7_rural = "figura_13_rural", fig_13_migrantes_quintil = "figura_14_migrantes_quintil",
        fig_2_educ_sup_25_50 = "figura_15_educ_sup_25_50", fig_10_piramide_migrantes_2024 = "figura_16_piramide_migrantes_2024",
        fig_10_1_piramide_nativos_2024 = "figura_17_piramide_nativos_2024", fig_01_perfil_socioeconomico_2024 = "figura_18_perfil_socioeconomico_2024",
        fig_02_condiciones_vida_2024 = "figura_19_condiciones_vida_2024", fig_03_regiones_migrantes_2024 = "figura_20_regiones_migrantes_2024",
        fig_04_piramide_migrantes_2024 = "figura_21_piramide_migrantes_2024"
      )
      unname(names[[name_base]])
    }

    save_plot <- function(p, name_base, target = c("historical", "current"), w = 10, h = 6, save_svg = FALSE) {
      target <- match.arg(target)
      out_dir <- switch(target, historical = fig_historical, current = fig_update)
      output_name <- public_name(name_base)
      if (is.null(output_name)) return(invisible(NULL))
      p_clean <- p +
        labs(title = NULL, subtitle = NULL, caption = NULL) +
        theme(
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          plot.caption = element_blank()
        )
      ggsave(file.path(out_dir, paste0(output_name, ".png")), p_clean, width = w, height = h, dpi = 320, bg = "white")
    }
    write_fig_table <- function(df, out_dir, figure_name, alias_name = NULL) {
      output_name <- public_name(figure_name)
      if (!is.null(output_name)) write_csv(df, file.path(out_dir, paste0(output_name, ".csv")))
    }

    get_years <- function(df) sort(unique(df$year))

    theme_migra <- function() {
      theme_minimal(base_size = 12, base_family = base_family) +
        theme(
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          plot.caption = element_blank(),
          axis.title = element_text(size = 11, colour = "#102a43"),
          axis.text.x = element_text(colour = "#243b53"),
          axis.text.y = element_text(colour = "#243b53"),
          legend.title = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 10, colour = "#243b53"),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#d9e2ec", linewidth = 0.35),
          plot.margin = margin(10, 16, 10, 10)
        )
    }

    format_percent <- scales::label_number(accuracy = 0.1, suffix = "%", decimal.mark = ",")
    format_number <- scales::label_number(big.mark = ".", decimal.mark = ",")

    make_line_plot <- function(df, y_var, title, y_lab, colors, percent_axis = FALSE) {
      p <- ggplot(df, aes(year, .data[[y_var]], color = grupo)) +
        geom_line(linewidth = 1.1) +
        geom_point(size = 2.3) +
        scale_x_continuous(breaks = get_years(df)) +
        scale_color_manual(values = colors) +
        labs(title = title, x = "A\u00f1o", y = y_lab) +
        theme_migra()
      if (percent_axis) p <- p + scale_y_continuous(labels = format_percent)
      if (!percent_axis) p <- p + scale_y_continuous(labels = format_number)
      p
    }

    make_group_bar_plot <- function(df, y_var, title, y_lab, percent_axis = TRUE) {
      p <- ggplot(df, aes(factor(year, levels = get_years(df)), .data[[y_var]], fill = grupo)) +
        geom_col(position = position_dodge(width = 0.75), width = 0.65) +
        scale_fill_manual(values = c("Chilenos" = color_chilenos, "Migrantes" = color_migrantes)) +
        labs(title = title, x = "A\u00f1o", y = y_lab) +
        theme_migra()
      if (percent_axis) p <- p + scale_y_continuous(labels = format_percent)
      if (!percent_axis) p <- p + scale_y_continuous(labels = format_number)
      p
    }

    # ----------------------------------------------------------------------
    # Figura 1: Stock migrante acumulado en el tiempo
    # ----------------------------------------------------------------------
    f1 <- panel |>
      filter(!is.na(mujer), extranjero == 1) |>
      mutate(sexo = if_else(mujer == 1, "Mujeres", "Hombres")) |>
      group_by(year, sexo) |>
      summarise(cantidad = sum(expr, na.rm = TRUE), .groups = "drop")
    write_fig_table(f1, tbl_compare, "fig_1_stock_migrantes", "fig_1_series")

    p1_historical <- ggplot(filter(f1, year <= 2022), aes(year, cantidad, color = sexo)) +
      geom_line(linewidth = 1.1) + geom_point(size = 2.3) +
      scale_x_continuous(breaks = get_years(filter(f1, year <= 2022))) +
      scale_y_continuous(labels = format_number) +
      scale_color_manual(values = c("Hombres" = color_chilenos, "Mujeres" = color_migrantes)) +
      labs(title = "Figura 1. Stock acumulado de migrantes en el tiempo", x = "A\u00f1o", y = "Cantidad de migrantes") +
      theme_migra()
    p1_update <- ggplot(f1, aes(year, cantidad, color = sexo)) +
      geom_line(linewidth = 1.1) + geom_point(size = 2.3) +
      scale_x_continuous(breaks = get_years(f1)) +
      scale_y_continuous(labels = format_number) +
      scale_color_manual(values = c("Hombres" = color_chilenos, "Mujeres" = color_migrantes)) +
      labs(title = "Figura 1. Stock acumulado de migrantes en el tiempo", x = "A\u00f1o", y = "Cantidad de migrantes") +
      theme_migra()
    save_plot(p1_historical, "fig_1_stock_migrantes", "historical")
    save_plot(p1_update, "fig_1_stock_migrantes", "current")

    # ----------------------------------------------------------------------
    # Figura 2: Educación superior entre 25 y 50 años
    # ----------------------------------------------------------------------
    f2_wide <- panel |>
      filter(!is.na(mujer), edad >= 25, edad <= 50, !is.na(extranjero)) |>
      group_by(year) |>
      summarise(
        den_nativo = sum(expr * as.numeric(extranjero == 0), na.rm = TRUE),
        den_migrante = sum(expr * as.numeric(extranjero == 1), na.rm = TRUE),
        num_nativo = sum(expr * as.numeric(extranjero == 0 & educ_sup == 1), na.rm = TRUE),
        num_migrante = sum(expr * as.numeric(extranjero == 1 & educ_sup == 1), na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        pct_chilenos = 100 * num_nativo / den_nativo,
        pct_migrantes = 100 * num_migrante / den_migrante
      )
    f2 <- bind_rows(
      f2_wide |> transmute(year, grupo = "Chilenos", pct = pct_chilenos),
      f2_wide |> transmute(year, grupo = "Migrantes", pct = pct_migrantes)
    )
    write_fig_table(f2, tbl_compare, "fig_2_educ_sup_25_50", "fig_2_series")
    save_plot(make_group_bar_plot(filter(f2, year <= 2022), "pct", "Figura 2. Poblaci\u00f3n de 25 a 50 a\u00f1os con educaci\u00f3n superior", "Porcentaje"), "fig_2_educ_sup_25_50", "historical")
    save_plot(make_group_bar_plot(f2, "pct", "Figura 2. Poblaci\u00f3n de 25 a 50 a\u00f1os con educaci\u00f3n superior", "Porcentaje"), "fig_2_educ_sup_25_50", "current")

    # ----------------------------------------------------------------------
    # Figura 3: Distribución del hacinamiento
    # ----------------------------------------------------------------------
    f3 <- panel |>
      filter(!is.na(mujer), !is.na(extranjero), !is.na(hacinamiento), !hacinamiento %in% c(4, 9, -88), hacinamiento %in% c(1, 2, 3)) |>
      mutate(
        grupo = if_else(extranjero == 1, "Migrantes", "Chilenos"),
        hac_key = case_when(
          hacinamiento == 1 ~ "sin_hacinamiento",
          hacinamiento == 2 ~ "hacinamiento_medio",
          hacinamiento == 3 ~ "hacinamiento_critico",
          TRUE ~ NA_character_
        ),
        hac_cat = case_when(
          hacinamiento == 1 ~ "Sin hacinamiento",
          hacinamiento == 2 ~ "Hacinamiento medio",
          hacinamiento == 3 ~ "Hacinamiento cr\u00edtico",
          TRUE ~ NA_character_
        ),
        hac_key = factor(hac_key, levels = c("sin_hacinamiento", "hacinamiento_medio", "hacinamiento_critico"))
      ) |>
      group_by(year, grupo, hac_key, hac_cat) |>
      summarise(w_n = sum(expr, na.rm = TRUE), .groups = "drop") |>
      group_by(year, grupo) |>
      mutate(pct = 100 * w_n / sum(w_n, na.rm = TRUE)) |>
      ungroup()
    write_fig_table(f3, tbl_compare, "fig_3_hacinamiento", "fig_3_series")

    make_hacin_plot <- function(df) {
      ggplot(df, aes(factor(year, levels = get_years(df)), pct, fill = hac_key)) +
        geom_col(position = "stack", width = 0.68) +
        facet_wrap(~grupo) +
        scale_fill_manual(values = c(
          "sin_hacinamiento" = color_hacin_sin,
          "hacinamiento_medio" = color_hacin_medio,
          "hacinamiento_critico" = color_hacin_crit
        ),
        breaks = c("sin_hacinamiento", "hacinamiento_medio", "hacinamiento_critico"),
        labels = c("Sin hacinamiento", "Hacinamiento medio", "Hacinamiento cr\u00edtico")) +
        scale_y_continuous(labels = format_percent) +
        labs(title = "Figura 3. Distribuci\u00f3n de la poblaci\u00f3n seg\u00fan nivel de hacinamiento", x = "A\u00f1o", y = "Distribuci\u00f3n") +
        theme_migra()
    }
    save_plot(make_hacin_plot(filter(f3, year <= 2022)), "fig_3_hacinamiento", "historical", w = 12, h = 6)
    save_plot(make_hacin_plot(f3), "fig_3_hacinamiento", "current", w = 12, h = 6)

    # ----------------------------------------------------------------------
    # Figura 4: Horas semanales promedio trabajadas
    # ----------------------------------------------------------------------
    f4 <- panel |>
      filter(year != 2020, !is.na(horas_trabajo), horas_trabajo <= 80, !is.na(extranjero)) |>
      mutate(grupo = if_else(extranjero == 1, "Migrantes", "Chilenos")) |>
      group_by(year, grupo) |>
      summarise(horas = weighted.mean(horas_trabajo, w = expr, na.rm = TRUE), .groups = "drop")
    write_fig_table(f4, tbl_compare, "fig_4_horas", "fig_4_series")
    save_plot(make_line_plot(filter(f4, year <= 2022), "horas", "Figura 4. Horas semanales promedio trabajadas", "Horas", c("Chilenos" = color_chilenos, "Migrantes" = color_migrantes), percent_axis = FALSE), "fig_4_horas", "historical")
    save_plot(make_line_plot(f4, "horas", "Figura 4. Horas semanales promedio trabajadas", "Horas", c("Chilenos" = color_chilenos, "Migrantes" = color_migrantes), percent_axis = FALSE), "fig_4_horas", "current")

    # ----------------------------------------------------------------------
    # Figura 5: Población ocupada con contrato de trabajo
    # ----------------------------------------------------------------------
    f5 <- panel |>
      filter(year != 2006, year != 2009, year != 2020, !is.na(contrato_bin), !is.na(extranjero)) |>
      mutate(grupo = if_else(extranjero == 1, "Migrantes", "Chilenos")) |>
      group_by(year, grupo) |>
      summarise(pct = 100 * weighted.mean(contrato_bin, w = expr, na.rm = TRUE), .groups = "drop")
    write_fig_table(f5, tbl_compare, "fig_5_contrato", "fig_5_series")
    save_plot(make_group_bar_plot(filter(f5, year <= 2022), "pct", "Figura 5. Poblaci\u00f3n ocupada con contrato de trabajo", "Porcentaje"), "fig_5_contrato", "historical")
    save_plot(make_group_bar_plot(f5, "pct", "Figura 5. Poblaci\u00f3n ocupada con contrato de trabajo", "Porcentaje"), "fig_5_contrato", "current")

    # ----------------------------------------------------------------------
    # Figura 6: Salario promedio por hora
    # ----------------------------------------------------------------------
    f6 <- panel |>
      filter(year != 2020, !is.na(ing_del_trabajo), ing_del_trabajo <= 5000000, !is.na(salario_hora), !is.na(extranjero)) |>
      mutate(grupo = if_else(extranjero == 1, "Migrantes", "Chilenos")) |>
      group_by(year, grupo) |>
      summarise(salario = weighted.mean(salario_hora, w = expr, na.rm = TRUE), .groups = "drop")
    write_fig_table(f6, tbl_compare, "fig_6_salario_hora", "fig_6_series")
    save_plot(make_line_plot(filter(f6, year <= 2022), "salario", "Figura 6. Salario promedio por hora", "Pesos chilenos", c("Chilenos" = color_chilenos, "Migrantes" = color_migrantes), percent_axis = FALSE), "fig_6_salario_hora", "historical")
    save_plot(make_line_plot(f6, "salario", "Figura 6. Salario promedio por hora", "Pesos chilenos", c("Chilenos" = color_chilenos, "Migrantes" = color_migrantes), percent_axis = FALSE), "fig_6_salario_hora", "current")

    # ----------------------------------------------------------------------
    # Figura 15: Ingreso laboral total
    # ----------------------------------------------------------------------
    f15 <- panel |>
      filter(
        year != 2020,
        !is.na(ing_del_trabajo),
        ing_del_trabajo >= 0,
        ing_del_trabajo <= 10000000,
        !is.na(extranjero)
      ) |>
      mutate(grupo = if_else(extranjero == 1, "Migrantes", "Chilenos")) |>
      group_by(year, grupo) |>
      summarise(
        ingreso_total = weighted.mean(ing_del_trabajo * inflacion, w = expr, na.rm = TRUE),
        .groups = "drop"
      )
    write_fig_table(f15, tbl_explore, "fig_15_ingreso_laboral_total")
    save_plot(
      make_line_plot(
        f15,
        "ingreso_total",
        "Figura 15. Ingreso laboral total promedio",
        "Pesos chilenos",
        c("Chilenos" = color_chilenos, "Migrantes" = color_migrantes),
        percent_axis = FALSE
      ),
      "fig_15_ingreso_laboral_total",
      "current"
    )

    # ----------------------------------------------------------------------
    # Figura 7: Población que vive en zonas rurales
    # ----------------------------------------------------------------------
    f7 <- panel |>
      filter(!is.na(mujer), !is.na(zona), !is.na(extranjero)) |>
      mutate(rural = if_else(zona == 2, 1, if_else(zona == 1, 0, NA_real_)), grupo = if_else(extranjero == 1, "Migrantes", "Chilenos")) |>
      filter(!is.na(rural)) |>
      group_by(year, grupo) |>
      summarise(pct = 100 * weighted.mean(rural, w = expr, na.rm = TRUE), .groups = "drop")
    write_fig_table(f7, tbl_compare, "fig_7_rural", "fig_7_series")
    save_plot(make_group_bar_plot(filter(f7, year <= 2022), "pct", "Figura 7. Poblaci\u00f3n que vive en zonas rurales", "Porcentaje"), "fig_7_rural", "historical")
    save_plot(make_group_bar_plot(f7, "pct", "Figura 7. Poblaci\u00f3n que vive en zonas rurales", "Porcentaje"), "fig_7_rural", "current")

    # ----------------------------------------------------------------------
    # Figura 8: Años promedio de escolaridad (25-50)
    # ----------------------------------------------------------------------
    f8 <- panel |>
      filter(edad >= 25, edad <= 50, !is.na(esc), !is.na(extranjero)) |>
      mutate(grupo = if_else(extranjero == 1, "Migrantes", "Chilenos")) |>
      group_by(year, grupo) |>
      summarise(esc_mean = weighted.mean(esc, w = expr, na.rm = TRUE), .groups = "drop")
    write_fig_table(f8, tbl_compare, "fig_8_escolaridad", "fig_8_series")
    save_plot(make_line_plot(filter(f8, year <= 2022), "esc_mean", "Figura 8. A\u00f1os promedio de escolaridad (25 a 50 a\u00f1os)", "Años", c("Chilenos" = color_chilenos, "Migrantes" = color_migrantes), percent_axis = FALSE), "fig_8_escolaridad", "historical")
    save_plot(make_line_plot(f8, "esc_mean", "Figura 8. A\u00f1os promedio de escolaridad (25 a 50 a\u00f1os)", "Años", c("Chilenos" = color_chilenos, "Migrantes" = color_migrantes), percent_axis = FALSE), "fig_8_escolaridad", "current")

    # ----------------------------------------------------------------------
    # Figura 9: Pobreza multidimensional
    # ----------------------------------------------------------------------
    f9 <- panel |>
      filter(year %in% c(2013, 2015, 2017, 2022, 2024), !is.na(pobreza_multi), !is.na(extranjero)) |>
      mutate(grupo = if_else(extranjero == 1, "Migrantes", "Chilenos")) |>
      group_by(year, grupo) |>
      summarise(pct = 100 * weighted.mean(as.numeric(pobreza_multi == 1), w = expr, na.rm = TRUE), .groups = "drop")
    write_fig_table(f9, tbl_compare, "fig_9_pobreza_multi", "fig_9_series")
    save_plot(make_group_bar_plot(filter(f9, year <= 2022), "pct", "Figura 9. Poblaci\u00f3n en situaci\u00f3n de pobreza multidimensional", "Porcentaje"), "fig_9_pobreza_multi", "historical")
    save_plot(make_group_bar_plot(f9, "pct", "Figura 9. Poblaci\u00f3n en situaci\u00f3n de pobreza multidimensional", "Porcentaje"), "fig_9_pobreza_multi", "current")

    # ----------------------------------------------------------------------
    # Figura 10: Pirámide poblacional
    # ----------------------------------------------------------------------
    build_pyramid <- function(df, target_year, target_extranjero, label_title) {
      d <- df |>
        filter(year == target_year, extranjero == target_extranjero, !is.na(edad), !is.na(mujer)) |>
        mutate(edad_top = pmin(edad, 90), grupo_edad = 5 * floor(edad_top / 5)) |>
        group_by(grupo_edad, mujer) |>
        summarise(pop = sum(expr, na.rm = TRUE), .groups = "drop")
      total <- sum(d$pop, na.rm = TRUE)
      if (total == 0) return(NULL)
      d <- d |>
        mutate(pct = 100 * pop / total, pct_plot = if_else(mujer == 0, -pct, pct), sexo = if_else(mujer == 0, "Hombres", "Mujeres"))
      p <- ggplot(d, aes(x = grupo_edad, y = pct_plot, fill = sexo)) +
        geom_col(width = 4.5) +
        coord_flip() +
        scale_fill_manual(values = c("Hombres" = color_chilenos, "Mujeres" = color_migrantes)) +
        scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
        labs(title = label_title, x = "Edad (tramos de 5 a\u00f1os)", y = "Porcentaje") +
        theme_migra()
      list(plot = p, data = d)
    }

    p10_historical_m <- build_pyramid(panel, 2022, 1, "Pirámide poblacional de migrantes (2022)")
    p10_historical_n <- build_pyramid(panel, 2022, 0, "Pirámide poblacional de chilenos (2022)")
    if (!is.null(p10_historical_m)) {
      save_plot(p10_historical_m$plot, "fig_10_piramide_migrantes_2022", "historical", w = 9, h = 7)
      write_fig_table(p10_historical_m$data, tbl_compare, "fig_10_piramide_migrantes_2022")
    }
    if (!is.null(p10_historical_n)) {
      save_plot(p10_historical_n$plot, "fig_10_1_piramide_nativos_2022", "historical", w = 9, h = 7)
      write_fig_table(p10_historical_n$data, tbl_compare, "fig_10_1_piramide_nativos_2022")
    }

    p10_update_m <- build_pyramid(panel, 2024, 1, "Figura 10. Pir\u00e1mide poblacional de migrantes (2024)")
    p10_update_n <- build_pyramid(panel, 2024, 0, "Figura 10.1. Pir\u00e1mide poblacional de chilenos (2024)")
    if (!is.null(p10_update_m)) {
      save_plot(p10_update_m$plot, "fig_10_piramide_migrantes_2024", "current", w = 9, h = 7)
      write_fig_table(p10_update_m$data, tbl_compare, "fig_10_piramide_migrantes_2024")
    }
    if (!is.null(p10_update_n)) {
      save_plot(p10_update_n$plot, "fig_10_1_piramide_nativos_2024", "current", w = 9, h = 7)
      write_fig_table(p10_update_n$data, tbl_compare, "fig_10_1_piramide_nativos_2024")
    }

    # ----------------------------------------------------------------------
    # Analisis exploratorio 1: participación laboral
    # ----------------------------------------------------------------------
    fx1 <- panel |>
      filter(!is.na(lfp), !is.na(extranjero)) |>
      mutate(grupo = if_else(extranjero == 1, "Migrantes", "Chilenos")) |>
      group_by(year, grupo) |>
      summarise(pct = 100 * weighted.mean(lfp, w = expr, na.rm = TRUE), .groups = "drop")
    write_fig_table(fx1, tbl_explore, "fig_11_participacion_laboral", "extra_1_participacion_laboral")
    save_plot(make_line_plot(fx1, "pct", "Figura 11. Participación laboral", "Porcentaje", c("Chilenos" = color_chilenos, "Migrantes" = color_migrantes), percent_axis = TRUE), "fig_11_participacion_laboral", "current")

    # ----------------------------------------------------------------------
    # Analisis exploratorio 2: porcentaje de migrantes por región
    # ----------------------------------------------------------------------
    latest_region_year <- max(panel$year[!is.na(panel$region)])
    fx2 <- panel |>
      filter(year == latest_region_year, !is.na(region), !is.na(extranjero), region %in% region_order_norte_sur) |>
      group_by(region) |>
      summarise(pct_migrante = 100 * weighted.mean(as.numeric(extranjero == 1), w = expr, na.rm = TRUE), .groups = "drop") |>
      arrange(match(region, region_order_norte_sur)) |>
      mutate(
        region_label = region_labels[as.character(region)],
        region_factor = factor(region, levels = rev(region_order_norte_sur), labels = region_labels_plot[as.character(rev(region_order_norte_sur))])
      )
    write_fig_table(fx2, tbl_explore, "fig_12_migrantes_region", "extra_2_migrantes_region")

    px2 <- ggplot(fx2, aes(x = region_factor, y = pct_migrante)) +
      geom_col(fill = color_region, width = 0.7) +
      coord_flip() +
      scale_y_continuous(labels = format_percent) +
      labs(title = paste0("Figura 12. Porcentaje de migrantes por regi\u00f3n (", latest_region_year, ")"), x = "Regi\u00f3n", y = "Porcentaje") +
      theme_migra() +
      theme(axis.text.y = element_text(size = 10))
    save_plot(px2, "fig_12_migrantes_region", "current", w = 10.5, h = 7.4)

    # ----------------------------------------------------------------------
    # Analisis exploratorio 4: mapa comunal de migrantes
    # ----------------------------------------------------------------------
    latest_map_year <- max(panel$year[!is.na(panel$comuna)])
    fx4 <- panel |>
      filter(year == latest_map_year, !is.na(comuna), !is.na(extranjero)) |>
      group_by(comuna) |>
      summarise(
        poblacion_total = sum(expr, na.rm = TRUE),
        poblacion_migrante = sum(expr * as.numeric(extranjero == 1), na.rm = TRUE),
        pct_migrante = 100 * poblacion_migrante / poblacion_total,
        .groups = "drop"
      )
    write_fig_table(fx4, tbl_explore, "fig_14_mapa_comunal_migrantes", "extra_4_mapa_comunal_migrantes")

    # Se prioriza 2017 porque 2022 no tiene comuna harmonizada y 2024 no es comparable en el panel actual.
    map_path <- here::here("data", "raw", "casen", "comunas", "COMUNAS_v1.shp")
    comunas_sf <- NULL

    if (requireNamespace("chilemapas", quietly = TRUE)) {
      comunas_sf <- tryCatch(
        chilemapas::mapa_comunas,
        error = function(e) NULL
      )
    }

    if (is.null(comunas_sf) && file.exists(map_path)) {
      comunas_sf <- tryCatch(sf::read_sf(map_path), error = function(e) NULL)
    }

    if (!is.null(comunas_sf)) {
      name_candidates <- intersect(c("nombre_comuna", "nom_com", "comuna", "NOM_COMUNA", "NOM_COM"), names(comunas_sf))
      code_candidates <- intersect(c("codigo_comuna", "cod_comuna", "cod_com", "CUT_COM", "CUT", "comuna"), names(comunas_sf))

      if (length(code_candidates) > 0) {
        code_var <- code_candidates[1]
        comunas_sf[[code_var]] <- suppressWarnings(as.numeric(as.character(comunas_sf[[code_var]])))
        mapa_fx4 <- comunas_sf |>
          dplyr::left_join(fx4, by = stats::setNames("comuna", code_var))

        px4 <- ggplot(mapa_fx4) +
          geom_sf(aes(fill = pct_migrante), color = "white", linewidth = 0.02) +
          scale_fill_viridis_c(option = "magma", direction = -1, na.value = "#f1efe9", labels = format_percent) +
          labs(
            title = paste0("Figura 14. Distribuci\u00f3n comunal de migrantes (", latest_map_year, ")"),
            subtitle = "Porcentaje de migrantes sobre la poblaci\u00f3n comunal expandida",
            fill = "% migrante"
          ) +
          theme_void(base_size = 13, base_family = base_family) +
          theme(
            plot.title = element_text(face = "bold", size = 14, colour = "#16324f", hjust = 0.5),
            plot.subtitle = element_text(size = 10, colour = "#4a5a66", hjust = 0.5),
            legend.position = "right",
            plot.margin = margin(12, 18, 12, 18)
          )
        save_plot(px4, "fig_14_mapa_comunal_migrantes", "current", w = 9.2, h = 12, save_svg = FALSE)
      }
    } else {
      log_info("Mapa comunal no generado: falta `chilemapas` o `data/raw/casen/comunas/COMUNAS_v1.shp`.")
    }

    # ----------------------------------------------------------------------
    # Analisis exploratorio 3: porcentaje de migrantes por quintil
    # ----------------------------------------------------------------------
    latest_q_year <- max(panel$year[!is.na(panel$qaut)])
    fx3 <- panel |>
      filter(year == latest_q_year, qaut %in% 1:5, !is.na(extranjero)) |>
      group_by(qaut) |>
      summarise(pct_migrante = 100 * weighted.mean(as.numeric(extranjero == 1), w = expr, na.rm = TRUE), .groups = "drop") |>
      mutate(quintil = paste("Quintil", qaut))
    write_fig_table(fx3, tbl_explore, "fig_13_migrantes_quintil", "extra_3_migrantes_quintil")

    px3 <- ggplot(fx3, aes(x = factor(quintil, levels = quintil), y = pct_migrante)) +
      geom_col(fill = color_quintil, width = 0.7) +
      scale_y_continuous(labels = format_percent) +
      labs(title = paste0("Figura 13. Porcentaje de migrantes por quintil de ingreso (", latest_q_year, ")"), x = "Quintil", y = "Porcentaje") +
      theme_migra()
    save_plot(px3, "fig_13_migrantes_quintil", "current")

    # ----------------------------------------------------------------------
    # Bloque web 2024: figuras de corte transversal publicadas en la pagina
    # ----------------------------------------------------------------------
    panel_2024 <- panel |>
      filter(year == 2024) |>
      mutate(
        grupo = if_else(extranjero == 1, "Migrantes", "Chilenos"),
        sexo = if_else(mujer == 1, "Mujeres", "Hombres"),
        region_nombre = recode(as.character(as.integer(region)), !!!region_labels),
        region_nombre = if_else(is.na(region_nombre), "Sin información", region_nombre),
        contrato = case_when(o17 == 1 ~ 1, o17 %in% c(0, 2) ~ 0, TRUE ~ NA_real_),
        hacinamiento_alto = case_when(hacinamiento >= 3 ~ 1, !is.na(hacinamiento) ~ 0, TRUE ~ NA_real_),
        zona_rural = case_when(zona == 2 ~ 1, zona == 1 ~ 0, TRUE ~ NA_real_),
        edad_tramo = case_when(
          edad < 5 ~ "0 a 4",
          edad < 10 ~ "5 a 9",
          edad < 15 ~ "10 a 14",
          edad < 20 ~ "15 a 19",
          edad < 25 ~ "20 a 24",
          edad < 30 ~ "25 a 29",
          edad < 35 ~ "30 a 34",
          edad < 40 ~ "35 a 39",
          edad < 45 ~ "40 a 44",
          edad < 50 ~ "45 a 49",
          edad < 55 ~ "50 a 54",
          edad < 60 ~ "55 a 59",
          edad < 65 ~ "60 a 64",
          edad < 70 ~ "65 a 69",
          edad < 75 ~ "70 a 74",
          edad < 80 ~ "75 a 79",
          TRUE ~ "80 o más"
        )
      )

    f_update_1 <- bind_rows(
      panel_2024 |>
        filter(edad >= 25, edad <= 50) |>
        group_by(grupo) |>
        summarise(indicador = "Educación superior, 25-50 años", valor = weighted.mean(educ_sup == 1, expr, na.rm = TRUE), .groups = "drop"),
      panel_2024 |>
        filter(edad >= 18, edad <= 64) |>
        group_by(grupo) |>
        summarise(indicador = "Participación laboral, 18-64 años", valor = weighted.mean(lfp == 1, expr, na.rm = TRUE), .groups = "drop"),
      panel_2024 |>
        filter(edad >= 18, edad <= 64, lfp == 1, unemployed == 0) |>
        group_by(grupo) |>
        summarise(indicador = "Contrato laboral entre ocupados, 18-64 años", valor = weighted.mean(contrato == 1, expr, na.rm = TRUE), .groups = "drop")
    ) |>
      mutate(indicador = factor(indicador, levels = c("Educación superior, 25-50 años", "Participación laboral, 18-64 años", "Contrato laboral entre ocupados, 18-64 años")))
    write_fig_table(f_update_1, tbl_update, "fig_01_perfil_socioeconomico_2024")

    p_update_1 <- ggplot(f_update_1, aes(indicador, valor, fill = grupo)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      scale_fill_manual(values = c("Migrantes" = color_migrantes, "Chilenos" = color_chilenos)) +
      scale_y_continuous(labels = format_percent, expand = expansion(mult = c(0, 0.06))) +
      labs(x = NULL, y = "Porcentaje") +
      theme_migra() +
      theme(axis.text.x = element_text(face = "bold"))
    save_plot(p_update_1, "fig_01_perfil_socioeconomico_2024", "current", w = 11, h = 6)

    f_update_2 <- bind_rows(
      panel_2024 |>
        group_by(grupo) |>
        summarise(indicador = "Pobreza multidimensional", valor = weighted.mean(pobreza_multi == 1, expr, na.rm = TRUE), .groups = "drop"),
      panel_2024 |>
        group_by(grupo) |>
        summarise(indicador = "Hacinamiento alto", valor = weighted.mean(hacinamiento_alto == 1, expr, na.rm = TRUE), .groups = "drop"),
      panel_2024 |>
        group_by(grupo) |>
        summarise(indicador = "Residencia rural", valor = weighted.mean(zona_rural == 1, expr, na.rm = TRUE), .groups = "drop")
    ) |>
      mutate(indicador = factor(indicador, levels = c("Pobreza multidimensional", "Hacinamiento alto", "Residencia rural")))
    write_fig_table(f_update_2, tbl_update, "fig_02_condiciones_vida_2024")

    p_update_2 <- ggplot(f_update_2, aes(indicador, valor, fill = grupo)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      scale_fill_manual(values = c("Migrantes" = color_migrantes, "Chilenos" = "#c9473a")) +
      scale_y_continuous(labels = format_percent, expand = expansion(mult = c(0, 0.06))) +
      labs(x = NULL, y = "Porcentaje") +
      theme_migra() +
      theme(axis.text.x = element_text(face = "bold"))
    save_plot(p_update_2, "fig_02_condiciones_vida_2024", "current", w = 10, h = 6)

    f_update_3 <- panel_2024 |>
      filter(!is.na(region), region_nombre != "Sin información") |>
      group_by(region_nombre) |>
      summarise(
        total_region = sum(expr, na.rm = TRUE),
        migrantes = sum(expr[extranjero == 1], na.rm = TRUE),
        share_migrantes = migrantes / total_region,
        .groups = "drop"
      ) |>
      mutate(region_code = as.integer(names(region_labels)[match(region_nombre, region_labels)])) |>
      filter(region_code %in% region_order_norte_sur) |>
      arrange(match(region_code, region_order_norte_sur))
    write_fig_table(f_update_3, tbl_update, "fig_03_regiones_migrantes_2024")

    p_update_3 <- f_update_3 |>
      mutate(region_plot = factor(wrap_label(region_nombre, width = 24), levels = rev(region_labels_plot[as.character(region_order_norte_sur)]))) |>
      ggplot(aes(migrantes, region_plot, fill = share_migrantes)) +
      geom_col() +
      geom_text(aes(label = paste0(format_number(migrantes), " (", format_percent(100 * share_migrantes), ")")), hjust = -0.05, size = 3.1, colour = "#32475b") +
      scale_fill_gradient(low = "#cfe3f4", high = color_chilenos, labels = scales::label_percent(accuracy = 0.1, decimal.mark = ","), name = "% migrante") +
      scale_x_continuous(labels = format_number, expand = expansion(mult = c(0, 0.18))) +
      labs(x = "Personas", y = NULL) +
      theme_migra()
    save_plot(p_update_3, "fig_03_regiones_migrantes_2024", "current", w = 11.5, h = 7.2)

    f_update_4 <- panel_2024 |>
      filter(extranjero == 1, !is.na(edad_tramo), !is.na(mujer)) |>
      group_by(edad_tramo, sexo) |>
      summarise(personas = sum(expr, na.rm = TRUE), .groups = "drop") |>
      mutate(
        edad_tramo = factor(edad_tramo, levels = c("0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24", "25 a 29", "30 a 34", "35 a 39", "40 a 44", "45 a 49", "50 a 54", "55 a 59", "60 a 64", "65 a 69", "70 a 74", "75 a 79", "80 o más")),
        personas_plot = if_else(sexo == "Hombres", -personas, personas)
      )
    write_fig_table(f_update_4, tbl_update, "fig_04_piramide_migrantes_2024")

    max_abs_update_4 <- max(abs(f_update_4$personas_plot), na.rm = TRUE)
    p_update_4 <- ggplot(f_update_4, aes(personas_plot, edad_tramo, fill = sexo)) +
      geom_col(width = 0.85) +
      scale_fill_manual(values = c("Hombres" = color_chilenos, "Mujeres" = color_migrantes)) +
      scale_x_continuous(labels = function(v) format_number(abs(v)), limits = c(-max_abs_update_4 * 1.12, max_abs_update_4 * 1.12)) +
      labs(x = "Personas", y = NULL) +
      theme_migra()
    save_plot(p_update_4, "fig_04_piramide_migrantes_2024", "current", w = 10, h = 7)

    # ----------------------------------------------------------------------
    # Analisis exploratorio 5: tasa de desocupacion
    # ----------------------------------------------------------------------
    fx5 <- panel |>
      filter(year >= 2011, !is.na(unemployed), !is.na(lfp), lfp == 1, !is.na(extranjero)) |>
      mutate(grupo = if_else(extranjero == 1, "Migrantes", "Chilenos")) |>
      group_by(year, grupo) |>
      summarise(pct = 100 * weighted.mean(as.numeric(unemployed == 1), w = expr, na.rm = TRUE), .groups = "drop")
    write_csv(fx5, file.path(tbl_explore, "figura_03_tasa_desocupacion.csv"))
    save_plot(
      make_line_plot(
        fx5,
        "pct",
        "Figura 14. Tasa de desocupación",
        "Porcentaje",
        c("Chilenos" = color_chilenos, "Migrantes" = color_migrantes),
        percent_axis = TRUE
      ),
      "fig_14_tasa_desocupacion",
      "current"
    )

    log_info(sprintf("Figuras CASEN exportadas en %s", fig_root))
  }

  # --------------------------------------------------------------------------
  # Orquestacion completa
  # --------------------------------------------------------------------------
  panel <- NULL

  if (build %in% c("all", "panel")) {
    panel <- build_panel(force_rebuild = force_rebuild)
  }

  if (build %in% c("all", "figures")) {
    if (is.null(panel)) {
      panel_path <- here::here("data", "final", "casen", "casen_panel.rds")
      if (!file.exists(panel_path)) stop("No existe casen_panel.rds. Corre run_casen(build = \"panel\") primero.")
      log_info(sprintf("Cargando panel desde %s", panel_path))
      panel <- readRDS(panel_path)
    }
    build_figures(panel)
  }

  log_info("CASEN pipeline completo finalizado")
  invisible(panel)
}

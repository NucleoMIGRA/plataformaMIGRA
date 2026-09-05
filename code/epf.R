# EPF
# Fuente de datos: https://www.ine.gob.cl/estadisticas-por-tema/sociedad-y-condiciones-de-vida/encuesta-de-presupuestos-familiares
# Unidad de observación: hogar y gasto clasificado según la CCIF.
# Espera insumos en `data/raw/epf/` y construye datos procesados y productos públicos.
# Aunque la carpeta publica no distribuye esas bases, aqui queda documentada la logica general del flujo:
# 1. lectura de datos originales;
# 2. procesamiento hacia `data/interim/`;
# 3. construccion de paneles en `data/final/`;
# 4. generacion de tablas espejo;
# 5. generacion de figuras publicables;
# 6. exportación hacia `figures/epf/` y `tables/epf/`.

suppressPackageStartupMessages({
  library(here)
  library(haven)
  library(readr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(scales)
  library(purrr)
  library(ragg)
  library(tibble)
})

run_epf <- function(build = c("all", "data", "figures"), force_rebuild = FALSE) {
  build <- match.arg(build)

  for (loc in c("es_CL.UTF-8", "en_US.UTF-8", "C.UTF-8")) {
    ok <- tryCatch(Sys.setlocale("LC_CTYPE", loc), warning = function(w) NA_character_, error = function(e) NA_character_)
    if (!is.na(ok)) break
  }

  raw_root <- here::here("data", "raw", "epf")
  raw_ix <- file.path(raw_root, "IX EPF")
  raw_viii <- file.path(raw_root, "VIII EPF")

  figures_root <- here::here("figures", "epf")
  tables_root <- here::here("tables", "epf")
  fig_snapshot <- figures_root
  tab_snapshot <- tables_root
  fig_compare <- figures_root
  tab_compare <- tables_root

  dir.create(interim_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(fig_snapshot, recursive = TRUE, showWarnings = FALSE)
  dir.create(tab_snapshot, recursive = TRUE, showWarnings = FALSE)
  dir.create(fig_compare, recursive = TRUE, showWarnings = FALSE)
  dir.create(tab_compare, recursive = TRUE, showWarnings = FALSE)

  final_household <- file.path(final_dir, "epf_household_panel.rds")
  final_summary <- file.path(final_dir, "epf_summary_objects.rds")
  final_figures <- file.path(final_dir, "epf_figure_objects.rds")

  log_msg <- function(...) message(sprintf("EPF: %s", paste0(..., collapse = "")))

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

  wrap_label <- function(x, width = 24) {
    vapply(x, function(label) paste(strwrap(as.character(label), width = width), collapse = "\n"), character(1))
  }

  clean_label <- function(x) {
    x <- as.character(x)
    x <- iconv(x, from = "", to = "UTF-8", sub = "")
    x[is.na(x)] <- NA_character_
    x <- gsub("[[:space:]]+", " ", x)
    trimws(x)
  }

  normalize_names <- function(x) {
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "_", x)
    gsub("(^_+|_+$)", "", x)
  }

  normalize_code <- function(x, width = 2) {
    x <- as.character(x)
    x[is.na(x)] <- NA_character_
    x <- gsub("\\.0+$", "", x)
    x <- trimws(x)
    ifelse(
      is.na(x) | x == "",
      NA_character_,
      stringr::str_pad(x, width = width, side = "left", pad = "0")
    )
  }

  division_labels <- c(
    "01" = "Alimentación y bebidas no alcohólicas",
    "02" = "Bebidas alcohólicas, tabaco y narcóticos",
    "03" = "Vestuario y calzado",
    "04" = "Vivienda, agua, electricidad, gas y otros combustibles",
    "05" = "Muebles y artículos para el hogar",
    "06" = "Salud",
    "07" = "Transporte",
    "08" = "Información y comunicaciones",
    "09" = "Recreación, deporte y cultura",
    "10" = "Educación",
    "11" = "Restaurantes y servicios de alojamiento",
    "12" = "Seguros y servicios financieros",
    "13" = "Cuidado personal, protección social y otros bienes y servicios"
  )

  tenure_labels <- c(
    "Propia pagada",
    "Propia pagándose",
    "Arrendada",
    "Cedida",
    "Ocupación irregular / otras"
  )

  theme_datamigra <- function() {
    theme_minimal(base_family = font_family) +
      theme(
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "#d9e2ec", linewidth = 0.35),
        axis.title = element_text(color = "#102a43", size = 11),
        axis.text = element_text(color = "#243b53", size = 10),
        legend.title = element_blank(),
        legend.text = element_text(color = "#243b53", size = 10),
        legend.position = "top",
        plot.margin = margin(10, 16, 10, 28)
      )
  }

  export_png <- function(plot, path, width = 9, height = 5.8, dpi = 320) {
    ragg::agg_png(path, width = width, height = height, units = "in", res = dpi, scaling = 1)
    print(plot)
    dev.off()
  }

  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }

  estimate_plot_height <- function(labels, base = 4.8, per_item = 0.22, per_extra_line = 0.15, min_height = 5.8, max_height = 11.5) {
    labels <- as.character(labels)
    labels <- labels[!is.na(labels) & nzchar(labels)]
    if (!length(labels)) {
      return(min_height)
    }
    wrapped <- vapply(strsplit(labels, "\n", fixed = TRUE), length, integer(1))
    height <- base + length(labels) * per_item + sum(pmax(wrapped - 1L, 0L)) * per_extra_line
    max(min_height, min(max_height, height))
  }

  public_name <- function(file_stem) {
    names <- c(
      fig_1_composicion_gasto_categoria_2022 = "figura_01_composicion_gasto_categoria_2022",
      fig_6_ratio_gasto_ingreso_2022 = "figura_02_ratio_gasto_ingreso_2022",
      fig_2_brecha_gasto_categoria_2022 = "figura_03_brecha_gasto_categoria_2022",
      fig_4_peso_alimentacion_2022 = "figura_04_peso_alimentacion_2022",
      fig_5_peso_transporte_2022 = "figura_05_peso_transporte_2022",
      fig_7_hogares_sobregasto_2022 = "figura_06_hogares_sobregasto_2022",
      fig_3_peso_vivienda_2022 = "figura_07_peso_vivienda_2022",
      fig_9_tenencia_vivienda_2022 = "figura_08_tenencia_vivienda_2022",
      fig_10_carga_vivienda_tenencia_2022 = "figura_09_carga_vivienda_tenencia_2022",
      fig_8_top_subcategorias_migrantes_2022 = "figura_10_top_subcategorias_migrantes_2022",
      fig_1_composicion_total_hogares_2017_2022 = "figura_11_composicion_total_hogares_2017_2022"
    )
    unname(names[[file_stem]])
  }

  save_table <- function(x, dir_path, file_stem) {
    readr::write_csv(x, file.path(dir_path, paste0(public_name(file_stem), ".csv")))
  }

  load_ccif_levels <- function(path, wave = c("ix", "viii")) {
    wave <- match.arg(wave)
    ccif <- haven::read_dta(path)
    names(ccif) <- normalize_names(names(ccif))
    glosa_source <- if ("glosa_ccif" %in% names(ccif)) ccif$glosa_ccif else if ("glosa" %in% names(ccif)) ccif$glosa else NA_character_
    ccif <- ccif %>%
      mutate(
        d = normalize_code(.data$d),
        g = normalize_code(.data$g),
        c = normalize_code(.data$c),
        sc = normalize_code(.data$sc),
        p = normalize_code(.data$p),
        glosa = clean_label(glosa_source)
      )

    division_map <- tibble(
      d = names(division_labels),
      division = unname(division_labels)
    )

    group_map <- ccif %>%
      filter(!is.na(d), !is.na(g), g != "00", c == "00", sc == "00", p == "00") %>%
      transmute(
        d,
        g,
        group_code = paste0(d, ".", g),
        group_label = glosa
      ) %>%
      distinct()

    list(division_map = division_map, group_map = group_map, raw = ccif)
  }

  recode_tenure <- function(code) {
    code <- suppressWarnings(as.integer(as.character(code)))
    case_when(
      code == 1L ~ "Propia pagada",
      code == 2L ~ "Propia pagándose",
      code %in% c(3L, 4L, 10L) ~ "Arrendada",
      code %in% c(5L, 6L) ~ "Cedida",
      code %in% c(7L, 8L, 9L) ~ "Ocupación irregular / otras",
      TRUE ~ NA_character_
    )
  }

  read_households_ix <- function() {
    path <- file.path(raw_ix, "base-personas-ix-epf-stata.dta")
    if (!file.exists(path)) {
      stop("EPF: falta base-personas-ix-epf-stata.dta en IX EPF.", call. = FALSE)
    }

    persons <- haven::read_dta(path) %>%
      transmute(
        wave = "IX EPF",
        year = 2022L,
        folio = as.character(folio),
        persona = suppressWarnings(as.integer(persona)),
        parentesco = suppressWarnings(as.integer(parentesco)),
        mh12 = suppressWarnings(as.integer(mh12)),
        mh14 = suppressWarnings(as.integer(mh14)),
        mh15 = suppressWarnings(as.integer(mh15)),
        fe = as.numeric(fe),
        n_personas = suppressWarnings(as.integer(npersonas)),
        gasto_hogar = as.numeric(gastot_hd),
        gasto_hogar_pc = as.numeric(gastot_hd_pc),
        ingreso_total_hogar = as.numeric(ing_total_hogar_hd),
        ingreso_disponible_hogar = as.numeric(ing_disp_hog_hd),
        tvp = suppressWarnings(as.integer(tvp)),
        vp = suppressWarnings(as.integer(vp)),
        macrozona = clean_label(as.character(macrozona))
      )

    heads <- persons %>%
      arrange(folio, desc(parentesco == 1L), persona) %>%
      group_by(folio) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        migrant_status = case_when(
          mh12 %in% c(2L, 3L) ~ "Jefatura migrante",
          mh12 == 1L ~ "Jefatura chilena",
          TRUE ~ NA_character_
        ),
        migrant_flag = migrant_status == "Jefatura migrante",
        tenure_group = recode_tenure(tvp)
      )

    members <- persons %>%
      mutate(migrant_member = mh12 %in% c(2L, 3L)) %>%
      group_by(folio) %>%
      summarise(
        household_any_migrant = any(migrant_member, na.rm = TRUE),
        household_migrant_members = sum(migrant_member, na.rm = TRUE),
        .groups = "drop"
      )

    heads %>%
      left_join(members, by = "folio")
  }

  read_households_viii <- function() {
    path <- file.path(raw_viii, "base-personas-viii-epf-(stata).dta")
    if (!file.exists(path)) {
      stop("EPF: falta base-personas-viii-epf-(stata).dta en VIII EPF.", call. = FALSE)
    }

    persons <- haven::read_dta(path) %>%
      transmute(
        wave = "VIII EPF",
        year = 2017L,
        folio = as.character(FOLIO),
        persona = suppressWarnings(as.integer(PERSONA)),
        jhogar = suppressWarnings(as.integer(JHOGAR)),
        parentesco = suppressWarnings(as.integer(PARENTESCO)),
        fe = as.numeric(FE),
        n_personas = suppressWarnings(as.integer(NPERSONAS)),
        gasto_hogar = as.numeric(GASTOT_HD),
        gasto_hogar_pc = as.numeric(GASTOT_HD_PC),
        ingreso_total_hogar = as.numeric(ING_TOTAL_HOG_HD),
        ingreso_disponible_hogar = as.numeric(ING_DISP_HOG_HD),
        tvp = suppressWarnings(as.integer(TVP)),
        vp = suppressWarnings(as.integer(VP)),
        zona = clean_label(as.character(ZONA))
      )

    persons %>%
      arrange(folio, desc(jhogar == 1L), desc(parentesco == 1L), persona) %>%
      group_by(folio) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        migrant_status = NA_character_,
        migrant_flag = NA,
        tenure_group = recode_tenure(tvp),
        household_any_migrant = NA,
        household_migrant_members = NA_integer_
      )
  }

  build_spending_objects <- function(gastos_path, households, ccif_levels, wave = c("ix", "viii")) {
    wave <- match.arg(wave)
    if (!file.exists(gastos_path)) {
      stop("EPF: falta base de gastos para ", wave, ".", call. = FALSE)
    }

    gastos <- haven::read_dta(gastos_path)
    names(gastos) <- normalize_names(names(gastos))

    folio_col <- if ("folio" %in% names(gastos)) "folio" else stop("EPF: no se encontró identificador de hogar en gastos.", call. = FALSE)
    glosa_col <- dplyr::coalesce(if ("glosa_ccif" %in% names(gastos)) "glosa_ccif" else NA_character_, if ("glosa" %in% names(gastos)) "glosa" else NA_character_)

    gastos <- gastos %>%
      transmute(
        folio = as.character(.data[[folio_col]]),
        d = normalize_code(.data$d),
        g = normalize_code(.data$g),
        c = normalize_code(.data$c),
        sc = normalize_code(.data$sc),
        p = normalize_code(.data$p),
        gasto = as.numeric(.data$gasto),
        glosa = clean_label(if (!is.na(glosa_col)) .data[[glosa_col]] else NA_character_)
      ) %>%
      filter(!is.na(folio), !is.na(d), is.finite(gasto))

    household_base <- households %>%
      transmute(
        folio,
        wave,
        year,
        fe,
        gasto_hogar,
        ingreso_disponible_hogar,
        ingreso_total_hogar,
        tenure_group,
        migrant_status
      )

    merged <- gastos %>%
      inner_join(household_base, by = "folio") %>%
      filter(is.finite(gasto_hogar), gasto_hogar > 0, is.finite(fe), fe > 0)

    item_totals <- merged %>%
      group_by(folio) %>%
      summarise(gasto_items_total = sum(gasto, na.rm = TRUE), .groups = "drop")

    division_spend <- merged %>%
      group_by(wave, year, folio, migrant_status, tenure_group, fe, gasto_hogar, ingreso_disponible_hogar, ingreso_total_hogar, d) %>%
      summarise(category_spend = sum(gasto, na.rm = TRUE), .groups = "drop") %>%
      select(folio, d, category_spend)

    household_division <- tidyr::crossing(
      household_base %>%
        filter(is.finite(gasto_hogar), gasto_hogar > 0) %>%
        left_join(item_totals, by = "folio") %>%
        filter(is.finite(gasto_items_total), gasto_items_total > 0),
      d = ccif_levels$division_map$d
    ) %>%
      left_join(division_spend, by = c("folio", "d")) %>%
      mutate(category_spend = dplyr::coalesce(category_spend, 0)) %>%
      left_join(ccif_levels$division_map, by = "d") %>%
      mutate(share = category_spend / gasto_items_total)

    household_group <- merged %>%
      group_by(wave, year, folio, migrant_status, tenure_group, fe, gasto_hogar, d, g) %>%
      summarise(group_spend = sum(gasto, na.rm = TRUE), .groups = "drop") %>%
      left_join(item_totals, by = "folio") %>%
      mutate(group_code = paste0(d, ".", g)) %>%
      left_join(ccif_levels$group_map, by = c("d", "g", "group_code")) %>%
      mutate(share = group_spend / gasto_items_total)

    list(
      household_division = household_division,
      household_group = household_group,
      merged = merged
    )
  }

  summarize_division_shares <- function(household_division, by_migrant = TRUE) {
    if (by_migrant) {
      household_division %>%
        filter(!is.na(migrant_status)) %>%
        group_by(year, migrant_status, d, division) %>%
        summarise(
          share = weighted.mean(share, fe, na.rm = TRUE),
          households = n(),
          .groups = "drop"
        ) %>%
        mutate(share_pct = share * 100)
    } else {
      household_division %>%
        group_by(year, d, division) %>%
        summarise(
          share = weighted.mean(share, fe, na.rm = TRUE),
          households = n(),
          .groups = "drop"
        ) %>%
        mutate(share_pct = share * 100)
    }
  }

  summarize_household_indicators <- function(households_ix) {
    households_ix %>%
      filter(
        !is.na(migrant_status),
        is.finite(fe), fe > 0,
        is.finite(gasto_hogar), gasto_hogar >= 0,
        is.finite(ingreso_disponible_hogar), ingreso_disponible_hogar > 0
      ) %>%
      mutate(
        ratio_gasto_ingreso = gasto_hogar / ingreso_disponible_hogar,
        sobregasto = gasto_hogar > ingreso_disponible_hogar
      ) %>%
      group_by(migrant_status) %>%
      summarise(
        ratio_gasto_ingreso = weighted.mean(gasto_hogar, fe, na.rm = TRUE) / weighted.mean(ingreso_disponible_hogar, fe, na.rm = TRUE),
        share_sobregasto = weighted.mean(sobregasto, fe, na.rm = TRUE),
        hogares = n(),
        .groups = "drop"
      ) %>%
      mutate(
        ratio_gasto_ingreso_pct = ratio_gasto_ingreso * 100,
        share_sobregasto_pct = share_sobregasto * 100
      )
  }

  build_figures <- function(households_ix, households_viii, spending_ix, spending_viii, ccif_ix, crosswalk) {
    division_ix <- summarize_division_shares(spending_ix$household_division, by_migrant = TRUE)
    division_both <- bind_rows(
      summarize_division_shares(spending_viii$household_division, by_migrant = FALSE) %>% mutate(wave = "VIII EPF"),
      summarize_division_shares(spending_ix$household_division, by_migrant = FALSE) %>% mutate(wave = "IX EPF")
    ) %>%
      mutate(wave_label = if_else(year == 2017L, "VIII EPF (2017)", "IX EPF (2022)"))

    top_order <- division_ix %>%
      group_by(d, division) %>%
      summarise(avg_share = mean(share_pct, na.rm = TRUE), .groups = "drop") %>%
      arrange(avg_share) %>%
      pull(division)

    division_plot <- division_ix %>%
      mutate(
        division_plot = factor(division, levels = top_order, labels = wrap_label(top_order, 26))
      )

    fig1_table <- division_ix %>%
      select(migrant_status, d, division, share_pct, households) %>%
      arrange(match(division, top_order), migrant_status)

    fig1 <- ggplot(division_plot, aes(x = share_pct, y = division_plot, fill = migrant_status)) +
      geom_col(position = position_dodge(width = 0.72), width = 0.64) +
      scale_fill_manual(values = c("Jefatura chilena" = "#123b5d", "Jefatura migrante" = "#d47c47")) +
      scale_x_continuous(labels = label_number(suffix = "%", accuracy = 0.1), expand = expansion(mult = c(0, 0.03))) +
      labs(x = "Porcentaje del presupuesto del hogar", y = NULL) +
      theme_datamigra()

    fig2_table <- division_ix %>%
      select(migrant_status, d, division, share_pct) %>%
      tidyr::pivot_wider(names_from = migrant_status, values_from = share_pct) %>%
      mutate(
        brecha_pp = `Jefatura migrante` - `Jefatura chilena`
      ) %>%
      arrange(brecha_pp) %>%
      mutate(division_plot = factor(division, levels = division, labels = wrap_label(division, 26)))

    fig2 <- ggplot(fig2_table, aes(x = brecha_pp, y = division_plot, fill = brecha_pp > 0)) +
      geom_col(width = 0.64, show.legend = FALSE) +
      geom_vline(xintercept = 0, color = "#486581", linewidth = 0.45) +
      scale_fill_manual(values = c("TRUE" = "#d47c47", "FALSE" = "#123b5d")) +
      scale_x_continuous(labels = label_number(suffix = " pp", accuracy = 0.1)) +
      labs(x = "Brecha jefatura migrante menos jefatura chilena", y = NULL) +
      theme_datamigra()

    metric_categories <- c(
      "04" = "Peso del gasto en vivienda",
      "01" = "Peso del gasto en alimentación",
      "07" = "Peso del gasto en transporte"
    )

    single_metric_plot <- function(code, fill_color = "#d47c47") {
      tbl <- division_ix %>%
        filter(d == code) %>%
        mutate(migrant_status = factor(migrant_status, levels = c("Jefatura chilena", "Jefatura migrante")))

      plot <- ggplot(tbl, aes(x = migrant_status, y = share_pct, fill = migrant_status)) +
        geom_col(width = 0.62, show.legend = FALSE) +
        geom_text(aes(label = sprintf("%.1f%%", share_pct)), vjust = -0.35, family = font_family, size = 3.4, color = "#243b53") +
        scale_fill_manual(values = c("Jefatura chilena" = "#123b5d", "Jefatura migrante" = "#d47c47")) +
        scale_y_continuous(labels = label_number(suffix = "%", accuracy = 0.1), expand = expansion(mult = c(0, 0.12))) +
        labs(x = NULL, y = "Porcentaje del presupuesto del hogar") +
        theme_datamigra()

      list(table = tbl, plot = plot)
    }

    fig3_obj <- single_metric_plot("04")
    fig4_obj <- single_metric_plot("01")
    fig5_obj <- single_metric_plot("07")

    indicator_table <- summarize_household_indicators(households_ix) %>%
      mutate(migrant_status = factor(migrant_status, levels = c("Jefatura chilena", "Jefatura migrante")))

    fig6 <- ggplot(indicator_table, aes(x = migrant_status, y = ratio_gasto_ingreso_pct, fill = migrant_status)) +
      geom_col(width = 0.62, show.legend = FALSE) +
      geom_text(aes(label = sprintf("%.1f%%", ratio_gasto_ingreso_pct)), vjust = -0.35, family = font_family, size = 3.4, color = "#243b53") +
      scale_fill_manual(values = c("Jefatura chilena" = "#123b5d", "Jefatura migrante" = "#d47c47")) +
      scale_y_continuous(labels = label_number(suffix = "%", accuracy = 1), expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = "Gasto total / ingreso disponible del hogar") +
      theme_datamigra()

    fig7 <- ggplot(indicator_table, aes(x = migrant_status, y = share_sobregasto_pct, fill = migrant_status)) +
      geom_col(width = 0.62, show.legend = FALSE) +
      geom_text(aes(label = sprintf("%.1f%%", share_sobregasto_pct)), vjust = -0.35, family = font_family, size = 3.4, color = "#243b53") +
      scale_fill_manual(values = c("Jefatura chilena" = "#123b5d", "Jefatura migrante" = "#d47c47")) +
      scale_y_continuous(labels = label_number(suffix = "%", accuracy = 0.1), expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = "Hogares con gasto mayor al ingreso disponible") +
      theme_datamigra()

    total_budget_migrant <- households_ix %>%
      filter(!is.na(migrant_status), migrant_status == "Jefatura migrante", is.finite(fe), fe > 0, is.finite(gasto_hogar), gasto_hogar > 0) %>%
      summarise(total_budget = sum(gasto_hogar * fe, na.rm = TRUE)) %>%
      pull(total_budget)

    fig8_table <- spending_ix$merged %>%
      filter(migrant_status == "Jefatura migrante") %>%
      mutate(group_code = paste0(d, ".", g)) %>%
      left_join(
        spending_ix$household_group %>%
          select(d, g, group_code, group_label) %>%
          distinct(),
        by = c("d", "g", "group_code")
      ) %>%
      filter(!is.na(group_label)) %>%
      group_by(d, g, group_code, group_label) %>%
      summarise(
        weighted_spend = sum(gasto * fe, na.rm = TRUE),
        households = n_distinct(folio),
        .groups = "drop"
      ) %>%
      mutate(share_pct = weighted_spend / total_budget_migrant * 100) %>%
      arrange(desc(share_pct)) %>%
      slice_head(n = 12) %>%
      arrange(share_pct) %>%
      mutate(group_plot = factor(group_label, levels = group_label, labels = wrap_label(group_label, 28)))

    fig8 <- ggplot(fig8_table, aes(x = share_pct, y = group_plot)) +
      geom_col(fill = "#d47c47", width = 0.62) +
      scale_x_continuous(labels = label_number(suffix = "%", accuracy = 0.1), expand = expansion(mult = c(0, 0.03))) +
      labs(x = "Porcentaje del presupuesto del hogar", y = NULL) +
      theme_datamigra()

    fig9_table <- households_ix %>%
      filter(!is.na(migrant_status), !is.na(tenure_group), is.finite(fe), fe > 0) %>%
      group_by(migrant_status, tenure_group) %>%
      summarise(weighted_hh = sum(fe, na.rm = TRUE), .groups = "drop_last") %>%
      mutate(share = weighted_hh / sum(weighted_hh) * 100) %>%
      ungroup() %>%
      mutate(
        migrant_status = factor(migrant_status, levels = c("Jefatura chilena", "Jefatura migrante")),
        tenure_group = factor(tenure_group, levels = tenure_labels)
      )

    fig9 <- ggplot(fig9_table, aes(x = migrant_status, y = share, fill = tenure_group)) +
      geom_col(width = 0.62, position = "fill") +
      scale_y_continuous(labels = label_percent(accuracy = 1)) +
      scale_fill_manual(values = c("#123b5d", "#1f6f8b", "#d47c47", "#94b0c2", "#7b8794")) +
      labs(x = NULL, y = "Distribución porcentual") +
      theme_datamigra()

    fig10_table <- spending_ix$household_division %>%
      filter(d == "04", !is.na(migrant_status), !is.na(tenure_group), is.finite(share)) %>%
      group_by(migrant_status, tenure_group) %>%
      summarise(
        share_pct = weighted.mean(share, fe, na.rm = TRUE) * 100,
        households = n(),
        .groups = "drop"
      ) %>%
      mutate(
        migrant_status = factor(migrant_status, levels = c("Jefatura chilena", "Jefatura migrante")),
        tenure_group = factor(tenure_group, levels = tenure_labels)
      )

    fig10 <- ggplot(fig10_table, aes(x = share_pct, y = tenure_group, fill = migrant_status)) +
      geom_col(position = position_dodge(width = 0.72), width = 0.64) +
      scale_fill_manual(values = c("Jefatura chilena" = "#123b5d", "Jefatura migrante" = "#d47c47")) +
      scale_x_continuous(labels = label_number(suffix = "%", accuracy = 0.1), expand = expansion(mult = c(0, 0.03))) +
      labs(x = "Porcentaje del presupuesto del hogar", y = NULL) +
      theme_datamigra()

    compare_order <- division_both %>%
      group_by(d, division) %>%
      summarise(avg_share = mean(share_pct, na.rm = TRUE), .groups = "drop") %>%
      arrange(avg_share) %>%
      pull(division)

    fig_compare_table <- division_both %>%
      mutate(division_plot = factor(division, levels = compare_order, labels = wrap_label(compare_order, 26)))

    fig_compare_1 <- ggplot(fig_compare_table, aes(x = share_pct, y = division_plot, fill = wave_label)) +
      geom_col(position = position_dodge(width = 0.72), width = 0.64) +
      scale_fill_manual(values = c("VIII EPF (2017)" = "#94b0c2", "IX EPF (2022)" = "#123b5d")) +
      scale_x_continuous(labels = label_number(suffix = "%", accuracy = 0.1), expand = expansion(mult = c(0, 0.03))) +
      labs(x = "Porcentaje del presupuesto del hogar", y = NULL) +
      theme_datamigra()

    figure_objects <- list(
      division_ix = division_ix,
      division_both = division_both,
      indicator_table = indicator_table,
      top_groups_migrant = fig8_table,
      tenure_distribution = fig9_table,
      housing_tenure = fig10_table,
      crosswalk = crosswalk
    )

    list(
      objects = figure_objects,
      fig1 = list(
        plot = fig1,
        table = fig1_table,
        name = "fig_1_composicion_gasto_categoria_2022",
        width = 9.4,
        height = estimate_plot_height(levels(division_plot$division_plot), base = 5.2, per_item = 0.3, per_extra_line = 0.22, min_height = 8.8, max_height = 11.4)
      ),
      fig2 = list(
        plot = fig2,
        table = fig2_table,
        name = "fig_2_brecha_gasto_categoria_2022",
        width = 9.4,
        height = estimate_plot_height(levels(fig2_table$division_plot), base = 4.6, per_item = 0.24, per_extra_line = 0.18, min_height = 7.6, max_height = 10.2)
      ),
      fig3 = list(plot = fig3_obj$plot, table = fig3_obj$table, name = "fig_3_peso_vivienda_2022", width = 7.2, height = 5.8),
      fig4 = list(plot = fig4_obj$plot, table = fig4_obj$table, name = "fig_4_peso_alimentacion_2022", width = 7.2, height = 5.8),
      fig5 = list(plot = fig5_obj$plot, table = fig5_obj$table, name = "fig_5_peso_transporte_2022", width = 7.2, height = 5.8),
      fig6 = list(plot = fig6, table = indicator_table %>% select(migrant_status, ratio_gasto_ingreso, ratio_gasto_ingreso_pct, hogares), name = "fig_6_ratio_gasto_ingreso_2022", width = 7.2, height = 5.8),
      fig7 = list(plot = fig7, table = indicator_table %>% select(migrant_status, share_sobregasto, share_sobregasto_pct, hogares), name = "fig_7_hogares_sobregasto_2022", width = 7.2, height = 5.8),
      fig8 = list(
        plot = fig8,
        table = fig8_table,
        name = "fig_8_top_subcategorias_migrantes_2022",
        width = 9.6,
        height = estimate_plot_height(levels(fig8_table$group_plot), base = 4.8, per_item = 0.3, per_extra_line = 0.22, min_height = 8.8, max_height = 11.4)
      ),
      fig9 = list(plot = fig9, table = fig9_table, name = "fig_9_tenencia_vivienda_2022", width = 8.2, height = 6.2),
      fig10 = list(
        plot = fig10,
        table = fig10_table,
        name = "fig_10_carga_vivienda_tenencia_2022",
        width = 9.2,
        height = estimate_plot_height(levels(fig10_table$tenure_group), base = 4.6, per_item = 0.32, per_extra_line = 0.16, min_height = 6.4, max_height = 8.2)
      ),
      compare1 = list(
        plot = fig_compare_1,
        table = fig_compare_table,
        name = "fig_1_composicion_total_hogares_2017_2022",
        width = 9.4,
        height = estimate_plot_height(levels(fig_compare_table$division_plot), base = 5.2, per_item = 0.3, per_extra_line = 0.22, min_height = 8.8, max_height = 11.4)
      )
    )
  }

  build_data <- function() {
    log_msg("procesando IX y VIII EPF desde raw")

    ccif_ix <- load_ccif_levels(file.path(raw_ix, "ccif-ix-epf-stata.dta"), wave = "ix")
    ccif_viii <- load_ccif_levels(file.path(raw_viii, "ccif-viii-epf-(stata).dta"), wave = "viii")
    crosswalk <- readxl::read_excel(file.path(raw_ix, "tabla-de-correspondencia-de-ccif-viii---ix-epf.xlsx"), sheet = "corresp_ix_viii")

    households_ix <- read_households_ix()
    households_viii <- read_households_viii()

    spending_ix <- build_spending_objects(
      gastos_path = file.path(raw_ix, "base-gastos-ix-epf-stata.dta"),
      households = households_ix,
      ccif_levels = ccif_ix,
      wave = "ix"
    )

    spending_viii <- build_spending_objects(
      gastos_path = file.path(raw_viii, "base-gastos-viii-epf-(stata).dta"),
      households = households_viii,
      ccif_levels = ccif_viii,
      wave = "viii"
    )

    wave_inventory <- tibble(
      wave = c("VIII EPF", "IX EPF"),
      year = c(2017L, 2022L),
      persons_file = c("base-personas-viii-epf-(stata).dta", "base-personas-ix-epf-stata.dta"),
      gastos_file = c("base-gastos-viii-epf-(stata).dta", "base-gastos-ix-epf-stata.dta"),
      ccif_file = c("ccif-viii-epf-(stata).dta", "ccif-ix-epf-stata.dta"),
      migration_status_available = c(FALSE, TRUE),
      migration_strategy = c(NA_character_, "mh12 en jefatura de hogar"),
      notes = c(
        "La VIII queda preparada para armonización y exploración. No se publica aún comparación migratoria.",
        "Base principal para la sección web EPF."
      )
    )

    readr::write_csv(wave_inventory, file.path(interim_dir, "epf_wave_inventory.csv"))
    readr::write_csv(crosswalk, file.path(interim_dir, "epf_ccif_viii_ix_crosswalk.csv"))
    saveRDS(households_ix, file.path(interim_dir, "epf_ix_households.rds"))
    saveRDS(households_viii, file.path(interim_dir, "epf_viii_households.rds"))
    saveRDS(spending_ix$household_division, file.path(interim_dir, "epf_ix_household_division.rds"))
    saveRDS(spending_ix$household_group, file.path(interim_dir, "epf_ix_household_group.rds"))
    saveRDS(spending_viii$household_division, file.path(interim_dir, "epf_viii_household_division.rds"))
    saveRDS(list(ix = ccif_ix$division_map, viii = ccif_viii$division_map), file.path(interim_dir, "epf_division_maps.rds"))

    panel_final <- list(
      households_ix = households_ix,
      households_viii = households_viii,
      spending_ix = spending_ix,
      spending_viii = spending_viii,
      wave_inventory = wave_inventory,
      crosswalk = crosswalk
    )

    saveRDS(panel_final, final_household)

    summary_objects <- list(
      wave_inventory = wave_inventory,
      ix_households = households_ix %>% count(migrant_status, name = "n_hogares"),
      viii_households = tibble(n_hogares = nrow(households_viii)),
      ix_divisions = summarize_division_shares(spending_ix$household_division, by_migrant = TRUE),
      both_waves = bind_rows(
        summarize_division_shares(spending_viii$household_division, by_migrant = FALSE) %>% mutate(wave = "VIII EPF"),
        summarize_division_shares(spending_ix$household_division, by_migrant = FALSE) %>% mutate(wave = "IX EPF")
      )
    )
    saveRDS(summary_objects, final_summary)

    invisible(panel_final)
  }

  if (build %in% c("all", "data")) {
    if (!dir.exists(raw_ix) || !dir.exists(raw_viii)) {
      stop("EPF: faltan carpetas `IX EPF` o `VIII EPF` en `data/raw/epf/`.", call. = FALSE)
    }
    if (force_rebuild || !file.exists(final_household)) {
      build_data()
    }
  }

  if (build %in% c("all", "figures")) {
    if (!file.exists(final_household)) {
      if (force_rebuild) {
        build_data()
      } else {
        stop("EPF: falta `data/final/epf/epf_household_panel.rds`. Corre primero `build='data'` o usa `build='all'`.", call. = FALSE)
      }
    }

    panel_final <- readRDS(final_household)
    figure_bundle <- build_figures(
      households_ix = panel_final$households_ix,
      households_viii = panel_final$households_viii,
      spending_ix = panel_final$spending_ix,
      spending_viii = panel_final$spending_viii,
      ccif_ix = NULL,
      crosswalk = panel_final$crosswalk
    )
    saveRDS(figure_bundle$objects, final_figures)

    snapshot_items <- figure_bundle[c("fig1", "fig2", "fig3", "fig4", "fig5", "fig6", "fig7", "fig8", "fig9", "fig10")]
    compare_items <- figure_bundle[c("compare1")]

    purrr::walk(snapshot_items, function(item) {
      export_png(
        item$plot,
        file.path(fig_snapshot, paste0(public_name(item$name), ".png")),
        width = item$width %||% 9,
        height = item$height %||% 5.8
      )
      save_table(item$table, tab_snapshot, item$name)
    })

    purrr::walk(compare_items, function(item) {
      export_png(
        item$plot,
        file.path(fig_compare, paste0(public_name(item$name), ".png")),
        width = item$width %||% 9,
        height = item$height %||% 5.8
      )
      save_table(item$table, tab_compare, item$name)
    })

    log_msg("figuras exportadas en figures/epf")
  }

  invisible(list(build = build, force_rebuild = force_rebuild))
}

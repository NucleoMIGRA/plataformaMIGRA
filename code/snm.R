# SNM
# Fuente de datos: https://serviciomigraciones.cl/estudios-migratorios/datos-abiertos/
# Unidad de observación: trámite migratorio agregado por año y características del solicitante.
# Espera insumos en `data/raw/snm/` y construye datos procesados y productos públicos.
# Aunque la carpeta publica no distribuye esas bases, aqui queda documentada la logica general del flujo:
# 1. lectura de datos originales;
# 2. procesamiento hacia `data/interim/`;
# 3. construccion de paneles en `data/final/`;
# 4. generacion de tablas espejo;
# 5. generacion de figuras publicables;
# 6. exportación hacia `figures/snm/` y `tables/snm/`.

run_snm <- function(build = c("all", "data", "figures"), force_rebuild = FALSE) {
  build <- match.arg(build)

  required_packages <- c(
    "here", "readxl", "readr", "dplyr", "tidyr", "ggplot2", "purrr", "forcats", "scales", "ragg"
  )
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(
      "SNM: faltan paquetes requeridos: ",
      paste(missing_packages, collapse = ", "),
      ". Instala antes de correr el pipeline.",
      call. = FALSE
    )
  }

  suppressPackageStartupMessages({
    library(here)
    library(readxl)
    library(readr)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(purrr)
    library(forcats)
    library(scales)
    library(ragg)
  })

  for (loc in c("en_US.UTF-8", "es_CL.UTF-8", "C.UTF-8")) {
    ok <- tryCatch(Sys.setlocale("LC_CTYPE", loc), warning = function(w) NA_character_, error = function(e) NA_character_)
    if (!is.na(ok)) break
  }

  raw_snm <- here::here("data", "raw", "snm")
  raw_res <- file.path(raw_snm, "residencias")
  raw_ref <- file.path(raw_snm, "refugio")

  interim_dir <- here::here("data", "interim", "snm")
  final_dir <- here::here("data", "final", "snm")
  fig_dir <- here::here("figures", "snm")
  tab_dir <- here::here("tables", "snm")

  dir.create(interim_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

  residencias_interim <- file.path(interim_dir, "snm_residencias_clean.rds")
  refugio_interim <- file.path(interim_dir, "snm_refugio_clean.rds")
  residencias_final <- file.path(final_dir, "snm_residencias_panel.rds")
  refugio_final <- file.path(final_dir, "snm_refugio_panel.rds")

  clean_names_ascii <- function(x) {
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    x[is.na(x)] <- ""
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("(^_+|_+$)", "", x)
    x
  }

  clean_label <- function(x) {
    x <- enc2utf8(as.character(x))
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

  parse_total_num <- function(x) {
    readr::parse_number(as.character(x), locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
  }

  standardize_residencias_names <- function(df) {
    nms <- clean_names_ascii(names(df))
    nms[nms == "pa_is"] <- "pais"
    nms[nms == "regi_on"] <- "region"
    nms[nms == "a_no"] <- "ano"
    names(df) <- nms
    df
  }

  standardize_refugio_names <- function(df) {
    nms <- clean_names_ascii(names(df))
    nms[nms == "pa_is_de_nacionalidad"] <- "pais_nacionalidad"
    nms[nms == "a_no"] <- "ano"
    names(df) <- nms
    df
  }

  normalize_tipo_resuelto <- function(x) {
    key <- key_ascii(x)
    dplyr::case_when(
      is.na(key) ~ NA_character_,
      grepl("otorga", key) ~ "Otorga",
      grepl("abandono", key) ~ "Rechaza con abandono",
      grepl("con rt", key) ~ "Rechaza con RT",
      grepl("archiv", key) ~ "Archiva",
      grepl("rechaz", key) ~ "Rechaza",
      TRUE ~ clean_label(x)
    )
  }

  normalize_origen <- function(x) {
    key <- key_ascii(x)
    dplyr::case_when(
      is.na(key) ~ NA_character_,
      grepl("dentro", key) ~ "Dentro de Chile",
      grepl("fuera", key) ~ "Fuera de Chile",
      TRUE ~ clean_label(x)
    )
  }

  normalize_activity <- function(x) {
    key <- key_ascii(x)
    dplyr::case_when(
      is.na(key) ~ NA_character_,
      grepl("emplead", key) ~ "Empleado",
      grepl("estudiant", key) ~ "Estudiante",
      grepl("cuenta propia", key) ~ "Trabajador por cuenta propia",
      grepl("empresari|patron", key) ~ "Empresario/a o patrón/a",
      grepl("tripul", key) ~ "Tripulante",
      grepl("no informa|sin informacion|no inform", key) ~ "No informa",
      grepl("otra", key) ~ "Otras actividades",
      TRUE ~ clean_label(x)
    )
  }

  normalize_country <- function(x) {
    key <- key_ascii(x)
    out <- clean_label(x)
    out <- dplyr::case_when(
      is.na(key) ~ NA_character_,
      grepl("otros paises dentro de los 25 primeros", key) ~ "Otros países dentro de los 25 primeros",
      grepl("otros paises", key) ~ "Otros países",
      grepl("republica dominicana", key) ~ "República Dominicana",
      grepl("espana", key) ~ "España",
      grepl("haiti", key) ~ "Haití",
      grepl("peru", key) ~ "Perú",
      TRUE ~ out
    )
    out
  }

  normalize_region <- function(x) {
    out <- clean_label(x)
    key <- key_ascii(x)
    dplyr::case_when(
      is.na(key) ~ NA_character_,
      grepl("metropolitana", key) ~ "Metropolitana",
      grepl("o'higgins|bernardo", key) ~ "Libertador General Bernardo O'Higgins",
      TRUE ~ out
    )
  }

  normalize_benefit <- function(x) {
    key <- key_ascii(x)
    out <- clean_label(x)
    dplyr::case_when(
      is.na(key) ~ NA_character_,
      grepl("ninos|ninas|adolescentes", key) ~ "Humanitaria para niños, niñas y adolescentes",
      grepl("reunific", key) ~ "Reunificación familiar",
      grepl("actividades licitas", key) ~ "Actividades lícitas remuneradas",
      grepl("otras residencias temporales", key) ~ "Otras residencias temporales",
      grepl("otras humanitarias", key) ~ "Otras humanitarias",
      TRUE ~ out
    )
  }

  label_big_n <- scales::label_number(big.mark = ".", decimal.mark = ",")
  label_pct <- scales::label_percent(accuracy = 1, decimal.mark = ",")
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
  region_display_map <- c(
    "Arica y Parinacota" = "Arica y Parinacota",
    "Tarapacá" = "Tarapacá",
    "Antofagasta" = "Antofagasta",
    "Atacama" = "Atacama",
    "Coquimbo" = "Coquimbo",
    "Valparaíso" = "Valparaíso",
    "Metropolitana" = "Metropolitana",
    "O'Higgins" = "O'Higgins",
    "Maule" = "Maule",
    "Ñuble" = "Ñuble",
    "Biobío" = "Biobío",
    "La Araucanía" = "La Araucanía",
    "Los Ríos" = "Los Ríos",
    "Los Lagos" = "Los Lagos",
    "Aysén" = "Aysén",
    "Magallanes" = "Magallanes"
  )

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
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#d9e2ec", linewidth = 0.35),
        strip.text = element_text(colour = "#243b53"),
        plot.margin = margin(10, 16, 10, 10),
        legend.position = "top"
      )
  }

  theme_series_x <- function() {
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }

  public_name <- function(name) {
    names <- c(
      fig_1_rt_solicitudes = "figura_01_rt_solicitudes", fig_2_rt_aprobadas = "figura_02_rt_aprobadas",
      fig_3_rt_resueltas_resultado = "figura_03_rt_resueltas_resultado", fig_8_origen_solicitud_rt = "figura_04_origen_solicitud_rt",
      fig_9_rt_beneficios_2025 = "figura_05_rt_beneficios_2025", fig_10_rt_nacionalidades_2025 = "figura_06_rt_nacionalidades_2025",
      fig_12_rt_actividad_distribucion = "figura_07_rt_actividad_distribucion", fig_4_rd_solicitudes = "figura_08_rd_solicitudes",
      fig_5_rd_aprobadas = "figura_09_rd_aprobadas", fig_6_rd_resueltas_resultado = "figura_10_rd_resueltas_resultado",
      fig_11_rd_actividad_distribucion = "figura_11_rd_actividad_distribucion", fig_13_rd_regiones_2025 = "figura_12_rd_regiones_2025",
      fig_16_sexo_residencias_favorables_2025 = "figura_13_sexo_residencias_favorables_2025", fig_7_tasa_aprobacion = "figura_14_tasa_aprobacion",
      fig_14_refugio_series = "figura_15_refugio_series", fig_15_refugio_nacionalidades_2025 = "figura_16_refugio_nacionalidades_2025"
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
    ggplot2::ggsave(
      filename = file.path(fig_dir, paste0(public_name(name), ".png")),
      plot = plot_clean,
      device = ragg::agg_png,
      width = width,
      height = height,
      units = "in",
      dpi = 320,
      bg = "white"
    )
  }

  save_table <- function(df, name) {
    readr::write_csv(df, file.path(tab_dir, paste0(public_name(name), ".csv")))
  }

  reorder_with_other_last <- function(df, category, value, other_pattern = "^(Otros|Resto|No informa|Sin informaci[oó]n|Ignorado|No especificado)") {
    category <- rlang::ensym(category)
    value <- rlang::ensym(value)
    out <- df
    vals <- as.character(dplyr::pull(out, !!category))
    is_other <- grepl(other_pattern, vals)
    rank_non_other <- dplyr::pull(out, !!value)
    rank_non_other[is_other] <- -Inf
    ord <- order(rank_non_other, decreasing = TRUE)
    ordered_levels <- vals[ord]
    ordered_levels <- c(setdiff(ordered_levels, vals[is_other]), vals[is_other])
    out[[rlang::as_name(category)]] <- factor(vals, levels = rev(unique(ordered_levels)))
    out
  }

  collapse_top_categories <- function(df, category, value, n_top = 8, other_label = "Otros", keep_regex = NULL) {
    category <- rlang::ensym(category)
    value <- rlang::ensym(value)

    base_totals <- df %>%
      group_by(!!category) %>%
      summarise(total_tmp = sum(!!value, na.rm = TRUE), .groups = "drop") %>%
      mutate(cat_chr = as.character(!!category))

    keep_levels <- base_totals %>%
      filter(!grepl("^Otros", cat_chr)) %>%
      arrange(desc(total_tmp)) %>%
      slice_head(n = n_top) %>%
      pull(cat_chr)

    if (!is.null(keep_regex)) {
      keep_levels <- unique(c(keep_levels, base_totals$cat_chr[grepl(keep_regex, base_totals$cat_chr)]))
    }

    df %>%
      mutate(
        category_group = dplyr::case_when(
          is.na(!!category) ~ NA_character_,
          as.character(!!category) %in% keep_levels ~ as.character(!!category),
          grepl("^Otros", as.character(!!category)) ~ other_label,
          TRUE ~ other_label
        )
      ) %>%
      group_by(across(-c(!!category, !!value)), category_group) %>%
      summarise(total = sum(!!value, na.rm = TRUE), .groups = "drop")
  }

  activity_distribution <- function(df, top_n = 4) {
    top_levels <- df %>%
      filter(!actividad %in% c("Otras actividades")) %>%
      group_by(actividad) %>%
      summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      slice_head(n = top_n) %>%
      pull(actividad)

    df %>%
      mutate(
        actividad_grp = dplyr::case_when(
          actividad %in% top_levels ~ actividad,
          TRUE ~ "Otras actividades"
        )
      ) %>%
      group_by(ano, actividad_grp) %>%
      summarise(total = sum(total, na.rm = TRUE), .groups = "drop_last") %>%
      mutate(share = total / sum(total)) %>%
      ungroup()
  }

  if (build %in% c("all", "data") || force_rebuild || !file.exists(residencias_interim) || !file.exists(refugio_interim)) {
    message("SNM: limpiando residencias y refugio desde raw/.")

    res_files <- list.files(raw_res, full.names = TRUE, pattern = "xlsx$")
    ref_files <- list.files(raw_ref, full.names = TRUE, pattern = "xlsx$")

    if (length(res_files) == 0) stop("SNM: no hay archivos en data/raw/snm/residencias.", call. = FALSE)
    if (length(ref_files) == 0) stop("SNM: no hay archivos en data/raw/snm/refugio.", call. = FALSE)

    read_residencia <- function(path) {
      raw <- readxl::read_excel(path)
      raw <- standardize_residencias_names(raw)

      required <- c("sexo", "rango_etario", "pais", "actividad", "estudios", "region", "ano", "total")
      for (nm in required) if (!nm %in% names(raw)) raw[[nm]] <- NA
      if (!"tipo_resuelto" %in% names(raw)) raw$tipo_resuelto <- NA
      if (!"beneficio_agrupado" %in% names(raw)) raw$beneficio_agrupado <- NA
      if (!"origen_solicitud" %in% names(raw)) raw$origen_solicitud <- NA

      filename <- basename(path)
      tipo_residencia <- if (grepl("^RT", filename)) "Temporal" else "Definitiva"
      etapa <- if (grepl("Acogidas", filename, ignore.case = TRUE)) "Acogidas" else "Resueltas"

      raw %>%
        transmute(
          archivo = filename,
          tipo_residencia = tipo_residencia,
          etapa = etapa,
          sexo = clean_label(sexo),
          rango_etario = clean_label(rango_etario),
          pais = normalize_country(pais),
          actividad = normalize_activity(actividad),
          estudios = clean_label(estudios),
          region = normalize_region(region),
          beneficio_agrupado = normalize_benefit(beneficio_agrupado),
          origen_solicitud = normalize_origen(origen_solicitud),
          tipo_resuelto = normalize_tipo_resuelto(tipo_resuelto),
          ano = suppressWarnings(as.integer(ano)),
          total = parse_total_num(total)
        ) %>%
        filter(!is.na(ano), !is.na(total), total > 0)
    }

    read_refugio <- function(path) {
      raw <- readxl::read_excel(path)
      raw <- standardize_refugio_names(raw)
      for (nm in c("sexo", "pais_nacionalidad", "rango_etario", "ano")) {
        if (!nm %in% names(raw)) raw[[nm]] <- NA
      }

      evento <- dplyr::case_when(
        grepl("Solicit", basename(path), ignore.case = TRUE) ~ "Solicitantes",
        grepl("Reconoc", basename(path), ignore.case = TRUE) ~ "Reconocimientos",
        grepl("Rechaz", basename(path), ignore.case = TRUE) ~ "Rechazos",
        TRUE ~ "Otro"
      )

      raw %>%
        transmute(
          archivo = basename(path),
          evento = evento,
          sexo = clean_label(sexo),
          pais = normalize_country(pais_nacionalidad),
          rango_etario = clean_label(rango_etario),
          ano = suppressWarnings(as.integer(ano)),
          total = 1
        ) %>%
        filter(!is.na(ano))
    }

    residencias <- purrr::map_dfr(res_files, read_residencia)
    refugio <- purrr::map_dfr(ref_files, read_refugio)

    readr::write_rds(residencias, residencias_interim)
    readr::write_rds(refugio, refugio_interim)
    readr::write_rds(residencias, residencias_final)
    readr::write_rds(refugio, refugio_final)
  }

  residencias <- readr::read_rds(residencias_interim)
  refugio <- readr::read_rds(refugio_interim)

  if (build %in% c("all", "figures")) {
    message("SNM: generando figuras.")

    unlink(Sys.glob(file.path(fig_dir, "figura_*.png")), force = TRUE)
    unlink(Sys.glob(file.path(tab_dir, "figura_*.csv")), force = TRUE)

    colors_tipo <- c("Temporal" = "#2F6C8F", "Definitiva" = "#C26D4B")
    colors_resultado <- c(
      "Otorga" = "#2F6C8F",
      "Archiva" = "#A0A7B4",
      "Rechaza" = "#C45850",
      "Rechaza con RT" = "#D9895B",
      "Rechaza con abandono" = "#8A5C7A"
    )
    colors_origin <- c("Dentro de Chile" = "#2F6C8F", "Fuera de Chile" = "#C26D4B")
    colors_ref <- c("Solicitantes" = "#2F6C8F", "Reconocimientos" = "#5A8F3D", "Rechazos" = "#C45850")
    colors_activity <- c(
      "Empleado" = "#E84A5F",
      "Estudiante" = "#7A5195",
      "No informa" = "#BC5090",
      "Trabajador por cuenta propia" = "#FFA600",
      "Empresario/a o patrón/a" = "#5BC0DE",
      "Otras actividades" = "#6C757D"
    )
    colors_sex <- c("Hombre" = "#2F6C8F", "Mujer" = "#D9895B")
    region_order <- c(
      "Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo",
      "Valparaíso", "Metropolitana", "O'Higgins", "Maule", "Ñuble", "Biobío",
      "La Araucanía", "Los Ríos", "Los Lagos", "Aysén", "Magallanes"
    )

    years_all <- sort(unique(residencias$ano))
    latest_res_year <- max(years_all, na.rm = TRUE)
    latest_ref_year <- max(refugio$ano, na.rm = TRUE)

    fig1 <- residencias %>%
      filter(tipo_residencia == "Temporal", etapa == "Acogidas") %>%
      count(ano, wt = total, name = "total")
    p1 <- ggplot(fig1, aes(ano, total)) +
      geom_line(linewidth = 1.2, colour = colors_tipo[["Temporal"]]) +
      geom_point(size = 2.6, colour = colors_tipo[["Temporal"]]) +
      scale_x_continuous(breaks = fig1$ano) +
      scale_y_continuous(labels = label_big_n) +
      labs(
        title = "Figura 1. Evolución del número de solicitudes totales de residencias temporales",
        subtitle = "Serie construida con solicitudes acogidas por el SNM",
        x = "Año", y = "Solicitudes", caption = "Fuente: elaboración propia con datos abiertos del Servicio Nacional de Migraciones."
      ) +
      theme_datamigra() +
      theme_series_x()
    save_plot(p1, "fig_1_rt_solicitudes")
    save_table(fig1, "fig_1_rt_solicitudes")

    fig2 <- residencias %>%
      filter(tipo_residencia == "Temporal", etapa == "Resueltas", tipo_resuelto == "Otorga") %>%
      count(ano, wt = total, name = "total")
    p2 <- ggplot(fig2, aes(ano, total)) +
      geom_line(linewidth = 1.2, colour = colors_tipo[["Temporal"]]) +
      geom_point(size = 2.6, colour = colors_tipo[["Temporal"]]) +
      scale_x_continuous(breaks = fig2$ano) +
      scale_y_continuous(labels = label_big_n) +
      labs(
        title = "Figura 2. Evolución del número de solicitudes
de residencias temporales aprobadas (acogidas)",
        subtitle = "Serie operacional construida con resoluciones favorables (Otorga)",
        x = "Año", y = "Aprobaciones", caption = "Fuente: elaboración propia con datos abiertos del Servicio Nacional de Migraciones."
      ) +
      theme_datamigra() +
      theme_series_x()
    save_plot(p2, "fig_2_rt_aprobadas")
    save_table(fig2, "fig_2_rt_aprobadas")

    fig3 <- residencias %>%
      filter(tipo_residencia == "Temporal", etapa == "Resueltas", !is.na(tipo_resuelto)) %>%
      count(ano, tipo_resuelto, wt = total, name = "total")
    p3 <- ggplot(fig3, aes(ano, total, colour = tipo_resuelto)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.1) +
      scale_color_manual(values = colors_resultado) +
      scale_x_continuous(breaks = sort(unique(fig3$ano))) +
      scale_y_continuous(labels = label_big_n) +
      labs(
        title = "Figura 3. Residencias temporales resueltas, por resultado",
        subtitle = "Otorgadas, rechazadas y archivadas",
        x = "Año", y = "Casos", colour = NULL, caption = "Fuente: elaboración propia con datos abiertos del Servicio Nacional de Migraciones."
      ) +
      theme_datamigra() +
      theme_series_x()
    save_plot(p3, "fig_3_rt_resueltas_resultado")
    save_table(fig3, "fig_3_rt_resueltas_resultado")

    fig4 <- residencias %>%
      filter(tipo_residencia == "Definitiva", etapa == "Acogidas") %>%
      count(ano, wt = total, name = "total")
    p4 <- ggplot(fig4, aes(ano, total)) +
      geom_line(linewidth = 1.2, colour = colors_tipo[["Definitiva"]]) +
      geom_point(size = 2.6, colour = colors_tipo[["Definitiva"]]) +
      scale_x_continuous(breaks = fig4$ano) +
      scale_y_continuous(labels = label_big_n) +
      labs(
        title = "Figura 4. Evolución del número de solicitudes totales de residencias definitivas",
        subtitle = "Serie construida con solicitudes acogidas por el SNM",
        x = "Año", y = "Solicitudes", caption = "Fuente: elaboración propia con datos abiertos del Servicio Nacional de Migraciones."
      ) +
      theme_datamigra() +
      theme_series_x()
    save_plot(p4, "fig_4_rd_solicitudes")
    save_table(fig4, "fig_4_rd_solicitudes")

    fig5 <- residencias %>%
      filter(tipo_residencia == "Definitiva", etapa == "Resueltas", tipo_resuelto == "Otorga") %>%
      count(ano, wt = total, name = "total")
    p5 <- ggplot(fig5, aes(ano, total)) +
      geom_line(linewidth = 1.2, colour = colors_tipo[["Definitiva"]]) +
      geom_point(size = 2.6, colour = colors_tipo[["Definitiva"]]) +
      scale_x_continuous(breaks = fig5$ano) +
      scale_y_continuous(labels = label_big_n) +
      labs(
        title = "Figura 5. Evolución del número de solicitudes de residencias definitivas aprobadas (acogidas)",
        subtitle = "Serie operacional construida con resoluciones favorables (Otorga)",
        x = "Año", y = "Aprobaciones", caption = "Fuente: elaboración propia con datos abiertos del Servicio Nacional de Migraciones."
      ) +
      theme_datamigra() +
      theme_series_x()
    save_plot(p5, "fig_5_rd_aprobadas")
    save_table(fig5, "fig_5_rd_aprobadas")

    fig6 <- residencias %>%
      filter(tipo_residencia == "Definitiva", etapa == "Resueltas", !is.na(tipo_resuelto)) %>%
      count(ano, tipo_resuelto, wt = total, name = "total")
    p6 <- ggplot(fig6, aes(ano, total, colour = tipo_resuelto)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.1) +
      scale_color_manual(values = colors_resultado) +
      scale_x_continuous(breaks = sort(unique(fig6$ano))) +
      scale_y_continuous(labels = label_big_n) +
      labs(
        title = "Figura 6. Residencias definitivas resueltas, por resultado",
        subtitle = "Otorgadas, rechazadas y archivadas",
        x = "Año", y = "Casos", colour = NULL, caption = "Fuente: elaboración propia con datos abiertos del Servicio Nacional de Migraciones."
      ) +
      theme_datamigra() +
      theme_series_x()
    save_plot(p6, "fig_6_rd_resueltas_resultado")
    save_table(fig6, "fig_6_rd_resueltas_resultado")

    fig7 <- residencias %>%
      mutate(es_otorga = etapa == "Resueltas" & tipo_resuelto == "Otorga") %>%
      group_by(tipo_residencia, ano) %>%
      summarise(
        solicitudes = sum(total[etapa == "Acogidas"], na.rm = TRUE),
        aprobadas = sum(total[es_otorga], na.rm = TRUE),
        tasa = ifelse(solicitudes > 0, aprobadas / solicitudes, NA_real_),
        .groups = "drop"
      )
    p7 <- ggplot(fig7, aes(ano, tasa, colour = tipo_residencia)) +
      geom_line(linewidth = 1.15) +
      geom_point(size = 2.2) +
      scale_color_manual(values = colors_tipo) +
      scale_x_continuous(breaks = sort(unique(fig7$ano))) +
      scale_y_continuous(labels = label_pct) +
      labs(
        title = "Figura 7. Tasa de aprobación de residencia, por tipo",
        subtitle = "Aprobadas sobre solicitudes acogidas",
        x = "Año", y = "Tasa de aprobación", colour = NULL, caption = "Fuente: elaboración propia con datos abiertos del Servicio Nacional de Migraciones."
      ) +
      theme_datamigra() +
      theme_series_x()
    save_plot(p7, "fig_7_tasa_aprobacion")
    save_table(fig7, "fig_7_tasa_aprobacion")

    fig8 <- residencias %>%
      filter(tipo_residencia == "Temporal", etapa == "Acogidas", !is.na(origen_solicitud)) %>%
      count(ano, origen_solicitud, wt = total, name = "total") %>%
      group_by(ano) %>%
      mutate(share = total / sum(total)) %>%
      ungroup()
    p8 <- ggplot(fig8, aes(ano, share, colour = origen_solicitud)) +
      geom_line(linewidth = 1.15) +
      geom_point(size = 2.2) +
      scale_color_manual(values = colors_origin) +
      scale_x_continuous(breaks = sort(unique(fig8$ano))) +
      scale_y_continuous(labels = label_pct) +
      labs(
        title = "Figura 8. Origen de solicitud en residencias temporales acogidas",
        subtitle = "Dentro de Chile y fuera de Chile",
        x = "Año", y = "Participación", colour = NULL, caption = "Fuente: elaboración propia con datos abiertos del Servicio Nacional de Migraciones."
      ) +
      theme_datamigra() +
      theme_series_x()
    save_plot(p8, "fig_8_origen_solicitud_rt")
    save_table(fig8, "fig_8_origen_solicitud_rt")

    fig9 <- residencias %>%
      filter(tipo_residencia == "Temporal", etapa == "Acogidas", ano == latest_res_year, !is.na(beneficio_agrupado)) %>%
      mutate(beneficio_agrupado = dplyr::case_when(
        beneficio_agrupado %in% c("Otras residencias temporales", "Otras humanitarias") ~ "Otras",
        TRUE ~ beneficio_agrupado
      )) %>%
      count(beneficio_agrupado, wt = total, name = "total") %>%
      reorder_with_other_last(beneficio_agrupado, total, other_pattern = "^Otras$")
    p9 <- ggplot(fig9, aes(beneficio_agrupado, total)) +
      geom_col(fill = "#5C87A6", width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = label_big_n) +
      labs(
        title = paste0("Figura 9. Principales beneficios en residencias temporales acogidas, ", latest_res_year),
        subtitle = "Beneficios agrupados en el último año disponible",
        x = NULL, y = "Solicitudes", caption = "Fuente: elaboración propia con datos abiertos del Servicio Nacional de Migraciones."
      ) +
      theme_datamigra()
    save_plot(p9, "fig_9_rt_beneficios_2025", width = 11, height = 7)
    save_table(fig9, "fig_9_rt_beneficios_2025")

    fig10 <- residencias %>%
      filter(tipo_residencia == "Temporal", etapa == "Resueltas", tipo_resuelto == "Otorga", ano == latest_res_year, !is.na(pais)) %>%
      count(pais, wt = total, name = "total") %>%
      mutate(
        pais = dplyr::case_when(
          grepl("^Otros", pais) ~ "Otros países",
          TRUE ~ pais
        )
      ) %>%
      group_by(pais) %>%
      summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
      collapse_top_categories(pais, total, n_top = 8, other_label = "Otros países") %>%
      rename(pais = category_group) %>%
      group_by(pais) %>%
      summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
      reorder_with_other_last(pais, total, other_pattern = "^Otros")
    p10 <- ggplot(fig10, aes(pais, total)) +
      geom_col(fill = "#2F6C8F", width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = label_big_n) +
      labs(
        title = paste0("Figura 10. Principales nacionalidades en residencias temporales favorables, ", latest_res_year),
        subtitle = "Nacionalidades agrupadas con Otros países al final",
        x = NULL, y = "Resoluciones favorables", caption = "Fuente: elaboración propia con datos abiertos del Servicio Nacional de Migraciones."
      ) +
      theme_datamigra()
    save_plot(p10, "fig_10_rt_nacionalidades_2025", width = 11, height = 7)
    save_table(fig10, "fig_10_rt_nacionalidades_2025")

    fig11 <- residencias %>%
      filter(tipo_residencia == "Definitiva", etapa == "Resueltas", tipo_resuelto == "Otorga", !is.na(actividad)) %>%
      count(ano, actividad, wt = total, name = "total") %>%
      activity_distribution(top_n = 5)
    p11 <- ggplot(fig11, aes(factor(ano), share, fill = actividad_grp)) +
      geom_col(width = 0.72) +
      scale_y_continuous(labels = label_pct, expand = expansion(mult = c(0, 0.01))) +
      scale_fill_manual(values = colors_activity, breaks = names(colors_activity)) +
      labs(
        title = "Figura 11. Distribución de actividad principal de personas con residencia definitiva aprobada",
        subtitle = "Barra total = 100% en cada año",
        x = "Año", y = "Proporción", fill = NULL, caption = "Fuente: elaboración propia con datos abiertos del Servicio Nacional de Migraciones."
      ) +
      theme_datamigra() +
      theme_series_x()
    save_plot(p11, "fig_11_rd_actividad_distribucion", width = 12, height = 7)
    save_table(fig11, "fig_11_rd_actividad_distribucion")

    fig12 <- residencias %>%
      filter(tipo_residencia == "Temporal", etapa == "Resueltas", tipo_resuelto == "Otorga", !is.na(actividad)) %>%
      count(ano, actividad, wt = total, name = "total") %>%
      activity_distribution(top_n = 5)
    p12 <- ggplot(fig12, aes(factor(ano), share, fill = actividad_grp)) +
      geom_col(width = 0.72) +
      scale_y_continuous(labels = label_pct, expand = expansion(mult = c(0, 0.01))) +
      scale_fill_manual(values = colors_activity, breaks = names(colors_activity)) +
      labs(
        title = "Figura 12. Distribución de actividad principal de personas con residencia temporal aprobada",
        subtitle = "Barra total = 100% en cada año",
        x = "Año", y = "Proporción", fill = NULL, caption = "Fuente: elaboración propia con datos abiertos del Servicio Nacional de Migraciones."
      ) +
      theme_datamigra() +
      theme_series_x()
    save_plot(p12, "fig_12_rt_actividad_distribucion", width = 12, height = 7)
    save_table(fig12, "fig_12_rt_actividad_distribucion")

    fig13 <- residencias %>%
      filter(tipo_residencia == "Definitiva", etapa == "Resueltas", tipo_resuelto == "Otorga", ano == latest_res_year, !is.na(region)) %>%
      count(region, wt = total, name = "total") %>%
      filter(region %in% region_order) %>%
      right_join(tibble(region = region_order), by = "region") %>%
      mutate(
        total = replace_na(total, 0),
        region_label = recode(region, !!!region_display_map),
        region_plot = factor(wrap_label(region_label, width = 24), levels = rev(wrap_label(unname(region_display_map[region_order]), width = 24)))
      ) %>%
      arrange(match(as.character(region), region_order))
    p13 <- ggplot(fig13, aes(region_plot, total)) +
      geom_col(fill = "#7E8EA3", width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = label_big_n) +
      labs(
        title = paste0("Figura 13. Residencias definitivas aprobadas por región, ", latest_res_year),
        subtitle = "Todas las regiones, ordenadas de norte a sur",
        x = NULL, y = "Resoluciones favorables", caption = "Fuente: elaboración propia con datos abiertos del Servicio Nacional de Migraciones."
      ) +
      theme_datamigra()
    save_plot(p13, "fig_13_rd_regiones_2025", width = 11, height = 7)
    save_table(fig13, "fig_13_rd_regiones_2025")

    fig16 <- residencias %>%
      filter(etapa == "Resueltas", tipo_resuelto == "Otorga", ano == latest_res_year, !is.na(sexo), !is.na(tipo_residencia)) %>%
      count(tipo_residencia, sexo, wt = total, name = "total") %>%
      group_by(tipo_residencia) %>%
      mutate(share = total / sum(total)) %>%
      ungroup() %>%
      mutate(
        tipo_residencia = factor(tipo_residencia, levels = c("Temporal", "Definitiva")),
        sexo = factor(sexo, levels = c("Hombre", "Mujer"))
      )
    p16 <- ggplot(fig16, aes(tipo_residencia, share, fill = sexo)) +
      geom_col(width = 0.62) +
      geom_text(aes(label = label_pct(share)), position = position_fill(vjust = 0.5), size = 3.2, colour = "white") +
      scale_fill_manual(values = colors_sex) +
      scale_y_continuous(labels = label_pct, expand = expansion(mult = c(0, 0.02))) +
      labs(
        title = paste0("Figura 16. Composición por sexo en residencias favorables, ", latest_res_year),
        subtitle = "Distribución porcentual dentro de residencias temporales y definitivas aprobadas",
        x = NULL, y = "Participación", fill = NULL, caption = "Fuente: elaboración propia con datos abiertos del Servicio Nacional de Migraciones."
      ) +
      theme_datamigra()
    save_plot(p16, "fig_16_sexo_residencias_favorables_2025", width = 8.5, height = 6.5)
    save_table(fig16, "fig_16_sexo_residencias_favorables_2025")

    fig14 <- refugio %>%
      count(ano, evento, wt = total, name = "total")
    p14 <- ggplot(fig14, aes(ano, total, colour = evento)) +
      geom_line(linewidth = 1.15) +
      geom_point(size = 2.2) +
      scale_color_manual(values = colors_ref) +
      scale_x_continuous(breaks = sort(unique(fig14$ano))) +
      scale_y_continuous(labels = label_big_n) +
      labs(
        title = "Figura 14. Solicitudes, reconocimientos y rechazos de refugio",
        subtitle = "Serie anual disponible",
        x = "Año", y = "Casos", colour = NULL, caption = "Fuente: elaboración propia con datos abiertos del Servicio Nacional de Migraciones."
      ) +
      theme_datamigra() +
      theme_series_x()
    save_plot(p14, "fig_14_refugio_series")
    save_table(fig14, "fig_14_refugio_series")

    fig15 <- refugio %>%
      filter(ano == latest_ref_year, !is.na(pais)) %>%
      count(evento, pais, wt = total, name = "total") %>%
      mutate(pais = if_else(grepl("^Otros", pais), "Otros países", pais)) %>%
      group_by(evento, pais) %>%
      summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
      group_by(evento) %>%
      group_modify(~ {
        collapse_top_categories(.x, pais, total, n_top = 4, other_label = "Otros países") %>%
          rename(pais = category_group) %>%
          group_by(pais) %>%
          summarise(total = sum(total, na.rm = TRUE), .groups = "drop")
      }) %>%
      ungroup()

    order_pais_fig15 <- fig15 %>%
      group_by(pais) %>%
      summarise(total_all = sum(total, na.rm = TRUE), .groups = "drop") %>%
      reorder_with_other_last(pais, total_all, other_pattern = "^Otros") %>%
      pull(pais) %>%
      as.character()

    fig15 <- fig15 %>%
      mutate(pais = factor(pais, levels = levels(reorder_with_other_last(fig15 %>% group_by(pais) %>% summarise(total = sum(total), .groups = "drop"), pais, total, other_pattern = "^Otros")$pais)))

    p15 <- ggplot(fig15, aes(pais, total, fill = evento)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.66) +
      coord_flip() +
      scale_fill_manual(values = colors_ref) +
      scale_y_continuous(labels = label_big_n) +
      labs(
        title = paste0("Figura 15. Nacionalidades principales en refugio, por resolución, ", latest_ref_year),
        subtitle = "Países en filas y resoluciones diferenciadas por color",
        x = NULL, y = "Casos", fill = NULL, caption = "Fuente: elaboración propia con datos abiertos del Servicio Nacional de Migraciones."
      ) +
      theme_datamigra()
    save_plot(p15, "fig_15_refugio_nacionalidades_2025", width = 12, height = 8)
    save_table(fig15, "fig_15_refugio_nacionalidades_2025")

  }

  invisible(list(
    residencias = residencias_final,
    refugio = refugio_final,
    figures = fig_dir,
    tables = tab_dir
  ))
}

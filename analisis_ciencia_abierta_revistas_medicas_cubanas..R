## ============================================================================
## CIENCIA ABIERTA EN REVISTAS MÉDICAS CUBANAS
## Script reproducible de limpieza, validación, análisis, tablas y figuras
## ============================================================================

options(stringsAsFactors = FALSE, scipen = 999, "openxlsx.maxWidth" = 45)

# -----------------------------------------------------------------------------
# 0. CONFIGURACIÓN: este es el único bloque que debe editarse si cambia la ruta
# -----------------------------------------------------------------------------

CARPETA_BASE <- "C:/Users/alexc/Desktop/Coolaboración Lee/Último Análisis"

NOMBRE_ARCHIVO_DATOS <- paste0(
  "Revistas científicas médicas cubanas, adopción e implementación de ",
  "prácticas de Ciencia Abierta, 2026 (1).xlsx"
)
NOMBRE_ARCHIVO_CLUSTER <- "Lista_Revistas_por_Cluster_Final.xlsx"

SEMILLA <- 20260722
N_ESPERADO <- 68L
N_CLUSTER <- 4L
USAR_CLUSTER_FINAL <- TRUE
INSTALAR_PAQUETES_FALTANTES <- TRUE

ejecutar_analisis <- function() {
  if (!dir.exists(CARPETA_BASE)) {
    stop(
      "No se encontró la carpeta de trabajo:\n",
      CARPETA_BASE,
      call. = FALSE
    )
  }
  
  resolver_archivo <- function(nombre_preferido, patron, etiqueta) {
    ruta_preferida <- file.path(CARPETA_BASE, nombre_preferido)
    if (file.exists(ruta_preferida)) {
      return(normalizePath(ruta_preferida, winslash = "/", mustWork = TRUE))
    }
    
    candidatos <- list.files(
      CARPETA_BASE,
      pattern = patron,
      full.names = TRUE,
      ignore.case = TRUE
    )
    candidatos <- candidatos[
      !startsWith(basename(candidatos), "~$") & file.exists(candidatos)
    ]
    
    if (length(candidatos) == 1L) {
      message(
        "Se utilizará automáticamente ", etiqueta, ": ",
        basename(candidatos)
      )
      return(normalizePath(candidatos, winslash = "/", mustWork = TRUE))
    }
    
    if (length(candidatos) == 0L) {
      stop(
        "No se encontró ", etiqueta, " en:\n",
        CARPETA_BASE,
        "\nNombre esperado: ",
        nombre_preferido,
        call. = FALSE
      )
    }
    
    stop(
      "Se encontraron varios archivos posibles para ", etiqueta, ":\n- ",
      paste(basename(candidatos), collapse = "\n- "),
      "\nConserve solamente el archivo correcto o escriba su nombre exacto ",
      "en el bloque de configuración.",
      call. = FALSE
    )
  }
  
  ARCHIVO_DATOS <- resolver_archivo(
    NOMBRE_ARCHIVO_DATOS,
    "^Revistas .*Ciencia Abierta.*[.]xlsx$",
    "la base principal"
  )
  ARCHIVO_CLUSTER <- resolver_archivo(
    NOMBRE_ARCHIVO_CLUSTER,
    "^Lista_Revistas_por_Cluster_Final.*[.]xlsx$",
    "el archivo de clústeres"
  )
  
  CARPETA_SALIDAS <- file.path(CARPETA_BASE, "salidas")
  CARPETA_FIGURAS <- file.path(CARPETA_SALIDAS, "figuras")
  CARPETA_CSV <- file.path(CARPETA_SALIDAS, "tablas_csv")
  CARPETA_DIAGNOSTICO <- file.path(CARPETA_SALIDAS, "diagnostico")
  
  # -----------------------------------------------------------------------------
  # 1. PAQUETES
  # -----------------------------------------------------------------------------
  
  paquetes <- c(
    "readxl", "dplyr", "tidyr", "stringr", "stringi", "ggplot2",
    "FactoMineR", "sandwich", "lmtest", "openxlsx", "flextable",
    "officer", "patchwork", "scales", "tibble"
  )
  
  faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
  
  if (length(faltantes) > 0 && isTRUE(INSTALAR_PAQUETES_FALTANTES)) {
    message("Se instalarán los paquetes faltantes: ", paste(faltantes, collapse = ", "))
    install.packages(
      faltantes,
      repos = "https://cloud.r-project.org",
      dependencies = TRUE
    )
    faltantes <- paquetes[
      !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
    ]
  }
  
  if (length(faltantes) > 0) {
    stop(
      "Faltan paquetes: ", paste(faltantes, collapse = ", "), ".\n",
      "Instálelos con install.packages() y vuelva a ejecutar el script.",
      call. = FALSE
    )
  }
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(ggplot2)
  })
  
  dir.create(CARPETA_SALIDAS, recursive = TRUE, showWarnings = FALSE)
  dir.create(CARPETA_FIGURAS, recursive = TRUE, showWarnings = FALSE)
  dir.create(CARPETA_CSV, recursive = TRUE, showWarnings = FALSE)
  dir.create(CARPETA_DIAGNOSTICO, recursive = TRUE, showWarnings = FALSE)
  
  if (!file.exists(ARCHIVO_DATOS)) {
    stop("No se encontró la base principal:\n", ARCHIVO_DATOS, call. = FALSE)
  }
  if (!file.exists(ARCHIVO_CLUSTER)) {
    stop("No se encontró el archivo de clústeres:\n", ARCHIVO_CLUSTER, call. = FALSE)
  }
  
  # -----------------------------------------------------------------------------
  # 2. FUNCIONES AUXILIARES
  # -----------------------------------------------------------------------------
  
  normalizar_texto <- function(x) {
    x <- as.character(x)
    x <- str_replace_all(x, "\u00A0", " ")
    x <- str_squish(x)
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
    x <- tolower(x)
    x <- str_replace_all(x, "[^a-z0-9]+", " ")
    str_squish(x)
  }
  
  a_binario <- function(x) {
    texto <- normalizar_texto(x)
    resultado <- rep(NA_integer_, length(texto))
    resultado[str_starts(texto, "si") %in% TRUE] <- 1L
    resultado[str_starts(texto, "no") %in% TRUE] <- 0L
    resultado
  }
  
  wilson_ic <- function(x, n, confianza = 0.95) {
    if (n == 0) return(c(inferior = NA_real_, superior = NA_real_))
    z <- qnorm(1 - (1 - confianza) / 2)
    p <- x / n
    denominador <- 1 + z^2 / n
    centro <- (p + z^2 / (2 * n)) / denominador
    amplitud <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denominador
    c(
      inferior = max(0, centro - amplitud),
      superior = min(1, centro + amplitud)
    )
  }
  
  formatear_ic <- function(inferior, superior, digitos = 1) {
    paste0(
      str_replace_all(
        formatC(100 * inferior, format = "f", digits = digitos),
        fixed("."),
        ","
      ),
      "\u2013",
      str_replace_all(
        formatC(100 * superior, format = "f", digits = digitos),
        fixed("."),
        ","
      )
    )
  }
  
  formatear_decimal <- function(x, digitos = 1) {
    str_replace_all(
      formatC(x, format = "f", digits = digitos),
      fixed("."),
      ","
    )
  }
  
  formatear_p <- function(x) {
    ifelse(
      is.na(x),
      NA_character_,
      ifelse(x < 0.001, "<0,001", sub("\\.", ",", sprintf("%.3f", x)))
    )
  }
  
  combinaciones_dos <- function(x) {
    x <- as.numeric(x)
    x * (x - 1) / 2
  }
  
  indice_rand_ajustado <- function(x, y) {
    tabla <- table(x, y)
    suma_celdas <- sum(combinaciones_dos(tabla))
    suma_filas <- sum(combinaciones_dos(rowSums(tabla)))
    suma_columnas <- sum(combinaciones_dos(colSums(tabla)))
    total <- combinaciones_dos(sum(tabla))
    esperado <- suma_filas * suma_columnas / total
    maximo <- (suma_filas + suma_columnas) / 2
    if (isTRUE(all.equal(maximo, esperado))) return(NA_real_)
    (suma_celdas - esperado) / (maximo - esperado)
  }
  
  guardar_csv <- function(x, nombre) {
    write.csv(
      x,
      file = file.path(CARPETA_CSV, paste0(nombre, ".csv")),
      row.names = FALSE,
      fileEncoding = "UTF-8",
      na = ""
    )
  }
  
  guardar_figura <- function(figura, nombre, ancho, alto) {
    ggsave(
      filename = file.path(CARPETA_FIGURAS, paste0(nombre, ".png")),
      plot = figura,
      width = ancho,
      height = alto,
      units = "in",
      dpi = 300,
      bg = "white"
    )
    ggsave(
      filename = file.path(CARPETA_FIGURAS, paste0(nombre, ".tiff")),
      plot = figura,
      width = ancho,
      height = alto,
      units = "in",
      dpi = 600,
      compression = "lzw",
      bg = "white"
    )
  }
  
  tema_publicacion <- function() {
    theme_minimal(base_size = 11, base_family = "sans") +
      theme(
        plot.title = element_text(face = "bold", size = 13, hjust = 0),
        plot.subtitle = element_text(size = 10),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(face = "plain"),
        strip.text = element_text(face = "bold"),
        plot.margin = margin(10, 18, 10, 10)
      )
  }
  
  paleta_cluster <- c(
    "1" = "#0072B2",
    "2" = "#009E73",
    "3" = "#E69F00",
    "4" = "#CC79A7"
  )
  
  # -----------------------------------------------------------------------------
  # 3. LECTURA Y NORMALIZACIÓN DE LAS DOS BASES
  # -----------------------------------------------------------------------------
  
  datos_originales <- readxl::read_excel(ARCHIVO_DATOS)
  cluster_original <- readxl::read_excel(ARCHIVO_CLUSTER)
  
  names(datos_originales) <- str_squish(names(datos_originales))
  names(cluster_original) <- str_squish(names(cluster_original))
  
  columnas_datos_requeridas <- c(
    "Título de Revista", "Tipología", "Disponibilidad", "Antigüedad",
    "Versión OJS", "Tipo de Licencia CC", "Frecuencia publicación",
    "Tipo Acceso Abierto", "Política CA declarada/web",
    "Acepta Preprints/web", "Implementación preprint", "Open Data/web",
    "Implementación Open Data", "Open Review/web",
    "Implementación Open Review", "Tipo(s) de revisión", "DOAJ",
    "SciELO", "Catálogo Latindex 2.0", "Redalyc"
  )
  
  columnas_cluster_requeridas <- c(
    "Título de Revista", "Cluster_Asignado", "Política declarada/web",
    "Acepta Preprints/web", "Open Data/web", "Open Review/web",
    "DOAJ", "SciELO", "Catálogo Latindex 2.0", "Redalyc"
  )
  
  faltan_datos <- setdiff(columnas_datos_requeridas, names(datos_originales))
  faltan_cluster <- setdiff(columnas_cluster_requeridas, names(cluster_original))
  
  if (length(faltan_datos) > 0) {
    stop("Faltan columnas en la base principal: ", paste(faltan_datos, collapse = ", "))
  }
  if (length(faltan_cluster) > 0) {
    stop("Faltan columnas en la base de clústeres: ", paste(faltan_cluster, collapse = ", "))
  }
  
  datos <- datos_originales %>%
    transmute(
      titulo = str_squish(str_replace_all(as.character(`Título de Revista`), "\u00A0", " ")),
      clave_titulo = normalizar_texto(titulo),
      tipologia = str_squish(as.character(Tipología)),
      disponibilidad = as.integer(Disponibilidad),
      antiguedad = as.integer(Antigüedad),
      version_ojs = str_squish(as.character(`Versión OJS`)),
      licencia = str_squish(as.character(`Tipo de Licencia CC`)),
      frecuencia = str_squish(as.character(`Frecuencia publicación`)),
      acceso_abierto = str_squish(as.character(`Tipo Acceso Abierto`)),
      politica_ca = a_binario(`Política CA declarada/web`),
      acepta_preprints = a_binario(`Acepta Preprints/web`),
      implementa_preprints = a_binario(`Implementación preprint`),
      datos_abiertos = a_binario(`Open Data/web`),
      implementa_datos = a_binario(`Implementación Open Data`),
      revision_abierta = a_binario(`Open Review/web`),
      implementa_revision = a_binario(`Implementación Open Review`),
      tipo_revision = str_squish(as.character(`Tipo(s) de revisión`)),
      doaj = a_binario(DOAJ),
      scielo = a_binario(SciELO),
      latindex = a_binario(`Catálogo Latindex 2.0`),
      redalyc = a_binario(Redalyc)
    )
  
  cluster <- cluster_original %>%
    transmute(
      titulo_cluster = str_squish(str_replace_all(as.character(`Título de Revista`), "\u00A0", " ")),
      clave_titulo = normalizar_texto(titulo_cluster),
      cluster_final = as.integer(Cluster_Asignado),
      politica_ca_cluster = a_binario(`Política declarada/web`),
      acepta_preprints_cluster = a_binario(`Acepta Preprints/web`),
      datos_abiertos_cluster = a_binario(`Open Data/web`),
      revision_abierta_cluster = a_binario(`Open Review/web`),
      doaj_cluster = a_binario(DOAJ),
      scielo_cluster = a_binario(SciELO),
      latindex_cluster = a_binario(`Catálogo Latindex 2.0`),
      redalyc_cluster = a_binario(Redalyc)
    )
  
  columnas_binarias_datos <- c(
    "politica_ca", "acepta_preprints", "implementa_preprints",
    "datos_abiertos", "implementa_datos", "revision_abierta",
    "implementa_revision", "doaj", "scielo", "latindex", "redalyc"
  )
  columnas_binarias_cluster <- c(
    "politica_ca_cluster", "acepta_preprints_cluster",
    "datos_abiertos_cluster", "revision_abierta_cluster",
    "doaj_cluster", "scielo_cluster", "latindex_cluster", "redalyc_cluster"
  )
  
  binarias_invalidas_datos <- columnas_binarias_datos[
    vapply(datos[columnas_binarias_datos], anyNA, logical(1))
  ]
  binarias_invalidas_cluster <- columnas_binarias_cluster[
    vapply(cluster[columnas_binarias_cluster], anyNA, logical(1))
  ]
  
  if (length(binarias_invalidas_datos) > 0) {
    stop(
      "Hay valores distintos de Sí/No o celdas vacías en la base principal: ",
      paste(binarias_invalidas_datos, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(binarias_invalidas_cluster) > 0) {
    stop(
      "Hay valores distintos de Sí/No o celdas vacías en la base de clústeres: ",
      paste(binarias_invalidas_cluster, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (anyDuplicated(datos$clave_titulo)) {
    stop("Hay títulos duplicados en la base principal después de normalizarlos.")
  }
  if (anyDuplicated(cluster$clave_titulo)) {
    stop("Hay títulos duplicados en la base de clústeres después de normalizarlos.")
  }
  
  sin_cluster <- anti_join(datos, cluster, by = "clave_titulo")
  sin_datos <- anti_join(cluster, datos, by = "clave_titulo")
  
  if (nrow(sin_cluster) > 0 || nrow(sin_datos) > 0) {
    write.csv(sin_cluster, file.path(CARPETA_DIAGNOSTICO, "titulos_sin_cluster.csv"), row.names = FALSE)
    write.csv(sin_datos, file.path(CARPETA_DIAGNOSTICO, "titulos_sin_datos.csv"), row.names = FALSE)
    stop("Los títulos de las dos bases no coinciden. Revise los archivos diagnósticos.")
  }
  
  comparacion_binaria <- datos %>%
    select(
      clave_titulo, titulo, politica_ca, acepta_preprints, datos_abiertos,
      revision_abierta, doaj, scielo, latindex, redalyc
    ) %>%
    inner_join(cluster, by = "clave_titulo") %>%
    transmute(
      titulo,
      diferencia_politica = politica_ca != politica_ca_cluster,
      diferencia_preprints = acepta_preprints != acepta_preprints_cluster,
      diferencia_datos = datos_abiertos != datos_abiertos_cluster,
      diferencia_revision = revision_abierta != revision_abierta_cluster,
      diferencia_doaj = doaj != doaj_cluster,
      diferencia_scielo = scielo != scielo_cluster,
      diferencia_latindex = latindex != latindex_cluster,
      diferencia_redalyc = redalyc != redalyc_cluster
    )
  
  diferencias_binarias <- comparacion_binaria %>%
    filter(if_any(starts_with("diferencia_"), identity))
  
  guardar_csv(diferencias_binarias, "Diagnostico_diferencias_entre_bases")
  
  if (nrow(diferencias_binarias) > 0) {
    stop("Las dos bases no coinciden en las variables binarias. Revise el diagnóstico.")
  }
  
  datos <- datos %>%
    inner_join(cluster %>% select(clave_titulo, cluster_final), by = "clave_titulo") %>%
    mutate(
      grupo_antiguedad = cut(
        antiguedad,
        breaks = c(-Inf, 10, 20, Inf),
        labels = c("\u226410 años", "11\u201320 años", ">20 años"),
        right = TRUE
      ),
      grupo_tipologia = if_else(
        tipologia %in% c("Sociedad Científica", "Universitaria"),
        "Sociedad científica/universitaria",
        "Institucional/estudiantil"
      ),
      grupo_tipologia = factor(
        grupo_tipologia,
        levels = c("Institucional/estudiantil", "Sociedad científica/universitaria")
      ),
      frecuencia_agrupada = if_else(
        str_starts(normalizar_texto(frecuencia), "continua"),
        "Continua",
        frecuencia
      ),
      numero_indices = doaj + scielo + latindex + redalyc,
      cluster_final = factor(cluster_final, levels = 1:4)
    )
  
  if (nrow(datos) != N_ESPERADO) {
    stop("Se esperaban ", N_ESPERADO, " revistas y se encontraron ", nrow(datos), ".")
  }
  if (any(is.na(datos$cluster_final))) {
    stop("Hay revistas sin clúster final válido.")
  }
  
  n_total <- nrow(datos)
  
  # -----------------------------------------------------------------------------
  # 4. TABLA 1: CARACTERÍSTICAS EDITORIALES
  # -----------------------------------------------------------------------------
  
  resumir_categorias <- function(x, caracteristica, categorias) {
    valores <- as.character(x)
    conteos <- vapply(
      as.character(categorias),
      function(categoria) sum(valores == categoria, na.rm = TRUE),
      integer(1)
    )
    intervalos <- t(vapply(conteos, wilson_ic, numeric(2), n = n_total))
    tibble(
      Característica = caracteristica,
      Categoría = as.character(categorias),
      n = conteos,
      Total = n_total,
      Proporción = conteos / n_total,
      `IC 95 %` = formatear_ic(intervalos[, 1], intervalos[, 2])
    )
  }
  
  tabla_1 <- bind_rows(
    resumir_categorias(
      datos$tipologia,
      "Tipología",
      c("Institucional", "Sociedad Científica", "Universitaria", "Estudiantil")
    ),
    resumir_categorias(
      datos$grupo_antiguedad,
      "Antigüedad",
      c("\u226410 años", "11\u201320 años", ">20 años")
    ),
    resumir_categorias(
      datos$frecuencia_agrupada,
      "Frecuencia de publicación",
      c("Continua", "Cuatrimestral", "Semestral")
    ),
    resumir_categorias(
      datos$version_ojs,
      "Versión de OJS",
      c("3.3.0-18", "2.4.8.0", "2.4.3.0", "2.4.2.0", "2.3.8.0")
    ),
    resumir_categorias(
      datos$licencia,
      "Licencia",
      c("CC BY-NC", "CC BY", "CC BY-NC-SA", "CC BY-NC-ND")
    ),
    resumir_categorias(
      datos$acceso_abierto,
      "Tipo de acceso abierto",
      "Diamante"
    ),
    resumir_categorias(
      datos$tipo_revision,
      "Modalidad de revisión",
      c(
        "Abierta y Doble Ciego",
        "Doble Ciego",
        "Abierta",
        "Abierta, Doble Ciego, Simple Ciego",
        "Simple Ciego",
        "Abierta y Simple Ciego"
      )
    )
  )
  
  conteos_indizacion <- c(
    DOAJ = sum(datos$doaj),
    SciELO = sum(datos$scielo),
    `Catálogo Latindex 2.0` = sum(datos$latindex),
    Redalyc = sum(datos$redalyc)
  )
  intervalos_indizacion <- t(vapply(
    conteos_indizacion,
    wilson_ic,
    numeric(2),
    n = n_total
  ))
  
  tabla_indizacion <- tibble(
    Característica = "Indización",
    Categoría = names(conteos_indizacion),
    n = as.integer(conteos_indizacion),
    Total = n_total,
    Proporción = as.numeric(conteos_indizacion) / n_total,
    `IC 95 %` = formatear_ic(
      intervalos_indizacion[, 1],
      intervalos_indizacion[, 2]
    )
  )
  
  tabla_numero_indices <- resumir_categorias(
    datos$numero_indices,
    "Número de bases de indización",
    0:4
  )
  
  tabla_1 <- bind_rows(tabla_1, tabla_indizacion, tabla_numero_indices)
  
  # -----------------------------------------------------------------------------
  # 5. TABLA 2: DECLARACIÓN E IMPLEMENTACIÓN VERIFICABLE
  # -----------------------------------------------------------------------------
  
  crear_fila_practica <- function(nombre, declarada, implementada) {
    n_declarada <- sum(declarada)
    n_implementada <- sum(implementada)
    ic_declarada <- wilson_ic(n_declarada, n_total)
    ic_implementada <- wilson_ic(n_implementada, n_total)
    discordante_di <- sum(declarada == 1 & implementada == 0)
    discordante_id <- sum(declarada == 0 & implementada == 1)
    total_discordante <- discordante_di + discordante_id
    p_exacta <- if (total_discordante == 0) {
      1
    } else {
      binom.test(
        min(discordante_di, discordante_id),
        total_discordante,
        p = 0.5,
        alternative = "two.sided"
      )$p.value
    }
    
    tibble(
      Práctica = nombre,
      `Declarada n` = n_declarada,
      `Total declaración` = n_total,
      `Declarada proporción` = n_declarada / n_total,
      `Declarada IC 95 %` = formatear_ic(ic_declarada[1], ic_declarada[2]),
      `Implementada n` = n_implementada,
      `Total implementación` = n_total,
      `Implementada proporción` = n_implementada / n_total,
      `Implementada IC 95 %` = formatear_ic(ic_implementada[1], ic_implementada[2]),
      `Brecha (pp)` = 100 * (n_declarada - n_implementada) / n_total,
      `Discordantes D+/I-` = discordante_di,
      `Discordantes D-/I+` = discordante_id,
      `p exacta de McNemar` = p_exacta
    )
  }
  
  tabla_2 <- bind_rows(
    crear_fila_practica("Preprints", datos$acepta_preprints, datos$implementa_preprints),
    crear_fila_practica("Datos abiertos", datos$datos_abiertos, datos$implementa_datos),
    crear_fila_practica(
      "Revisión por pares abierta",
      datos$revision_abierta,
      datos$implementa_revision
    )
  )
  
  # -----------------------------------------------------------------------------
  # 6. ANÁLISIS BIVARIADO COMPLEMENTARIO
  # -----------------------------------------------------------------------------
  
  pruebas_bivariadas <- list()
  resultados_binarios <- c(
    "Datos abiertos declarados" = "datos_abiertos",
    "Revisión abierta declarada" = "revision_abierta",
    "Aceptación de preprints" = "acepta_preprints"
  )
  
  for (nombre_resultado in names(resultados_binarios)) {
    variable_resultado <- resultados_binarios[[nombre_resultado]]
    
    tabla_tipo <- table(datos$grupo_tipologia, datos[[variable_resultado]])
    tabla_edad <- table(datos$grupo_antiguedad, datos[[variable_resultado]])
    
    pruebas_bivariadas[[length(pruebas_bivariadas) + 1]] <- tibble(
      Resultado = nombre_resultado,
      Predictor = "Tipología agrupada",
      Prueba = "Exacta de Fisher",
      p = fisher.test(tabla_tipo)$p.value
    )
    pruebas_bivariadas[[length(pruebas_bivariadas) + 1]] <- tibble(
      Resultado = nombre_resultado,
      Predictor = "Grupo de antigüedad",
      Prueba = "Exacta de Fisher",
      p = fisher.test(tabla_edad)$p.value
    )
  }
  
  tabla_s1_bivariado <- bind_rows(pruebas_bivariadas) %>%
    mutate(`p BH global (6 pruebas)` = p.adjust(p, method = "BH"))
  
  # -----------------------------------------------------------------------------
  # 7. TABLA 3: POISSON CON VARIANZA ROBUSTA
  # -----------------------------------------------------------------------------
  
  ajustar_poisson_robusto <- function(variable_resultado, nombre_resultado) {
    base_modelo <- datos %>%
      mutate(resultado = .data[[variable_resultado]])
    
    modelo <- glm(
      resultado ~ grupo_tipologia + grupo_antiguedad,
      family = poisson(link = "log"),
      data = base_modelo
    )
    
    matriz_robusta <- sandwich::vcovHC(modelo, type = "HC0")
    prueba <- lmtest::coeftest(modelo, vcov. = matriz_robusta)
    terminos <- rownames(prueba)
    conservar <- terminos != "(Intercept)"
    terminos <- terminos[conservar]
    estimaciones <- prueba[conservar, "Estimate"]
    errores <- prueba[conservar, "Std. Error"]
    valores_p <- prueba[conservar, "Pr(>|z|)"]
    
    etiquetas <- c(
      "grupo_tipologiaSociedad científica/universitaria" =
        "Sociedad científica/universitaria vs. institucional/estudiantil",
      "grupo_antiguedad11\u201320 años" =
        "Antigüedad 11\u201320 vs. \u226410 años",
      "grupo_antiguedad>20 años" =
        "Antigüedad >20 vs. \u226410 años"
    )
    
    tibble(
      `Práctica declarada` = nombre_resultado,
      Predictor = unname(etiquetas[terminos]),
      RP = exp(estimaciones),
      `IC 95 % inferior` = exp(estimaciones - qnorm(0.975) * errores),
      `IC 95 % superior` = exp(estimaciones + qnorm(0.975) * errores),
      p = valores_p
    ) %>%
      mutate(
        Predictor = if_else(is.na(Predictor), terminos, Predictor),
        `p BH por resultado` = p.adjust(p, method = "BH")
      )
  }
  
  tabla_3 <- bind_rows(
    ajustar_poisson_robusto("datos_abiertos", "Datos abiertos declarados"),
    ajustar_poisson_robusto("revision_abierta", "Revisión abierta declarada"),
    ajustar_poisson_robusto("acepta_preprints", "Aceptación de preprints")
  ) %>%
    mutate(`p BH global (9 contrastes)` = p.adjust(p, method = "BH"))
  
  # -----------------------------------------------------------------------------
  # 8. ACM Y COMPARACIÓN DIAGNÓSTICA CON HCPC
  # -----------------------------------------------------------------------------
  
  datos_acm <- datos %>%
    transmute(
      `Política CA` = factor(if_else(politica_ca == 1, "Sí", "No"), levels = c("No", "Sí")),
      Preprints = factor(if_else(acepta_preprints == 1, "Sí", "No"), levels = c("No", "Sí")),
      `Datos abiertos` = factor(if_else(datos_abiertos == 1, "Sí", "No"), levels = c("No", "Sí")),
      `Revisión abierta` = factor(if_else(revision_abierta == 1, "Sí", "No"), levels = c("No", "Sí")),
      DOAJ = factor(if_else(doaj == 1, "Sí", "No"), levels = c("No", "Sí")),
      SciELO = factor(if_else(scielo == 1, "Sí", "No"), levels = c("No", "Sí")),
      Latindex = factor(if_else(latindex == 1, "Sí", "No"), levels = c("No", "Sí")),
      Redalyc = factor(if_else(redalyc == 1, "Sí", "No"), levels = c("No", "Sí"))
    )
  
  resultado_acm <- FactoMineR::MCA(datos_acm, graph = FALSE, ncp = 8)
  
  porcentaje_dim_1 <- resultado_acm$eig[1, 2]
  porcentaje_dim_2 <- resultado_acm$eig[2, 2]
  
  coordenadas_acm <- as.data.frame(resultado_acm$ind$coord[, 1:2, drop = FALSE]) %>%
    setNames(c("Dimension_1", "Dimension_2")) %>%
    mutate(
      titulo = datos$titulo,
      cluster_final = datos$cluster_final,
      .before = 1
    )
  
  contribuciones_acm <- as.data.frame(resultado_acm$var$contrib[, 1:2, drop = FALSE]) %>%
    setNames(c("Contribucion_Dim_1", "Contribucion_Dim_2")) %>%
    mutate(
      categoria = rownames(resultado_acm$var$contrib),
      categoria = str_replace(categoria, "_No$", ": No"),
      categoria = str_replace(categoria, "_Sí$", ": Sí"),
      categoria = str_replace(categoria, "\\.No$", ": No"),
      categoria = str_replace(categoria, "\\.Sí$", ": Sí"),
      .before = 1
    ) %>%
    select(categoria, everything())
  
  set.seed(SEMILLA)
  resultado_hcpc <- FactoMineR::HCPC(
    resultado_acm,
    nb.clust = N_CLUSTER,
    consol = TRUE,
    graph = FALSE
  )
  
  cluster_hcpc_crudo <- resultado_hcpc$data.clust$clust
  orden_hcpc <- match(rownames(datos_acm), rownames(resultado_hcpc$data.clust))
  if (any(is.na(orden_hcpc))) {
    stop("No fue posible alinear las filas del HCPC con la base analítica.")
  }
  cluster_hcpc <- factor(as.integer(as.character(cluster_hcpc_crudo[orden_hcpc])))
  comparacion_clusters <- as.data.frame.matrix(table(
    `Clúster final` = datos$cluster_final,
    `HCPC recalculado` = cluster_hcpc
  ))
  comparacion_clusters <- tibble::rownames_to_column(comparacion_clusters, "Clúster final")
  
  ari_clusters <- indice_rand_ajustado(datos$cluster_final, cluster_hcpc)
  
  if (USAR_CLUSTER_FINAL) {
    datos$cluster_analisis <- datos$cluster_final
  } else {
    warning(
      "USAR_CLUSTER_FINAL=FALSE: las tablas y figuras utilizarán el HCPC recalculado."
    )
    datos$cluster_analisis <- cluster_hcpc
  }
  
  # -----------------------------------------------------------------------------
  # 9. TABLAS 4 Y 5: PERFILES Y LISTA DE REVISTAS POR CLÚSTER
  # -----------------------------------------------------------------------------
  
  nombres_perfil <- c(
    "1" = "Alta declaración y baja indexación internacional",
    "2" = "Alta declaración y alta indexación",
    "3" = "Datos abiertos declarados e indexación limitada",
    "4" = "Baja declaración e indexación"
  )
  
  tabla_4 <- datos %>%
    group_by(cluster_analisis) %>%
    summarise(
      n = n(),
      `Política general de Ciencia Abierta` = mean(politica_ca),
      `Aceptación de preprints` = mean(acepta_preprints),
      `Datos abiertos declarados` = mean(datos_abiertos),
      `Revisión abierta declarada` = mean(revision_abierta),
      DOAJ = mean(doaj),
      SciELO = mean(scielo),
      `Catálogo Latindex 2.0` = mean(latindex),
      Redalyc = mean(redalyc),
      .groups = "drop"
    ) %>%
    mutate(
      Clúster = as.integer(as.character(cluster_analisis)),
      `Perfil descriptivo` = unname(nombres_perfil[as.character(Clúster)]),
      .before = 1
    ) %>%
    select(-cluster_analisis)
  
  tabla_4_larga <- datos %>%
    transmute(
      Clúster = as.integer(as.character(cluster_analisis)),
      `Política CA` = politica_ca,
      Preprints = acepta_preprints,
      `Datos abiertos` = datos_abiertos,
      `Revisión abierta` = revision_abierta,
      DOAJ = doaj,
      SciELO = scielo,
      Latindex = latindex,
      Redalyc = redalyc
    ) %>%
    pivot_longer(
      cols = -Clúster,
      names_to = "Variable",
      values_to = "Valor"
    ) %>%
    group_by(Clúster, Variable) %>%
    summarise(
      Numerador = sum(Valor),
      `N clúster` = n(),
      Proporción = mean(Valor),
      .groups = "drop"
    ) %>%
    mutate(`Perfil descriptivo` = unname(nombres_perfil[as.character(Clúster)]))
  
  listas_cluster <- lapply(1:4, function(k) {
    sort(datos$titulo[as.integer(as.character(datos$cluster_analisis)) == k])
  })
  maximo_cluster <- max(lengths(listas_cluster))
  listas_cluster <- lapply(
    listas_cluster,
    function(x) c(x, rep("", maximo_cluster - length(x)))
  )
  names(listas_cluster) <- vapply(
    1:4,
    function(k) paste0("Clúster ", k, " (n=", sum(as.integer(as.character(datos$cluster_analisis)) == k), ")"),
    character(1)
  )
  tabla_5 <- tibble::as_tibble(listas_cluster, .name_repair = "minimal")
  
  # -----------------------------------------------------------------------------
  # 10. FIGURAS
  # -----------------------------------------------------------------------------
  
  datos_figura_1 <- tabla_2 %>%
    transmute(
      Práctica = factor(
        Práctica,
        levels = c("Revisión por pares abierta", "Datos abiertos", "Preprints")
      ),
      Declaración = 100 * `Declarada proporción`,
      Implementación = 100 * `Implementada proporción`
    )
  
  figura_1 <- ggplot(datos_figura_1, aes(y = Práctica)) +
    geom_segment(
      aes(x = Implementación, xend = Declaración, yend = Práctica),
      linewidth = 1.6,
      color = "#B8BEC8"
    ) +
    geom_point(
      aes(x = Declaración, color = "Declaración editorial"),
      size = 3.8
    ) +
    geom_point(
      aes(x = Implementación, color = "Implementación verificable"),
      size = 3.8
    ) +
    geom_text(
      aes(x = Declaración, label = paste0(formatear_decimal(Declaración, 1), " %")),
      hjust = -0.18,
      size = 3.5
    ) +
    geom_text(
      aes(x = Implementación, label = paste0(formatear_decimal(Implementación, 1), " %")),
      hjust = -0.25,
      vjust = 1.45,
      size = 3.5
    ) +
    scale_color_manual(
      values = c(
        "Declaración editorial" = "#0072B2",
        "Implementación verificable" = "#D55E00"
      )
    ) +
    scale_x_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 20),
      expand = expansion(mult = c(0, 0.03))
    ) +
    labs(
      title = "Brecha entre declaración editorial e implementación verificable",
      x = "Porcentaje de revistas",
      y = NULL
    ) +
    tema_publicacion() +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank()
    ) +
    coord_cartesian(clip = "off")
  
  guardar_figura(figura_1, "Figura_1_Brecha_declaracion_implementacion", 8.6, 4.8)
  
  set.seed(SEMILLA)
  coordenadas_figura <- coordenadas_acm %>%
    mutate(
      Dimension_1_jitter = Dimension_1 + rnorm(n(), 0, 0.012),
      Dimension_2_jitter = Dimension_2 + rnorm(n(), 0, 0.012)
    )
  
  figura_2 <- ggplot(
    coordenadas_figura,
    aes(
      x = Dimension_1_jitter,
      y = Dimension_2_jitter,
      color = cluster_final,
      fill = cluster_final
    )
  ) +
    stat_ellipse(
      geom = "polygon",
      type = "norm",
      level = 0.80,
      alpha = 0.12,
      color = NA,
      show.legend = FALSE
    ) +
    geom_point(size = 2.8, alpha = 0.82, shape = 21, stroke = 0.45, color = "white") +
    geom_hline(yintercept = 0, color = "#7A7F87", linewidth = 0.5) +
    geom_vline(xintercept = 0, color = "#7A7F87", linewidth = 0.5) +
    scale_fill_manual(
      values = paleta_cluster,
      labels = paste0(
        "Clúster ", 1:4, " (n=",
        as.integer(table(factor(datos$cluster_final, levels = 1:4))),
        ")"
      )
    ) +
    scale_color_manual(values = paleta_cluster, guide = "none") +
    labs(
      title = "Perfiles editoriales en el plano del ACM",
      subtitle = "Los colores corresponden al archivo de asignación final",
      x = paste0("Dimensión 1 (", formatear_decimal(porcentaje_dim_1, 1), " %)"),
      y = paste0("Dimensión 2 (", formatear_decimal(porcentaje_dim_2, 1), " %)"),
      fill = NULL
    ) +
    coord_equal() +
    tema_publicacion() +
    theme(legend.position = "right")
  
  guardar_figura(figura_2, "Figura_2_ACM_clusters_finales", 9.2, 6.1)
  
  contribucion_dim_1 <- contribuciones_acm %>%
    slice_max(Contribucion_Dim_1, n = 8, with_ties = FALSE) %>%
    arrange(Contribucion_Dim_1) %>%
    mutate(categoria = factor(categoria, levels = categoria))
  
  contribucion_dim_2 <- contribuciones_acm %>%
    slice_max(Contribucion_Dim_2, n = 8, with_ties = FALSE) %>%
    arrange(Contribucion_Dim_2) %>%
    mutate(categoria = factor(categoria, levels = categoria))
  
  grafico_dim_1 <- ggplot(
    contribucion_dim_1,
    aes(
      x = Contribucion_Dim_1,
      y = categoria,
      fill = str_detect(as.character(categoria), ": Sí$")
    )
  ) +
    geom_col(width = 0.78) +
    geom_vline(xintercept = 100 / nrow(contribuciones_acm), linetype = 2, color = "#666666") +
    scale_fill_manual(values = c("FALSE" = "#0072B2", "TRUE" = "#009E73"), guide = "none") +
    labs(title = "Dimensión 1", x = "Contribución (%)", y = NULL) +
    tema_publicacion()
  
  grafico_dim_2 <- ggplot(
    contribucion_dim_2,
    aes(
      x = Contribucion_Dim_2,
      y = categoria,
      fill = str_detect(as.character(categoria), ": Sí$")
    )
  ) +
    geom_col(width = 0.78) +
    geom_vline(xintercept = 100 / nrow(contribuciones_acm), linetype = 2, color = "#666666") +
    scale_fill_manual(values = c("FALSE" = "#0072B2", "TRUE" = "#009E73"), guide = "none") +
    labs(title = "Dimensión 2", x = "Contribución (%)", y = NULL) +
    tema_publicacion()
  
  figura_3 <- grafico_dim_1 + grafico_dim_2 +
    patchwork::plot_annotation(
      title = "Categorías con mayor contribución al ACM",
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )
  
  guardar_figura(figura_3, "Figura_3_Contribuciones_ACM", 12.8, 6.0)
  
  figura_4 <- ggplot(
    tabla_4_larga,
    aes(x = Variable, y = factor(Clúster), fill = 100 * Proporción)
  ) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(
      aes(
        label = sprintf("%.0f", 100 * Proporción),
        color = 100 * Proporción >= 55
      ),
      size = 3.8
    ) +
    scale_fill_gradient(
      low = "#F2F4F7",
      high = "#0072B2",
      limits = c(0, 100),
      name = "Porcentaje (%)"
    ) +
    scale_color_manual(values = c("FALSE" = "#24292F", "TRUE" = "white"), guide = "none") +
    labs(
      title = "Perfil de prácticas declaradas e indización por clúster",
      x = NULL,
      y = "Clúster final"
    ) +
    tema_publicacion() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  
  guardar_figura(figura_4, "Figura_4_Mapa_calor_clusters", 11.5, 4.6)
  
  # -----------------------------------------------------------------------------
  # 11. VALIDACIÓN Y DATOS LIMPIOS
  # -----------------------------------------------------------------------------
  
  distribucion_cluster <- table(factor(datos$cluster_final, levels = 1:4))
  
  tabla_validacion <- tibble(
    Comprobación = c(
      "Número de revistas en la base principal",
      "Número de revistas en la base de clústeres",
      "Títulos sin correspondencia",
      "Diferencias en ocho variables binarias",
      "Distribución de clústeres finales",
      "Concordancia archivo Final vs. HCPC recalculado",
      "Inercia explicada por las dimensiones 1 y 2"
    ),
    Esperado = c(
      as.character(N_ESPERADO),
      as.character(N_ESPERADO),
      "0",
      "0",
      "Definida por archivo Final",
      "Diagnóstico; no se exige identidad",
      "Resultado del ACM"
    ),
    Observado = c(
      as.character(nrow(datos_originales)),
      as.character(nrow(cluster_original)),
      as.character(nrow(sin_cluster) + nrow(sin_datos)),
      as.character(nrow(diferencias_binarias)),
      paste0("C", 1:4, "=", as.integer(distribucion_cluster), collapse = ", "),
      sprintf("ARI=%.3f", ari_clusters),
      sprintf(
        "%.2f %% + %.2f %% = %.2f %%",
        porcentaje_dim_1,
        porcentaje_dim_2,
        porcentaje_dim_1 + porcentaje_dim_2
      )
    ),
    Estado = c("OK", "OK", "OK", "OK", "OK", "ADVERTENCIA", "OK")
  )
  
  datos_limpios <- datos %>%
    transmute(
      `Título de Revista` = titulo,
      Tipología = tipologia,
      Disponibilidad = disponibilidad,
      Antigüedad = antiguedad,
      `Grupo de antigüedad` = as.character(grupo_antiguedad),
      `Versión OJS` = version_ojs,
      Licencia = licencia,
      `Frecuencia original` = frecuencia,
      `Frecuencia agrupada` = frecuencia_agrupada,
      `Tipo de acceso abierto` = acceso_abierto,
      `Política CA declarada` = politica_ca,
      `Acepta preprints` = acepta_preprints,
      `Implementa preprints` = implementa_preprints,
      `Datos abiertos declarados` = datos_abiertos,
      `Implementa datos abiertos` = implementa_datos,
      `Revisión abierta declarada` = revision_abierta,
      `Implementa revisión abierta` = implementa_revision,
      `Modalidad de revisión` = tipo_revision,
      DOAJ = doaj,
      SciELO = scielo,
      `Latindex 2.0` = latindex,
      Redalyc = redalyc,
      `Número de índices` = numero_indices,
      `Grupo de tipología` = as.character(grupo_tipologia),
      `Clúster final` = as.integer(as.character(cluster_final)),
      `Clúster HCPC recalculado` = as.integer(as.character(cluster_hcpc))
    )
  
  # -----------------------------------------------------------------------------
  # 12. EXPORTACIÓN A CSV Y EXCEL
  # -----------------------------------------------------------------------------
  
  tablas_salida <- list(
    Tabla_1_Caracteristicas = tabla_1,
    Tabla_2_Declaracion_Impl = tabla_2,
    Tabla_3_Poisson = tabla_3,
    Tabla_4_Perfiles_Cluster = tabla_4,
    Tabla_5_Revistas_Cluster = tabla_5,
    Tabla_S1_Bivariado = tabla_s1_bivariado,
    Perfil_Cluster_Largo = tabla_4_larga,
    Coordenadas_ACM = coordenadas_acm,
    Contribuciones_ACM = contribuciones_acm,
    Comparacion_Clusters = comparacion_clusters,
    Validacion = tabla_validacion,
    Datos_limpios = datos_limpios
  )
  
  invisible(Map(guardar_csv, tablas_salida, names(tablas_salida)))
  
  ruta_excel <- file.path(CARPETA_SALIDAS, "Tablas_resultados.xlsx")
  wb <- openxlsx::createWorkbook(creator = "Dr. Alejandro García Rivero")
  
  estilo_encabezado <- openxlsx::createStyle(
    fontName = "Aptos",
    fontSize = 10,
    fontColour = "#FFFFFF",
    fgFill = "#1F4E78",
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    wrapText = TRUE,
    border = "Bottom",
    borderColour = "#17365D"
  )
  
  estilo_nota <- openxlsx::createStyle(
    fontName = "Aptos",
    fontSize = 9,
    fontColour = "#44546A",
    textDecoration = "italic",
    wrapText = TRUE
  )
  
  agregar_hoja <- function(nombre_hoja, tabla, titulo, nota = NULL, porcentajes = NULL) {
    # Las tablas de Excel exigen encabezados únicos sin distinguir mayúsculas.
    nombres_originales <- names(tabla)
    nombres_excel <- character(length(nombres_originales))
    nombres_usados <- character()
    
    for (i in seq_along(nombres_originales)) {
      nombre_base <- nombres_originales[i]
      if (is.na(nombre_base) || !nzchar(nombre_base)) {
        nombre_base <- paste0("Columna ", i)
      }
      
      nombre_candidato <- nombre_base
      sufijo <- 2L
      while (tolower(nombre_candidato) %in% tolower(nombres_usados)) {
        nombre_candidato <- paste0(nombre_base, " (", sufijo, ")")
        sufijo <- sufijo + 1L
      }
      
      nombres_excel[i] <- nombre_candidato
      nombres_usados <- c(nombres_usados, nombre_candidato)
    }
    
    names(tabla) <- nombres_excel
    
    openxlsx::addWorksheet(wb, nombre_hoja, gridLines = FALSE)
    openxlsx::writeData(wb, nombre_hoja, titulo, startRow = 1, startCol = 1)
    openxlsx::addStyle(
      wb,
      nombre_hoja,
      openxlsx::createStyle(
        fontName = "Aptos Display",
        fontSize = 15,
        fontColour = "#17365D",
        textDecoration = "bold"
      ),
      rows = 1,
      cols = 1,
      gridExpand = TRUE
    )
    fila_tabla <- if (is.null(nota)) 3 else 4
    if (!is.null(nota)) {
      openxlsx::writeData(wb, nombre_hoja, nota, startRow = 2, startCol = 1)
      openxlsx::addStyle(
        wb, nombre_hoja, estilo_nota,
        rows = 2, cols = seq_len(max(1, ncol(tabla))), gridExpand = TRUE
      )
    }
    openxlsx::writeDataTable(
      wb,
      nombre_hoja,
      tabla,
      startRow = fila_tabla,
      tableStyle = "TableStyleMedium2",
      withFilter = TRUE
    )
    openxlsx::addStyle(
      wb,
      nombre_hoja,
      estilo_encabezado,
      rows = fila_tabla,
      cols = seq_len(ncol(tabla)),
      gridExpand = TRUE,
      stack = TRUE
    )
    openxlsx::freezePane(wb, nombre_hoja, firstActiveRow = fila_tabla + 1, firstActiveCol = 1)
    openxlsx::setColWidths(wb, nombre_hoja, cols = seq_len(ncol(tabla)), widths = "auto")
    if (!is.null(porcentajes)) {
      columnas_pct <- which(names(tabla) %in% porcentajes)
      if (length(columnas_pct) > 0) {
        openxlsx::addStyle(
          wb,
          nombre_hoja,
          openxlsx::createStyle(numFmt = "0.0%"),
          rows = (fila_tabla + 1):(fila_tabla + nrow(tabla)),
          cols = columnas_pct,
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }
  }
  
  agregar_hoja(
    "Tabla 1",
    tabla_1,
    "Tabla 1. Características editoriales de las revistas estudiadas",
    "IC del 95 % de Wilson sin corrección de continuidad.",
    "Proporción"
  )
  agregar_hoja(
    "Tabla 2",
    tabla_2,
    "Tabla 2. Declaración editorial e implementación verificable",
    "La comparación es pareada; p exacta de McNemar calculada con los discordantes.",
    c("Declarada proporción", "Implementada proporción")
  )
  agregar_hoja(
    "Tabla 3",
    tabla_3,
    "Tabla 3. Razones de prevalencia de las prácticas declaradas",
    "Poisson con varianza robusta HC0. Se muestran FDR por resultado y global."
  )
  agregar_hoja(
    "Tabla 4",
    tabla_4,
    "Tabla 4. Perfil de prácticas declaradas e indización por clúster",
    "La asignación principal procede del archivo Lista_Revistas_por_Cluster_Final.xlsx.",
    c(
      "Política general de Ciencia Abierta", "Aceptación de preprints",
      "Datos abiertos declarados", "Revisión abierta declarada",
      "DOAJ", "SciELO", "Catálogo Latindex 2.0", "Redalyc"
    )
  )
  agregar_hoja(
    "Tabla 5",
    tabla_5,
    "Tabla 5. Revistas incluidas en cada clúster final"
  )
  agregar_hoja(
    "Bivariado",
    tabla_s1_bivariado,
    "Tabla suplementaria. Asociaciones bivariadas"
  )
  agregar_hoja(
    "Perfil largo",
    tabla_4_larga,
    "Cálculos auditables del perfil de clúster",
    porcentajes = "Proporción"
  )
  agregar_hoja(
    "Coordenadas ACM",
    coordenadas_acm,
    "Coordenadas individuales del análisis de correspondencias múltiples"
  )
  agregar_hoja(
    "Contribuciones ACM",
    contribuciones_acm,
    "Contribuciones de las categorías a las dimensiones factoriales"
  )
  agregar_hoja(
    "Comparación clúster",
    comparacion_clusters,
    "Comparación diagnóstica: clúster final frente a HCPC recalculado",
    sprintf("Índice de Rand ajustado: %.3f.", ari_clusters)
  )
  agregar_hoja(
    "Validación",
    tabla_validacion,
    "Comprobaciones automáticas del análisis"
  )
  agregar_hoja(
    "Datos limpios",
    datos_limpios,
    "Base analítica limpia",
    "Los datos originales no se modifican; esta hoja documenta las transformaciones."
  )
  
  openxlsx::saveWorkbook(wb, ruta_excel, overwrite = TRUE)
  
  # -----------------------------------------------------------------------------
  # 13. TABLAS PUBLICABLES EN WORD
  # -----------------------------------------------------------------------------
  
  tabla_1_word <- tabla_1 %>%
    mutate(
      `%` = formatear_decimal(100 * Proporción, 1),
      `n/N` = paste0(n, "/", Total)
    ) %>%
    select(Característica, Categoría, `n/N`, `%`, `IC 95 %`)
  
  tabla_2_word <- tabla_2 %>%
    transmute(
      Práctica,
      `Declarada, n (%)` = sprintf(
        "%d (%s)",
        `Declarada n`,
        formatear_decimal(100 * `Declarada proporción`, 1)
      ),
      `IC 95 % declarada` = `Declarada IC 95 %`,
      `Implementada, n (%)` = sprintf(
        "%d (%s)",
        `Implementada n`,
        formatear_decimal(100 * `Implementada proporción`, 1)
      ),
      `IC 95 % implementada` = `Implementada IC 95 %`,
      `Brecha (pp)` = formatear_decimal(`Brecha (pp)`, 1),
      `p exacta` = formatear_p(`p exacta de McNemar`)
    )
  
  tabla_3_word <- tabla_3 %>%
    transmute(
      `Práctica declarada`,
      Predictor,
      `RP (IC 95 %)` = paste0(
        formatear_decimal(RP, 2),
        " (",
        formatear_decimal(`IC 95 % inferior`, 2),
        "\u2013",
        formatear_decimal(`IC 95 % superior`, 2),
        ")"
      ),
      p = formatear_p(p),
      `p FDR por resultado` = formatear_p(`p BH por resultado`),
      `p FDR global` = formatear_p(`p BH global (9 contrastes)`)
    )
  
  tabla_4_word <- tabla_4 %>%
    mutate(
      across(
        c(
          `Política general de Ciencia Abierta`, `Aceptación de preprints`,
          `Datos abiertos declarados`, `Revisión abierta declarada`,
          DOAJ, SciELO, `Catálogo Latindex 2.0`, Redalyc
        ),
        ~ formatear_decimal(100 * .x, 1)
      )
    )
  
  crear_ft <- function(tabla, tamano = 8.5) {
    flextable::flextable(tabla) %>%
      flextable::theme_booktabs() %>%
      flextable::font(fontname = "Arial", part = "all") %>%
      flextable::fontsize(size = tamano, part = "all") %>%
      flextable::bold(part = "header") %>%
      flextable::bg(bg = "#D9EAF7", part = "header") %>%
      flextable::color(color = "#17365D", part = "header") %>%
      flextable::align(align = "center", part = "header") %>%
      flextable::valign(valign = "center", part = "all") %>%
      flextable::autofit()
  }
  
  ft_1 <- crear_ft(tabla_1_word, 8.5)
  ft_2 <- crear_ft(tabla_2_word, 8.5)
  ft_3 <- crear_ft(tabla_3_word, 8.0)
  ft_4 <- crear_ft(tabla_4_word, 7.5)
  ft_5 <- crear_ft(tabla_5, 7.5)
  
  ruta_word <- file.path(CARPETA_SALIDAS, "Tablas_publicables.docx")
  flextable::save_as_docx(
    `Tabla 1. Características editoriales` = ft_1,
    `Tabla 2. Declaración e implementación` = ft_2,
    `Tabla 3. Modelos de Poisson` = ft_3,
    `Tabla 4. Perfiles de clúster` = ft_4,
    `Tabla 5. Revistas por clúster` = ft_5,
    path = ruta_word,
    pr_section = officer::prop_section(
      page_size = officer::page_size(orient = "landscape"),
      page_margins = officer::page_mar(
        top = 0.6,
        bottom = 0.6,
        left = 0.6,
        right = 0.6
      ),
      type = "continuous"
    )
  )
  
  # -----------------------------------------------------------------------------
  # 14. DIAGNÓSTICO Y CIERRE
  # -----------------------------------------------------------------------------
  
  guardar_csv(comparacion_clusters, "Diagnostico_cluster_final_vs_HCPC")
  
  informe_advertencias <- c(
    "ANÁLISIS REPRODUCIBLE DE CIENCIA ABIERTA",
    "",
    paste0("Base principal: ", ARCHIVO_DATOS),
    paste0("Asignación de clústeres: ", ARCHIVO_CLUSTER),
    paste0("Número de revistas: ", n_total),
    paste0(
      "Clústeres finales: ",
      paste0("C", 1:4, "=", as.integer(distribucion_cluster), collapse = ", ")
    ),
    paste0("ARI entre clúster final y HCPC recalculado: ", sprintf("%.3f", ari_clusters)),
    "",
    "La asignación final se usa en tablas y figuras.",
    "El HCPC recalculado se conserva únicamente como diagnóstico.",
    "No interprete los clústeres como grupos causalmente separados ni como prueba de no aleatoriedad.",
    "Las variables de Ciencia Abierta incluidas en el ACM son declaraciones editoriales, no implementación verificable."
  )
  
  writeLines(
    informe_advertencias,
    con = file.path(CARPETA_DIAGNOSTICO, "LEER_ADVERTENCIAS.txt"),
    useBytes = TRUE
  )
  
  captura_sesion <- capture.output(sessionInfo())
  writeLines(
    captura_sesion,
    con = file.path(CARPETA_DIAGNOSTICO, "sessionInfo.txt"),
    useBytes = TRUE
  )
  
  nombres_figuras <- c(
    "Figura_1_Brecha_declaracion_implementacion",
    "Figura_2_ACM_clusters_finales",
    "Figura_3_Contribuciones_ACM",
    "Figura_4_Mapa_calor_clusters"
  )
  archivos_figuras <- unlist(
    lapply(
      nombres_figuras,
      function(nombre) {
        file.path(CARPETA_FIGURAS, paste0(nombre, c(".png", ".tiff")))
      }
    ),
    use.names = FALSE
  )
  archivos_csv <- file.path(
    CARPETA_CSV,
    paste0(names(tablas_salida), ".csv")
  )
  archivos_obligatorios <- c(
    ruta_excel,
    ruta_word,
    archivos_figuras,
    archivos_csv,
    file.path(CARPETA_DIAGNOSTICO, "LEER_ADVERTENCIAS.txt"),
    file.path(CARPETA_DIAGNOSTICO, "sessionInfo.txt")
  )
  salidas_faltantes <- archivos_obligatorios[!file.exists(archivos_obligatorios)]
  
  if (length(salidas_faltantes) > 0) {
    stop(
      "El análisis terminó, pero faltan archivos de salida:\n- ",
      paste(salidas_faltantes, collapse = "\n- "),
      call. = FALSE
    )
  }
  
  message("Análisis terminado y validado correctamente.")
  message("Resultados guardados en: ", CARPETA_SALIDAS)
  
  invisible(
    list(
      carpeta_salidas = CARPETA_SALIDAS,
      archivo_excel = ruta_excel,
      archivo_word = ruta_word,
      figuras = archivos_figuras,
      validacion = tabla_validacion
    )
  )
}

tryCatch(
  ejecutar_analisis(),
  error = function(e) {
    stop(
      "ANÁLISIS INTERRUMPIDO.\n",
      conditionMessage(e),
      "\nNo utilice archivos parciales de esta ejecución.",
      call. = FALSE
    )
  }
)
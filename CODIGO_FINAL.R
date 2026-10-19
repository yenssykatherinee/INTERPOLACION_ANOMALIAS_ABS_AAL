
# INICIO

RUTA_EXCEL      <- "C:/Users/yenss/Desktop/ISAYYEN/DATOS/BASE_CONJUNTA.xlsx"
HOJA_EXCEL      <- NULL
CARPETA_SALIDA  <- "C:/Users/yenss/Desktop/ISAYYEN/RESULTADOS/RESULTADOS_POR_FASES"
RUTA_LIMITE_DEPARTAMENTO <- "C:/Users/yenss/Desktop/ISAYYEN/SALIDAS_GRAFICAS/colombia.geo.json"

VARIABLES_PRINCIPALES <- c("AAL", "ABS")          # variables
COVARIABLES_INFO      <- c("Elipsoidal", "Grav_Obs")   # para el cokriging

NOMBRES_VARIABLE <- c(Elipsoidal = "Altura elipsoidal", Grav_Obs = "Gravedad observada",
                      ABS = "Anomal\u00eda de Bouguer simple", AAL = "Anomal\u00eda de Aire Libre")
UNIDADES_R <- c(Elipsoidal = "m", Grav_Obs = "mGal", ABS = "mGal", AAL = "mGal")

DENSIDAD_BOUGUER <- "2,67 g/cm\u00b3" # Densidad de reducción usada en la anomalía de Bouguer simple


# PARA SALIDAS GRAFICAS

ESTILO_COLOR <- "oasis" # Por lo que nos dijo Cárdenas jiji 
PALETA_OASIS <- c("#1E3CFF", "#2878FF", "#28A0FF", "#00C8FF", "#00E6C8", "#00DC78", "#50DC00", "#A0E600", "#E6F000",
                  "#FFFF00", "#FFD700", "#FFAA00", "#FF7800", "#FF3C00", "#E60000", "#C80050", "#E600B4", "#FF64FF", "#FFC8FF")
N_CLASES_COLOR      <- 30       
N_ISOLINEAS         <- 30      
COORDENADAS_MAPA    <- "planas" 
INCLUIR_CARTUCHO    <- TRUE     
INSTITUCION         <- "UNIVERSIDAD DISTRITAL FRANCISCO JOS\u00c9 DE CALDAS"
AUTOR_MAPA          <- "Elabor\u00f3: Isabella Fajardo Moreno - Yenssy Katherine León Molano"
PALETA_GEOFISICA <- PALETA_OASIS   
escala_grav_color <- function(nombre = NULL, ...) scale_color_gradientn(name = nombre, colours = PALETA_GEOFISICA, ...)

# ATÍPICOS 

ELIMINAR_ATIPICOS   <- FALSE    # No se eliminan, si los quieres borrar pon TRUE
NOMBRE_DEPARTAMENTO <- "CUNDINAMARCA"
INCLUIR_BOGOTA      <- TRUE
CELDA_CUANTIL       <- 0.05     # grados ( son aproximadamente 5,5 km) 
N_CLASES_CUANTIL    <- 5
CALCULAR_AUTOCORRELACION <- TRUE
K_VECINOS_MORAN      <- 8       # Para la matriz de pesos espaciales y se tienen en cuenta 8 vecinos más cercanos
UMBRAL_DELAUNAY_FULL <- 10000
ALFA_LISA            <- 0.05
AJUSTE_P_LISA        <- "none"  # Para el ajuste del LISA, puede ser "none", "fdr" o "bonferroni"

CRITERIOS_W <- c(Reina1 = "Reina 1", KNN1 = "Vecino m\u00e1s cercano 1", KNN2 = "Vecino m\u00e1s cercano 2",
                 Gabriel = "Gabriel", SOI = "Esfera de Influencia", RelNeigh = "Vecinos Relativos")

# NORMALIDAD, TRANSFORMACIONES Y CONFIRMACIÓN

ALFA_NORMALIDAD <- 0.05         # el pvalor
NORMALIZACION_ELEGIDA <- c(AAL = NA_character_, ABS = NA_character_)

# EVALUACIÓN Y ELIMINACIÓN DE LA TENDENCIA

ALFA_TENDENCIA      <- 0.05
UMBRAL_R2_TENDENCIA <- 0.10
FORZAR_TENDENCIA    <- NA
ORDEN_TENDENCIA <- c(AAL = NA_real_, ABS = NA_real_)
MEJORA_R2_ORDEN <- 0.10
ORDEN_MAXIMO    <- 3

# VARIOGRAMA Y ANISOTROPÍA

USAR_ANISOTROPIA              <- TRUE
ANISOTROPIA_MEJORA_MIN        <- 0.20   
FRACCION_DISTANCIA_VARIOGRAMA <- 0.5    #para la distancia/2 (corrección de la reu)
N_BINS_VARIOGRAMA             <- 15
MODELOS_VARIOGRAMA            <- c("Sph", "Exp", "Gau", "Mat")  
KAPPA_MATERN                  <- 1   
HACER_ETAPA_6B                <- FALSE   
CRS_PLANO                     <- 9377   # EPSG sistema de referencia


# LIBRERÍAS Y CREACIÓN DE CARPETAS

paquetes <- c("readxl", "ggplot2", "sp", "gstat", "sf", "viridis", "gridExtra", "moments", "scales", "spdep")
instalar <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(instalar) > 0) install.packages(instalar, dependencies = TRUE)
invisible(lapply(paquetes, library, character.only = TRUE))
PALETA_GEOFISICA <- if (ESTILO_COLOR == "turbo") viridisLite::turbo(19) else PALETA_OASIS
opcionales <- c("jsonlite", "writexl")      # contorno del departamento y Excel de resultados
faltan <- opcionales[!opcionales %in% installed.packages()[, "Package"]]
if (length(faltan) > 0) try(install.packages(faltan, dependencies = TRUE), silent = TRUE)

ETAPAS <- c(e1 = "01_ETAPA_estadistica_descriptiva", e2 = "02_ETAPA_analisis_exploratorio",
            e3 = "03_ETAPA_normalidad", e4 = "04_ETAPA_tendencia", e5 = "05_ETAPA_anisotropia",
            e6 = "06_ETAPA_variograma", resumen = "RESUMEN_GENERAL")


ruta <- function(etapa, sub, tipo, archivo) {
  p <- file.path(CARPETA_SALIDA, ETAPAS[[etapa]])
  if (nzchar(sub))  p <- file.path(p, sub)
  if (nzchar(tipo)) p <- file.path(p, tipo)
  if (!dir.exists(p)) dir.create(p, showWarnings = FALSE, recursive = TRUE)
  file.path(p, archivo)
}
dir.create(CARPETA_SALIDA, showWarnings = FALSE, recursive = TRUE)
intentar <- function(expr, etiqueta) {
  tryCatch(expr, error = function(e) { message(sprintf("  \u274c Fall\u00f3 %s: %s", etiqueta, conditionMessage(e))); NULL })
}
apilar <- function(lst) if (length(lst) > 0) do.call(rbind, lst) else NULL
titulo_etapa <- function(txt) cat("\n\n", strrep("#", 78), "\n#  ", txt, "\n", strrep("#", 78), "\n", sep = "")
guardar <- function(archivo, grafico, w = 9, h = 6, dpi = 220, etiqueta = "") {
  intentar(ggsave(archivo, grafico, width = w, height = h, dpi = dpi, bg = "white"), paste("gr\u00e1fico", etiqueta))
}


# COORDENADAS, ANISOTROPÍA, LISA, GRÁFICOS 

COLORES_CUADRANTE <- c("Alto-Alto" = "#d7191c", "Bajo-Bajo" = "#2c7bb6",
                       "Alto-Bajo" = "#fdae61", "Bajo-Alto" = "#abd9e9", "No significativo" = "grey85")

jitter_duplicados <- function(d) {
  key <- paste(d$Longitud, d$Latitud)
  dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
  if (any(dup)) {

    idx_grupo <- ave(seq_along(key), key, FUN = seq_along)  # 1, 2, 3... dentro de cada grupo
    paso <- 1e-4
    d$Longitud[dup] <- d$Longitud[dup] + (idx_grupo[dup] - 1) * paso
    d$Latitud[dup]  <- d$Latitud[dup]  + (idx_grupo[dup] - 1) * paso * 0.7071068
  }
  d
}

proyectar_km <- function(df) {
  pts <- sf::st_as_sf(data.frame(x = df$Longitud, y = df$Latitud), coords = c("x", "y"), crs = 4326)
  p <- tryCatch(sf::st_transform(pts, CRS_PLANO), error = function(e) sf::st_transform(pts, 3116))
  xy <- sf::st_coordinates(p) / 1000
  df$X <- xy[, 1]; df$Y <- xy[, 2]
  df
}

transformar_anisotropia <- function(df, aniso) {
  th <- aniso$angulo * pi / 180
  df$U <- df$X * sin(th) + df$Y * cos(th)
  df$V <- (df$X * cos(th) - df$Y * sin(th)) / aniso$razon
  df
}

coords_trabajo <- function(df, aniso = NULL) {
  if (is.null(aniso)) { df$U <- df$X; df$V <- df$Y } else df <- transformar_anisotropia(df, aniso)
  df
}

ajustar_modelo_teorico <- function(v_emp) {
  mejor <- NULL
  for (m in MODELOS_VARIOGRAMA) {
    ini <- vgm(psill = max(v_emp$gamma) * 0.8, model = m, range = max(v_emp$dist) / 3,
               nugget = min(v_emp$gamma) * 0.5, kappa = KAPPA_MATERN)
    f <- tryCatch(suppressWarnings(fit.variogram(v_emp, ini)), error = function(e) NULL)
    if (is.null(f) || isTRUE(attr(f, "singular"))) next
    if (is.null(mejor) || attr(f, "SSErr") < attr(mejor, "SSErr")) mejor <- f
  }
  if (is.null(mejor)) {
    mejor <- vgm(psill = max(v_emp$gamma) * 0.8, model = "Sph", range = max(v_emp$dist) / 3,
                 nugget = min(v_emp$gamma) * 0.5)
    attr(mejor, "SSErr") <- NA_real_
  }
  mejor
}

estimar_anisotropia <- function(sp_datos, forma, cutoff, width) {
  v_dir <- as.data.frame(variogram(forma, sp_datos, cutoff = cutoff, width = width, alpha = seq(0, 157.5, by = 22.5)))
  v_dir <- v_dir[is.finite(v_dir$gamma) & v_dir$gamma > 0 & v_dir$np > 5 & v_dir$dist > 0, ]
  if (nrow(v_dir) < 20) return(NULL)
  dirs <- v_dir$dir.hor; h <- v_dir$dist; g <- v_dir$gamma; w2 <- v_dir$np / g^2
  
  ajustar <- function(theta, razon) {
    delta <- (dirs - theta) * pi / 180
    heff <- h * sqrt(cos(delta)^2 + (sin(delta) / razon)^2)
    fn <- function(p) {
      q <- exp(p)
      sum(w2 * (q[1] + q[2] * (1 - exp(-3 * heff / q[3])) - g)^2)
    }
    p0 <- log(c(max(min(g), 1e-6), max(g), 3 * median(heff)))
    o <- tryCatch(optim(p0, fn, method = "Nelder-Mead", control = list(maxit = 800, reltol = 1e-9)),
                  error = function(e) NULL)
    if (is.null(o)) return(NA_real_)
    o2 <- tryCatch(optim(o$par, fn, method = "Nelder-Mead", control = list(maxit = 800, reltol = 1e-10)),
                   error = function(e) NULL)
    min(o$value, if (is.null(o2)) Inf else o2$value)
  }
  
  sse_iso <- ajustar(0, 1)
  busq <- expand.grid(angulo = seq(0, 165, by = 15), razon = seq(0.9, 0.1, by = -0.1))
  busq$SSErr <- mapply(ajustar, busq$angulo, busq$razon)
  if (all(is.na(busq$SSErr))) return(NULL)
  b <- which.min(busq$SSErr)
  # refinamiento local alrededor de la mejor combinación
  ang_f <- (busq$angulo[b] + seq(-10, 10, by = 2.5)) %% 180
  raz_f <- pmin(1, pmax(0.05, busq$razon[b] * c(0.7, 0.8, 0.9, 1, 1.1, 1.25, 1.4)))
  fino <- expand.grid(angulo = ang_f, razon = raz_f)
  fino$SSErr <- mapply(ajustar, fino$angulo, fino$razon)
  busq <- rbind(busq, fino)
  mejor <- which.min(busq$SSErr)
  mejora <- if (is.finite(sse_iso) && sse_iso > 0) 1 - busq$SSErr[mejor] / sse_iso else NA_real_
  list(angulo = busq$angulo[mejor], razon = round(busq$razon[mejor], 3), sse_iso = sse_iso,
       sse_aniso = busq$SSErr[mejor], mejora = mejora,
       adoptada = isTRUE(mejora >= ANISOTROPIA_MEJORA_MIN), tabla = busq)
}

etiqueta_W <- function(id) vapply(id, function(z) {
  if (z %in% names(CRITERIOS_W)) unname(CRITERIOS_W[[z]])
  else if (z == "kNN8") "8 vecinos m\u00e1s cercanos (referencia)" else z
}, character(1), USE.NAMES = FALSE)

construir_nb <- function(tipo, coords) {
  if (tipo %in% c("Reina1", "Gabriel", "SOI", "RelNeigh")) {
    set.seed(2026)
    coords <- coords + matrix(rnorm(length(coords), 0, 2e-6), ncol = 2)
  }
  switch(tipo,
         "Reina1"   = spdep::tri2nb(coords),
         "KNN1"     = spdep::knn2nb(spdep::knearneigh(coords, k = 1)),
         "KNN2"     = spdep::knn2nb(spdep::knearneigh(coords, k = 2)),
         "Gabriel"  = spdep::graph2nb(spdep::gabrielneigh(coords, nnmult = 6), sym = TRUE),
         "SOI"      = spdep::graph2nb(spdep::soi.graph(spdep::tri2nb(coords), coords), sym = TRUE),
         "RelNeigh" = spdep::graph2nb(spdep::relativeneigh(coords, nnmult = 6), sym = TRUE),
         "kNN8"     = spdep::knn2nb(spdep::knearneigh(coords, k = K_VECINOS_MORAN)),
         stop("Criterio de vecindad desconocido: ", tipo))
}

test_moran <- function(df, valor_col, tipo_W = "kNN8") {
  dfj <- jitter_duplicados(df)  # este ayuda a no tener duplicados, por ejemplo, tomas en la misma estació gravimétrica
  coords <- as.matrix(dfj[, c("Longitud", "Latitud")])
  usado <- tipo_W
  if (tipo_W %in% c("Gabriel", "SOI", "RelNeigh") && nrow(coords) > UMBRAL_DELAUNAY_FULL) {
    message(sprintf("  Aviso: '%s' con %d puntos es demasiado lenta/puede colgarse; se usa kNN8 en su lugar (solo para esta etapa).",
                    etiqueta_W(tipo_W), nrow(coords)))
    usado <- "kNN8"
  }
  nb <- tryCatch(construir_nb(usado, coords), error = function(e) {
    message(sprintf("  Aviso: no se pudo construir la matriz '%s' con todos los puntos (%s); se usa kNN8.",
                    etiqueta_W(usado), conditionMessage(e))); NULL })
  if (is.null(nb) && usado != "kNN8") {
    usado <- "kNN8"
    nb <- tryCatch(construir_nb("kNN8", coords), error = function(e) NULL)
  }
  if (is.null(nb)) return(NULL)
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  res <- tryCatch(spdep::moran.test(dfj[[valor_col]], lw, zero.policy = TRUE), error = function(e) NULL)
  if (is.null(res)) return(NULL)
  list(moran_I = unname(res$estimate["Moran I statistic"]), p_valor = res$p.value,
       esperado = unname(res$estimate["Expectation"]), varianza = unname(res$estimate["Variance"]),
       matriz_W = usado, matriz_W_etiqueta = etiqueta_W(usado), lw = lw, datos_j = dfj,
       interpretacion = if (res$p.value < 0.05 && unname(res$estimate["Moran I statistic"]) > 0)
         "Autocorrelaci\u00f3n espacial positiva significativa (justifica interpolar)"
       else "No hay evidencia de autocorrelaci\u00f3n espacial significativa")
}

geary_global <- function(df, valor_col, lw) {
  ge <- spdep::geary.test(df[[valor_col]], lw, zero.policy = TRUE, alternative = "two.sided")
  list(C = unname(ge$estimate["Geary C statistic"]), E = unname(ge$estimate["Expectation"]),
       V = unname(ge$estimate["Variance"]), p = ge$p.value)
}

analisis_local <- function(df, valor_col, lw) {
  x <- df[[valor_col]]
  z <- as.numeric(scale(x))
  wz <- as.numeric(spdep::lag.listw(lw, z, zero.policy = TRUE))
  lm <- spdep::localmoran(x, lw, zero.policy = TRUE, alternative = "two.sided")
  p <- as.numeric(lm[, ncol(lm)])
  p_aj <- if (AJUSTE_P_LISA == "none") p else p.adjust(p, method = AJUSTE_P_LISA)
  cuadrante <- ifelse(z >= 0 & wz >= 0, "Alto-Alto",
                      ifelse(z < 0 & wz < 0, "Bajo-Bajo", ifelse(z >= 0 & wz < 0, "Alto-Bajo", "Bajo-Alto")))
  data.frame(Longitud = df$Longitud, Latitud = df$Latitud, Valor = x, z = z, Wz = wz,
             Ii = as.numeric(lm[, 1]), Z_Ii = as.numeric(lm[, "Z.Ii"]), p_valor = p, p_ajustado = p_aj,
             Cuadrante = cuadrante, Categoria = ifelse(p_aj < ALFA_LISA, cuadrante, "No significativo"),
             stringsAsFactors = FALSE)
}

resumen_lisa <- function(lisa, v) {
  cats <- c("Alto-Alto", "Bajo-Bajo", "Alto-Bajo", "Bajo-Alto", "No significativo")
  fila <- data.frame(Variable = v, N = nrow(lisa))
  for (k in cats) {
    kk <- gsub("[- ]", "_", k)
    fila[[paste0("N_", kk)]] <- sum(lisa$Categoria == k)
    fila[[paste0("Pct_", kk)]] <- round(100 * sum(lisa$Categoria == k) / nrow(lisa), 2)
  }
  fila
}

grafico_dispersion_moran <- function(lisa, variable, I) {
  ggplot(lisa, aes(x = z, y = Wz, color = Cuadrante)) +
    geom_point(size = 0.5, alpha = 0.35) +
    geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") +
    geom_abline(slope = I, intercept = 0, color = "black", linewidth = 0.8) +
    scale_color_manual(values = COLORES_CUADRANTE, name = NULL, breaks = c("Alto-Alto", "Bajo-Bajo", "Alto-Bajo", "Bajo-Alto")) +
    guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) + theme_minimal() +
    labs(title = paste("Diagrama de dispersi\u00f3n de Moran -", variable),
         subtitle = paste0("La pendiente de la recta es el I de Moran = ", round(I, 4)),
         x = paste(variable, "(estandarizada)"), y = "Rezago espacial (estandarizado)")
}

mapa_lisa <- function(lisa, variable, matriz) {
  lisa$Categoria <- factor(lisa$Categoria, levels = c("Alto-Alto", "Bajo-Bajo", "Alto-Bajo", "Bajo-Alto", "No significativo"))
  lisa <- lisa[order(lisa$Categoria == "No significativo", decreasing = TRUE), ]  
  ggplot(lisa, aes(x = Longitud, y = Latitud, color = Categoria)) +
    geom_point(size = 0.8) + coord_fixed(ratio = ratio_aspecto) + theme_minimal() +
    scale_color_manual(values = COLORES_CUADRANTE, drop = FALSE, name = NULL) +
    guides(color = guide_legend(override.aes = list(size = 3))) +
    labs(title = paste("Mapa de cl\u00fasteres LISA -", variable),
         subtitle = paste0("Matriz W: ", matriz, " | nivel de significancia ", ALFA_LISA,
                           if (AJUSTE_P_LISA != "none") paste0(" (ajuste ", AJUSTE_P_LISA, ")") else ""),
         x = "Longitud", y = "Latitud")
}

mapa_significancia_lisa <- function(lisa, variable, matriz) {
  lisa$Nivel <- cut(lisa$p_ajustado, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                    labels = c("p < 0,001", "p < 0,01", "p < 0,05", "No significativo"))
  lisa <- lisa[order(lisa$Nivel == "No significativo", decreasing = TRUE), ]
  ggplot(lisa, aes(x = Longitud, y = Latitud, color = Nivel)) +
    geom_point(size = 0.8) + coord_fixed(ratio = ratio_aspecto) + theme_minimal() +
    scale_color_manual(values = c("p < 0,001" = "#006d2c", "p < 0,01" = "#31a354", "p < 0,05" = "#a1d99b", "No significativo" = "grey85"),
                       name = NULL, drop = FALSE) +
    guides(color = guide_legend(override.aes = list(size = 3))) +
    labs(title = paste("Mapa de significancia del I de Moran local -", variable),
         subtitle = paste0("Matriz W: ", matriz), x = "Longitud", y = "Latitud")
}

leer_anillos_area <- function(ruta, nombre = NOMBRE_DEPARTAMENTO, incluir_bogota = INCLUIR_BOGOTA) {
  norm <- function(x) toupper(chartr("ÁÉÍÓÚáéíóúÑñ", "AEIOUaeiouNn", as.character(x)))
  objetivo <- norm(nombre); if (incluir_bogota) objetivo <- c(objetivo, "BOGOTA")
  capa <- NULL
  if (!is.null(ruta) && file.exists(ruta)) capa <- tryCatch(sf::st_read(ruta, quiet = TRUE), error = function(e) NULL)
  if (is.null(capa)) {
    message("  No se pudo leer el contorno en: ", ruta, "\n  -> se descarga el límite departamental de GADM (paquete geodata)")
    if (!requireNamespace("geodata", quietly = TRUE)) try(install.packages("geodata"), silent = TRUE)
    capa <- tryCatch(sf::st_as_sf(geodata::gadm("COL", level = 1, path = CARPETA_SALIDA)), error = function(e) NULL)
  }
  if (is.null(capa)) stop("No se pudo obtener el contorno de Cundinamarca. Revise RUTA_LIMITE_DEPARTAMENTO o la conexión a internet.")
  capa <- sf::st_transform(sf::st_make_valid(capa), 4326)
  atr <- sf::st_drop_geometry(capa)
  sel <- apply(atr, 1, function(fila) any(vapply(objetivo, function(o) any(grepl(o, norm(fila), fixed = TRUE)), logical(1))))
  if (!any(sel)) stop("El archivo de contorno no tiene ningún elemento llamado ", nombre)
  message("  Contorno leído: ", sum(sel), " polígono(s) (", paste(objetivo, collapse = " + "), ")")
  assign("LIMITE_DEPTOS_SF", capa[sel, ], envir = .GlobalEnv)     
  geom <- sf::st_union(sf::st_geometry(capa[sel, ]))             
  co <- sf::st_coordinates(sf::st_cast(geom, "POLYGON"))
  co <- co[co[, "L1"] == 1, , drop = FALSE]                        
  lapply(split(seq_len(nrow(co)), co[, "L2"]), function(i) cbind(co[i, "X"], co[i, "Y"]))
}

mapa_cuantiles <- function(datos, v, anillos = NULL) {
  d <- datos[!is.na(datos[[v]]), c("Longitud", "Latitud", v)]
  x0 <- min(d$Longitud); y0 <- min(d$Latitud)
  ix <- floor((d$Longitud - x0) / CELDA_CUANTIL); iy <- floor((d$Latitud - y0) / CELDA_CUANTIL)
  g <- aggregate(d[[v]], by = list(ix = ix, iy = iy), FUN = mean); names(g)[3] <- "media"
  g$n_puntos <- aggregate(d[[v]], by = list(ix = ix, iy = iy), FUN = length)$x
  g$xc <- x0 + (g$ix + 0.5) * CELDA_CUANTIL; g$yc <- y0 + (g$iy + 0.5) * CELDA_CUANTIL
  if (!is.null(anillos)) {
    dentro <- rep(FALSE, nrow(g))
    for (a in anillos) dentro <- dentro | (sp::point.in.polygon(g$xc, g$yc, a[, 1], a[, 2]) > 0)
    g <- g[dentro, ]
  }
  cortes <- quantile(g$media, probs = seq(0, 1, length.out = N_CLASES_CUANTIL + 1), names = FALSE)
  g$clase <- findInterval(g$media, cortes[-c(1, length(cortes))]) + 1
  etiquetas <- paste0("[", fmt(cortes[-length(cortes)], 2), ", ", fmt(cortes[-1], 2), "]")
  g$Clase <- factor(g$clase, levels = seq_len(N_CLASES_CUANTIL), labels = etiquetas)
  paleta <- grDevices::colorRampPalette(PALETA_GEOFISICA)(N_CLASES_CUANTIL)
  area <- if (!is.null(anillos) && INCLUIR_BOGOTA) "Cundinamarca y Bogot\u00e1 D.C." else "Departamento de Cundinamarca"
  
  xr <- range(g$xc) + c(-0.5, 0.5) * CELDA_CUANTIL; yr <- range(g$yc) + c(-0.5, 0.5) * CELDA_CUANTIL
  km50 <- 50 / (111.32 * cos(mean(g$yc) * pi / 180))
  xs <- xr[2] - 0.05 * diff(xr) - km50; ys <- yr[1] + 0.04 * diff(yr)
  p <- ggplot(g) +
    geom_tile(aes(x = xc, y = yc, fill = Clase), width = CELDA_CUANTIL, height = CELDA_CUANTIL) +
    scale_fill_manual(values = paleta, drop = FALSE, name = paste0("Cuantiles\n(", v, ", ", UNIDADES_R[[v]], ")")) +
    coord_fixed(ratio = ratio_aspecto) + theme_void() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "right") +
    labs(title = paste0("Mapa de cuantiles de la ", tolower(NOMBRES_VARIABLE[[v]]), "\n", area)) +
    annotate("segment", x = xr[2] - 0.04 * diff(xr), xend = xr[2] - 0.04 * diff(xr), y = yr[2] - 0.13 * diff(yr), yend = yr[2] - 0.03 * diff(yr),
             arrow = grid::arrow(length = grid::unit(0.3, "cm")), linewidth = 1) +
    annotate("text", x = xr[2] - 0.04 * diff(xr), y = yr[2] - 0.155 * diff(yr), label = "N", fontface = "bold") +
    annotate("segment", x = xs, xend = xs + km50, y = ys, yend = ys, linewidth = 1.6) +
    annotate("text", x = xs + km50 / 2, y = ys + 0.03 * diff(yr), label = "50 km", size = 3)
  if (!is.null(anillos)) for (a in anillos)
    p <- p + geom_path(data = data.frame(x = a[, 1], y = a[, 2]), aes(x = x, y = y), color = "black", linewidth = 0.4)
  
  tabla <- data.frame(Variable = v, Clase = seq_len(N_CLASES_CUANTIL), Limite_inferior = cortes[-length(cortes)],
                      Limite_superior = cortes[-1], stringsAsFactors = FALSE)
  tabla$N_celdas <- vapply(tabla$Clase, function(k) sum(g$clase == k), numeric(1))
  tabla$Porcentaje_celdas <- round(100 * tabla$N_celdas / nrow(g), 2)
  list(mapa = p, clases = tabla, celdas = data.frame(Variable = v, g[, c("ix", "iy", "xc", "yc", "media", "n_puntos", "clase")]))
}

grafico_atipicos <- function(datos, v, det) {
  if (is.null(det) || nrow(det) == 0) return(NULL)
  ggplot() +
    geom_point(data = datos, aes(x = Longitud, y = Latitud), color = "grey85", size = 0.3) +
    geom_point(data = det, aes(x = Longitud, y = Latitud, color = Lado), size = 1.3) +
    scale_color_manual(values = c("por debajo" = "#2c7bb6", "por encima" = "#d7191c"), name = "Valor at\u00edpico") +
    coord_fixed(ratio = ratio_aspecto) + theme_minimal() +
    labs(title = paste("Ubicaci\u00f3n de los valores at\u00edpicos (IQR) -", v),
         subtitle = paste(nrow(det), "puntos"), x = "Longitud", y = "Latitud")
}

grafico_hist_caja_qq <- function(x, variable) {
  x <- x[is.finite(x)]
  etq <- paste0(variable, " (", UNIDADES_R[[variable]], ")")
  df <- data.frame(v = x)
  g1 <- ggplot(df, aes(x = v)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "steelblue", color = "white", alpha = 0.8) +
    geom_density(color = "black", linewidth = 0.7) +
    geom_vline(xintercept = mean(x), color = "firebrick", linetype = "dashed") +
    geom_vline(xintercept = median(x), color = "darkorange", linetype = "dotted", linewidth = 0.8) +
    theme_minimal() + labs(title = "Histograma (rojo = media, naranja = mediana)", x = etq, y = "Densidad")
  g2 <- ggplot(df, aes(x = "", y = v)) +
    geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.color = "red", outlier.size = 0.6) +
    theme_minimal() + labs(title = "Diagrama de caja", x = NULL, y = etq)
  g3 <- ggplot(df, aes(sample = v)) +
    stat_qq(size = 0.5, alpha = 0.4, color = "steelblue") + stat_qq_line(color = "darkred") +
    theme_minimal() + labs(title = "Gr\u00e1fico Q-Q normal", x = "Cuantiles te\u00f3ricos", y = "Cuantiles de la muestra")
  gridExtra::arrangeGrob(g1, g2, g3, ncol = 3, widths = c(1.5, 0.8, 1.1),
                         top = paste0(NOMBRES_VARIABLE[[variable]], " (n = ", format(length(x), big.mark = "."), ")"))
}

detectar_iqr <- function(x, k = 1.5) {
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE); iqr <- diff(q)
  x < q[1] - k * iqr | x > q[2] + k * iqr
}

tabla_atipicos <- function(datos, v) {
  x <- datos[[v]]
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE, names = FALSE); iqr <- q[2] - q[1]
  lim <- c(q[1] - 1.5 * iqr, q[2] + 1.5 * iqr)
  idx <- which(!is.na(x) & (x < lim[1] | x > lim[2]))
  lado <- ifelse(x[idx] < lim[1], "por debajo", "por encima")
  det <- data.frame(Variable = rep(v, length(idx)), ID = datos$ID[idx], Longitud = datos$Longitud[idx], Latitud = datos$Latitud[idx],
                    Fuente = if ("Fuente" %in% names(datos)) datos$Fuente[idx] else rep(NA_character_, length(idx)),
                    Elipsoidal = datos$Elipsoidal[idx], Valor = x[idx], Lado = as.character(lado), stringsAsFactors = FALSE)
  n_ok <- sum(!is.na(x))
  res <- data.frame(Variable = v, N = n_ok, Q1 = q[1], Q3 = q[2], Limite_inferior = lim[1], Limite_superior = lim[2],
                    N_por_debajo = sum(lado == "por debajo"), N_por_encima = sum(lado == "por encima"),
                    Pct = 100 * length(idx) / n_ok,
                    Elipsoidal_mediana_atipicos = if (length(idx) > 0) median(det$Elipsoidal) else NA_real_,
                    Eliminados = ELIMINAR_ATIPICOS, stringsAsFactors = FALSE)
  list(detalle = det, resumen = res)
}

cumple_norm <- function(p) ifelse(is.na(p), NA_character_, ifelse(p >= ALFA_NORMALIDAD, "SI", "NO"))

fmt <- function(x, d = 4) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ","))

fmtp <- function(p) ifelse(is.na(p), "NA", ifelse(p < 0.001, "< 0,001", formatC(p, format = "f", digits = 4, decimal.mark = ",")))

# PRUEBAS DE NORMALIDAD Y TRANSFORMACIONES

prueba_shapiro <- function(x, n_max = 5000, semilla = 123) {
  x <- x[is.finite(x)]; n <- length(x)
  if (n > n_max) { set.seed(semilla); x <- sample(x, n_max) }
  r <- shapiro.test(x)
  data.frame(Prueba = "Shapiro-Wilk", Estadistico = unname(r$statistic), Nombre_estadistico = "W", p_valor = r$p.value, N_usado = length(x))
}
prueba_jarque_bera <- function(x) {
  x <- x[is.finite(x)]; r <- moments::jarque.test(x)
  data.frame(Prueba = "Jarque-Bera", Estadistico = unname(r$statistic), Nombre_estadistico = "JB", p_valor = r$p.value, N_usado = length(x))
}
prueba_anderson_darling <- function(x) {
  x <- sort(x[is.finite(x)]); n <- length(x)
  p <- pmin(pmax(pnorm((x - mean(x)) / sd(x)), 1e-12), 1 - 1e-12)
  i <- seq_len(n)
  A <- -n - mean((2 * i - 1) * (log(p) + log(1 - rev(p))))
  A2 <- A * (1 + 0.75 / n + 2.25 / n^2)
  pv <- if (A2 >= 0.6) exp(1.2937 - 5.709 * A2 + 0.0186 * A2^2)
        else if (A2 > 0.34) exp(0.9177 - 4.279 * A2 - 1.38 * A2^2)
        else if (A2 > 0.2) 1 - exp(-8.318 + 42.796 * A2 - 59.938 * A2^2)
        else 1 - exp(-13.436 + 101.14 * A2 - 223.73 * A2^2)
  data.frame(Prueba = "Anderson-Darling", Estadistico = A2, Nombre_estadistico = "A2*", p_valor = min(max(pv, 0), 1), N_usado = n)
}

#     TRES PRUEBAS DE NORMALIDAD
pruebas_normalidad <- function(x, variable, momento) {
  t <- rbind(prueba_shapiro(x), prueba_jarque_bera(x), prueba_anderson_darling(x))
  data.frame(Variable = variable, Momento = momento, t,
             Asimetria = moments::skewness(x[is.finite(x)]), Curtosis = moments::kurtosis(x[is.finite(x)]),
             Es_normal = ifelse(t$p_valor >= ALFA_NORMALIDAD, "SI", "NO"),
             Conclusion = ifelse(t$p_valor >= ALFA_NORMALIDAD, "No se rechaza H0: compatible con la normal",
                                 "Se rechaza H0: NO es normal"), stringsAsFactors = FALSE)
}

#     GRÁFICOS DE LA DISTRIBUCIÓN DE LOS DATOS
grafico_normalidad <- function(x, titulo) {
  x <- x[is.finite(x)]; mu <- mean(x); s <- sd(x); df <- data.frame(v = x)
  g1 <- ggplot(df, aes(v)) + geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "steelblue", color = "white", alpha = .8) +
    stat_function(fun = dnorm, args = list(mean = mu, sd = s), color = "firebrick", linewidth = 1) +
    theme_minimal() + labs(title = "Histograma y curva normal teórica", x = NULL, y = "Densidad")
  g2 <- ggplot(df, aes(sample = v)) + stat_qq(size = .5, alpha = .4, color = "steelblue") + stat_qq_line(color = "firebrick", linewidth = .9) +
    theme_minimal() + labs(title = "Gráfico Q-Q normal", x = "Cuantiles teóricos", y = "Cuantiles de la muestra")
  g3 <- ggplot(df, aes(v)) + stat_ecdf(color = "steelblue", linewidth = .9) +
    stat_function(fun = pnorm, args = list(mean = mu, sd = s), color = "firebrick", linewidth = .9, linetype = "dashed") +
    theme_minimal() + labs(title = "Distribución acumulada (azul) vs normal (rojo)", x = NULL, y = "F(x)")
  gridExtra::arrangeGrob(g1, g2, g3, ncol = 3, top = titulo)
}

# NORMALIZACIÓN UTILIZADA (YEO-JOHNSON Y ANAMORFOSIS GAUSSIANA)

#ANAMORFOSIS GAUSSIANA
norm_anamorfosis <- function(x) {
  x <- x[is.finite(x)]; ord <- sort(x); n <- length(ord)
  z_ord <- qnorm((rank(ord, ties.method = "average") - 0.5) / n)
  xs_u <- unique(ord)
  zs_u <- as.numeric(tapply(z_ord, factor(match(ord, xs_u), levels = seq_along(xs_u)), mean))
  fwd <- function(v) approx(xs_u, zs_u, xout = v, rule = 2)$y
  list(nombre = "Anamorfosis gaussiana", id = "anamorfosis", fwd = fwd,
       inv = function(z) approx(zs_u, xs_u, xout = z, rule = 2)$y,
       desc = paste0("z = qnorm((rango - 0,5)/n), n = ", n, " datos y ", length(xs_u), " valores únicos"))
}

yj_fwd <- function(x, l) {
  y <- numeric(length(x)); pos <- x >= 0
  y[pos]  <- if (abs(l) > 1e-8) ((x[pos] + 1)^l - 1) / l else log1p(x[pos])
  y[!pos] <- if (abs(l - 2) > 1e-8) -(((1 - x[!pos])^(2 - l) - 1) / (2 - l)) else -log1p(-x[!pos])
  y
}
yj_inv <- function(y, l) {
  x <- numeric(length(y)); pos <- y >= 0
  x[pos]  <- if (abs(l) > 1e-8) pmax(l * y[pos] + 1, 1e-12)^(1 / l) - 1 else expm1(y[pos])
  x[!pos] <- if (abs(l - 2) > 1e-8) 1 - pmax(1 - (2 - l) * y[!pos], 1e-12)^(1 / (2 - l)) else 1 - exp(-y[!pos])
  x
}
# YEO- JOHNSON
norm_yeojohnson <- function(x) {
  x <- x[is.finite(x)]; m0 <- mean(x); s0 <- sd(x); xs <- (x - m0) / s0; n <- length(xs)
  lj <- sum(sign(xs) * log1p(abs(xs)))
  ll <- function(l) { y <- yj_fwd(xs, l); -(n / 2) * log(mean((y - mean(y))^2)) + (l - 1) * lj }
  lambda <- optimize(ll, c(-5, 5), maximum = TRUE)$maximum
  list(nombre = "Yeo-Johnson", id = "yeojohnson",
       fwd = function(v) yj_fwd((v - m0) / s0, lambda),
       inv = function(z) m0 + s0 * yj_inv(z, lambda),
       desc = paste0("Yeo-Johnson de (x - ", formatC(m0, format = "f", digits = 3), ")/", formatC(s0, format = "f", digits = 3),
                     " con lambda = ", formatC(lambda, format = "f", digits = 4)), lambda = lambda)
}

# COMPARATIVO ANTES Y DESPUÉS DE NORMALIZAR
grafico_antes_despues <- function(x, z, titulo_antes, titulo_despues) {
  h <- function(v, t) ggplot(data.frame(v = v), aes(v)) + geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "steelblue", color = "white", alpha = .8) +
    stat_function(fun = dnorm, args = list(mean = mean(v), sd = sd(v)), color = "firebrick", linewidth = 1) + theme_minimal() + labs(title = t, x = NULL, y = "Densidad")
  q <- function(v, t) ggplot(data.frame(v = v), aes(sample = v)) + stat_qq(size = .5, alpha = .4, color = "steelblue") + stat_qq_line(color = "firebrick") +
    theme_minimal() + labs(title = t, x = "Cuantiles teóricos", y = "Cuantiles de la muestra")
  gridExtra::arrangeGrob(h(x, titulo_antes), h(z, titulo_despues), q(x, paste("Q-Q:", titulo_antes)), q(z, paste("Q-Q:", titulo_despues)), ncol = 2)
}

#  VARIOGRAMA Y ANISOTROPÍA 
rango_practico <- function(m, a, kappa = KAPPA_MATERN) {
  if (!is.finite(a)) return(NA_real_)
  switch(as.character(m), Sph = a, Exp = 3 * a, Gau = sqrt(3) * a,
         Mat = a * uniroot(function(t) 2^(1 - kappa) / gamma(kappa) * t^kappa * besselK(t, kappa) - 0.05, c(1e-6, 50))$root, a)
}
comparar_modelos <- function(v_emp) {
  filas <- lapply(MODELOS_VARIOGRAMA, function(m) {
    ini <- vgm(psill = max(v_emp$gamma) * 0.8, model = m, range = max(v_emp$dist) / 3,
               nugget = min(v_emp$gamma) * 0.5, kappa = KAPPA_MATERN)
    f <- tryCatch(suppressWarnings(fit.variogram(v_emp, ini)), error = function(e) NULL)
    if (is.null(f)) return(NULL)
    data.frame(Modelo = m, Pepita = sum(f$psill[f$model == "Nug"]), Meseta_parcial = sum(f$psill[f$model != "Nug"]),
               Rango_km = f$range[f$model != "Nug"][1], Rango_practico_km = rango_practico(m, f$range[f$model != "Nug"][1]),
               SSErr = attr(f, "SSErr"),
               Singular = isTRUE(attr(f, "singular")), stringsAsFactors = FALSE)
  })
  t <- do.call(rbind, filas)
  if (!is.null(t)) { t$Elegido <- FALSE; ok <- !t$Singular & is.finite(t$SSErr)
                     if (any(ok)) t$Elegido[which(ok)[which.min(t$SSErr[ok])]] <- TRUE }
  t
}

# Diagrama de rosa, este está en el libro del profe
rosa_anisotropia <- function(vd, gamma_ref, cutoff) {
  dirs <- sort(unique(vd$dir.hor))
  r <- vapply(dirs, function(a) {
    s <- vd[vd$dir.hor == a & is.finite(vd$gamma), ]; s <- s[order(s$dist), ]
    i <- which(s$gamma >= gamma_ref)[1]
    if (is.na(i)) return(NA_real_)
    if (i == 1) return(s$dist[1])
    s$dist[i - 1] + (gamma_ref - s$gamma[i - 1]) / (s$gamma[i] - s$gamma[i - 1]) * (s$dist[i] - s$dist[i - 1])
  }, numeric(1))
  data.frame(Direccion = dirs, Distancia_km = r, No_alcanza = is.na(r), Distancia_graf = ifelse(is.na(r), cutoff, r))
}

grafico_rosa <- function(ros, variable, gamma_ref) {
  d2 <- rbind(ros, transform(ros, Direccion = Direccion + 180))     
  d2 <- d2[order(d2$Direccion), ]; d2 <- rbind(d2, d2[1, ])
  th <- d2$Direccion * pi / 180
  d2$x <- d2$Distancia_graf * sin(th); d2$y <- d2$Distancia_graf * cos(th)
  ggplot(d2, aes(x = x, y = y)) + geom_path(color = "firebrick", linewidth = 1) +
    geom_point(aes(shape = No_alcanza), size = 2.2) +
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 1), labels = c("alcanza", "no alcanza (valor de corte)"), name = NULL) +
    geom_hline(yintercept = 0, color = "grey70") + geom_vline(xintercept = 0, color = "grey70") +
    coord_fixed() + theme_minimal() +
    labs(title = paste("Diagrama de rosa -", variable),
         subtitle = paste0("Distancia (km) a la que el variograma alcanza γ = ", round(gamma_ref, 4), " (N arriba)"),
         x = "Este (km)", y = "Norte (km)")
}

grafico_variograma_ajustado <- function(v_emp, modelo, titulo, subt = NULL) {
  linea <- variogramLine(modelo, maxdist = max(v_emp$dist))
  ggplot() +
    geom_point(data = v_emp, aes(x = dist, y = gamma, size = np), color = "steelblue") +
    geom_line(data = linea, aes(x = dist, y = gamma), color = "firebrick", linewidth = 1) +
    scale_size_continuous(name = "Pares", range = c(1, 4)) +
    expand_limits(y = 0) + theme_minimal() +
    labs(title = titulo, subtitle = subt, x = "Distancia (km)", y = "Semivarianza")
}


# CARGA DE LA BASE DE DATOS

titulo_etapa("CARGA DE LA BASE DE DATOS")
datos_crudos <- read_excel(RUTA_EXCEL, sheet = HOJA_EXCEL)
names(datos_crudos) <- trimws(names(datos_crudos))
cols_num <- c("Longitud", "Latitud", "Elipsoidal", "Grav_Obs", "ABS", "AAL")
faltantes <- setdiff(cols_num, names(datos_crudos))
if (length(faltantes) > 0) stop("Faltan columnas en la base: ", paste(faltantes, collapse = ", "),
                                "\nColumnas encontradas: ", paste(names(datos_crudos), collapse = ", "))
datos <- as.data.frame(datos_crudos)
for (cc in cols_num) datos[[cc]] <- suppressWarnings(as.numeric(gsub(",", ".", as.character(datos[[cc]]))))
n_antes <- nrow(datos)
datos <- datos[!is.na(datos$Longitud) & !is.na(datos$Latitud) & !is.na(datos$AAL) & !is.na(datos$ABS), ]
if (nrow(datos) < n_antes) message(sprintf("Se descartaron %d filas con datos vac\u00edos.", n_antes - nrow(datos)))
if (!"Fuente" %in% names(datos)) datos$Fuente <- "Base conjunta"
datos$ID <- seq_len(nrow(datos))
cat("Registros:", nrow(datos), "\n"); print(table(datos$Fuente))
n_dup <- sum(duplicated(datos[, c("Longitud", "Latitud")]))
if (n_dup > 0) cat("Aviso:", n_dup, "coordenada(s) repetida(s): no se eliminan (se desplazan ~11 m solo donde el variograma lo exige).\n")
ratio_aspecto <- 1 / cos(mean(datos$Latitud) * pi / 180)
VARIABLES_TODAS <- c(VARIABLES_PRINCIPALES, COVARIABLES_INFO)
anillos <- leer_anillos_area(RUTA_LIMITE_DEPARTAMENTO)   

# ESTADÍSTICA DESCRIPTIVA

titulo_etapa("ETAPA 1: ESTAD\u00cdSTICA DESCRIPTIVA")
estad <- function(x) {
  x <- x[is.finite(x)]; q <- quantile(x, c(.01, .05, .10, .25, .75, .90, .95, .99), names = FALSE)
  data.frame(N = length(x), Minimo = min(x), P01 = q[1], P05 = q[2], P10 = q[3], Q1 = q[4], Media = mean(x), Mediana = median(x),
             Q3 = q[5], P90 = q[6], P95 = q[7], P99 = q[8], Maximo = max(x), Rango = diff(range(x)), IQR = q[5] - q[4],
             Desv_estandar = sd(x), Coef_variacion_pct = 100 * sd(x) / abs(mean(x)),
             Asimetria = moments::skewness(x), Curtosis = moments::kurtosis(x))
}
t_desc <- do.call(rbind, lapply(VARIABLES_PRINCIPALES, function(v) data.frame(Variable = v, Unidad = UNIDADES_R[[v]], estad(datos[[v]]))))
cat("===== Estad\u00edsticos descriptivos =====\n"); print(t_desc, digits = 5, row.names = FALSE)
write.csv(t_desc, ruta("e1", "", "tablas", "TABLA_estadisticos_descriptivos.csv"), row.names = FALSE)

t_desc_fuente <- do.call(rbind, lapply(VARIABLES_PRINCIPALES, function(v)
  do.call(rbind, lapply(unique(datos$Fuente), function(f) data.frame(Variable = v, Fuente = f, estad(datos[[v]][datos$Fuente == f]))))))
write.csv(t_desc_fuente, ruta("e1", "", "tablas", "TABLA_estadisticos_por_fuente.csv"), row.names = FALSE)

t_frec <- do.call(rbind, lapply(VARIABLES_PRINCIPALES, function(v) {
  x <- datos[[v]]; k <- ceiling(1 + log2(length(x)))
  cortes <- seq(min(x), max(x), length.out = k + 1)
  cl <- cut(x, cortes, include.lowest = TRUE)
  f <- as.numeric(table(cl))
  data.frame(Variable = v, Clase = levels(cl), Marca = (cortes[-1] + cortes[-length(cortes)]) / 2, Frecuencia = f,
             Frec_relativa_pct = 100 * f / sum(f), Frec_acumulada_pct = 100 * cumsum(f) / sum(f))
}))
write.csv(t_frec, ruta("e1", "", "tablas", "TABLA_frecuencias.csv"), row.names = FALSE)

largo <- do.call(rbind, lapply(VARIABLES_PRINCIPALES, function(v) data.frame(Variable = v, Valor = datos[[v]], Fuente = datos$Fuente)))
guardar(ruta("e1", "", "graficos", "GRAFICO_histogramas.png"),
  ggplot(largo, aes(x = Valor)) + geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "steelblue", color = "white", alpha = .8) +
    geom_density(color = "darkred", linewidth = .8) + facet_wrap(~Variable, scales = "free") + theme_minimal() +
    labs(title = "Distribuci\u00f3n de las anomal\u00edas (mGal)", x = "mGal", y = "Densidad"), 10, 4.5, 250, "histogramas")
guardar(ruta("e1", "", "graficos", "GRAFICO_cajas.png"),
  ggplot(largo, aes(x = Variable, y = Valor)) + geom_boxplot(fill = "steelblue", alpha = .6, outlier.color = "red", outlier.size = .6) +
    facet_wrap(~Variable, scales = "free") + theme_minimal() + labs(title = "Diagramas de caja", x = NULL, y = "mGal"), 8, 4.5, 250, "cajas")
guardar(ruta("e1", "", "graficos", "GRAFICO_cajas_por_fuente.png"),
  ggplot(largo, aes(x = Fuente, y = Valor, fill = Fuente)) + geom_boxplot(alpha = .7, outlier.size = .6) +
    facet_wrap(~Variable, scales = "free") + theme_minimal() + theme(legend.position = "none") +
    labs(title = "Anomal\u00edas por fuente de datos", x = NULL, y = "mGal"), 9, 4.5, 250, "cajas por fuente")
guardar(ruta("e1", "", "graficos", "GRAFICO_densidad_por_fuente.png"),
  ggplot(largo, aes(Valor, fill = Fuente, color = Fuente)) + geom_density(alpha = .3) + facet_wrap(~Variable, scales = "free") +
    theme_minimal() + labs(title = "Densidad de las anomal\u00edas por fuente", x = "mGal", y = "Densidad"), 10, 4.5, 250, "densidad por fuente")
guardar(ruta("e1", "", "graficos", "GRAFICO_distribucion_acumulada.png"),
  ggplot(largo, aes(Valor, color = Fuente)) + stat_ecdf() + facet_wrap(~Variable, scales = "free_x") +
    theme_minimal() + labs(title = "Distribuci\u00f3n acumulada emp\u00edrica", x = "mGal", y = "F(x)"), 10, 4.5, 250, "ECDF")
for (v in VARIABLES_PRINCIPALES)
  guardar(ruta("e1", "", "graficos", paste0("GRAFICO_histograma_caja_QQ_", v, ".png")), grafico_hist_caja_qq(datos[[v]], v), 13, 4.6, 250, paste("hist+caja+QQ", v))


# ANÁLISIS EXPLORATORIO
titulo_etapa("ETAPA 2: AN\u00c1LISIS EXPLORATORIO DE DATOS")

#     DISTRIBUCIÓN ESPACIAL

cat("\n>>> 2a. Mapas de distribuci\u00f3n espacial\n")
guardar(ruta("e2", "a_mapas", "graficos", "MAPA_estaciones_por_fuente.png"),
  ggplot(datos, aes(Longitud, Latitud, color = Fuente)) + geom_point(size = .6, alpha = .7) + coord_fixed(ratio = ratio_aspecto) +
    theme_minimal() + guides(color = guide_legend(override.aes = list(size = 3))) + labs(title = "Distribuci\u00f3n de las estaciones gravim\u00e9tricas por fuente"),
  7.5, 7, 250, "mapa de fuentes")
acc_cuant <- list()
for (v in VARIABLES_PRINCIPALES) {
  guardar(ruta("e2", "a_mapas", "graficos", paste0("MAPA_puntos_", v, ".png")),
    ggplot(datos[order(datos[[v]]), ], aes(Longitud, Latitud, color = .data[[v]])) + geom_point(size = .8) +
      escala_grav_color(paste0(v, " (mGal)")) + coord_fixed(ratio = ratio_aspecto) + theme_minimal() +
      labs(title = paste("Distribuci\u00f3n espacial de la", tolower(NOMBRES_VARIABLE[[v]]))), 7.5, 7, 250, paste("mapa de puntos", v))
  cq <- intentar(mapa_cuantiles(datos, v, anillos), paste("mapa de cuantiles", v))
  if (!is.null(cq)) {
    ggsave(ruta("e2", "a_mapas", "graficos", paste0("MAPA_cuantiles_", v, ".png")), cq$mapa, width = 8.5, height = 9, dpi = 250)
    write.csv(cq$clases, ruta("e2", "a_mapas", "tablas", paste0("TABLA_cuantiles_clases_", v, ".csv")), row.names = FALSE)
    write.csv(cq$celdas, ruta("e2", "a_mapas", "datos", paste0("cuantiles_celdas_", v, ".csv")), row.names = FALSE)
    acc_cuant[[v]] <- cq$clases
  }
}
capa_contorno <- function() if (is.null(anillos)) NULL else
  lapply(seq_along(anillos), function(i) geom_path(data = data.frame(x = anillos[[i]][, 1], y = anillos[[i]][, 2]), aes(x, y), inherit.aes = FALSE, color = "grey25", linewidth = .4))
mapas_pts <- lapply(VARIABLES_PRINCIPALES, function(v)
  ggplot(datos[order(datos[[v]]), ], aes(Longitud, Latitud, color = .data[[v]])) + capa_contorno() + geom_point(size = .6) +
    escala_grav_color("mGal") + coord_fixed(ratio = ratio_aspecto) + theme_minimal() +
    labs(title = paste(v, "-", NOMBRES_VARIABLE[[v]])))
guardar(ruta("e2", "a_mapas", "graficos", "MAPA_AAL_y_ABS_lado_a_lado.png"),
        gridExtra::arrangeGrob(grobs = mapas_pts, ncol = 2, top = "Distribución espacial de AAL y ABS (con contorno del departamento)"), 15, 7.5, 220, "paneles AAL-ABS")
guardar(ruta("e2", "a_mapas", "graficos", "MAPA_densidad_de_estaciones.png"),
  ggplot(datos, aes(Longitud, Latitud)) + stat_density_2d_filled(alpha = .85, bins = 8) + capa_contorno() + geom_point(size = .15, color = "black", alpha = .3) +
    coord_fixed(ratio = ratio_aspecto) + theme_minimal() + labs(title = "Densidad espacial de estaciones gravimétricas", fill = "Densidad"), 8, 7.5, 220, "densidad estaciones")
for (v in VARIABLES_PRINCIPALES) {
  guardar(ruta("e2", "a_mapas", "graficos", paste0("MAPA_puntos_por_clases_", v, ".png")),
    ggplot(transform(datos, Clase = cut(datos[[v]], quantile(datos[[v]], 0:N_CLASES_CUANTIL / N_CLASES_CUANTIL), include.lowest = TRUE, dig.lab = 4)),
           aes(Longitud, Latitud, color = Clase)) + capa_contorno() + geom_point(size = .7) + scale_color_manual(values = grDevices::colorRampPalette(PALETA_GEOFISICA)(N_CLASES_CUANTIL)) +
      guides(color = guide_legend(override.aes = list(size = 3))) + coord_fixed(ratio = ratio_aspecto) + theme_minimal() +
      labs(title = paste(v, "por clases (quintiles)"), color = "mGal"), 8, 7.5, 220, paste("puntos por clases", v))
}
perfiles <- do.call(rbind, lapply(VARIABLES_PRINCIPALES, function(v) rbind(
  data.frame(Variable = v, Coordenada = "Longitud (Oeste-Este)", x = datos$Longitud, y = datos[[v]]),
  data.frame(Variable = v, Coordenada = "Latitud (Sur-Norte)", x = datos$Latitud, y = datos[[v]]))))
guardar(ruta("e2", "a_mapas", "graficos", "GRAFICO_perfiles_por_coordenada.png"),
  ggplot(perfiles, aes(x, y)) + geom_point(alpha = .2, size = .4, color = "steelblue") + geom_smooth(method = "gam", color = "firebrick", se = FALSE) +
    facet_grid(Variable ~ Coordenada, scales = "free") + theme_minimal() + labs(title = "Variación de las anomalías con la longitud y la latitud", x = NULL, y = "mGal"), 11, 7, 220, "perfiles")

# ATÍPICOS ( NO LOS VAMOS A ELIMINAR)
cat("\n>>> 2b. Valores at\u00edpicos\n")
acc_atip <- list(); acc_atip_f <- list()
for (v in VARIABLES_PRINCIPALES) {
  at <- intentar(tabla_atipicos(datos, v), paste("at\u00edpicos", v))
  if (is.null(at)) next
  acc_atip[[v]] <- at$resumen
  write.csv(at$detalle, ruta("e2", "b_atipicos", "datos", paste0("atipicos_detalle_", v, ".csv")), row.names = FALSE)
  if (nrow(at$detalle) > 0) {
    acc_atip_f[[v]] <- data.frame(Variable = v, as.data.frame(table(Fuente = at$detalle$Fuente, Lado = at$detalle$Lado), stringsAsFactors = FALSE))
    guardar(ruta("e2", "b_atipicos", "graficos", paste0("MAPA_atipicos_", v, ".png")), grafico_atipicos(datos, v, at$detalle), 7, 7, 220, paste("mapa at\u00edpicos", v))
  }
  cat(" ", v, ": at\u00edpicos IQR =", at$resumen$N_por_debajo + at$resumen$N_por_encima, "(se conservan)\n")
}
if (!is.null(apilar(acc_atip))) write.csv(apilar(acc_atip), ruta("e2", "b_atipicos", "tablas", "TABLA_atipicos_IQR.csv"), row.names = FALSE)
if (!is.null(apilar(acc_atip_f))) write.csv(apilar(acc_atip_f), ruta("e2", "b_atipicos", "tablas", "TABLA_atipicos_por_fuente.csv"), row.names = FALSE)

# CORRELACIÓN CON LAS QUE SERÁN COVARIABLES (GRAVEDAD OBSERVADA Y ALTURA ELIPSOIDAL)

cat("\n>>> 2c. Correlaci\u00f3n\n")
Xc <- datos[, VARIABLES_TODAS]
cor_p <- cor(Xc, method = "pearson"); cor_s <- cor(Xc, method = "spearman")
write.csv(data.frame(Variable = rownames(cor_p), cor_p), ruta("e2", "c_correlacion", "tablas", "TABLA_correlacion_Pearson.csv"), row.names = FALSE)
write.csv(data.frame(Variable = rownames(cor_s), cor_s), ruta("e2", "c_correlacion", "tablas", "TABLA_correlacion_Spearman.csv"), row.names = FALSE)
cat("Correlaci\u00f3n de Pearson:\n"); print(round(cor_p, 3))

# COLINEALIDAD CON LAS QUE SERÁN COVARIABLES (GRAVEDAD OBSERVADA Y ALTURA ELIPSOIDAL)
t_colin <- do.call(rbind, lapply(VARIABLES_PRINCIPALES, function(v) {
  m <- lm(datos[[v]] ~ datos$Elipsoidal + datos$Grav_Obs)
  data.frame(Variable = v, R2_explicado_por_altura_y_gravedad = summary(m)$r.squared)
}))
write.csv(t_colin, ruta("e2", "c_correlacion", "tablas", "TABLA_colinealidad_con_covariables.csv"), row.names = FALSE)
cat("R2 de cada anomal\u00eda explicada por altura + gravedad (si es ~1 el cokriging puede ser inestable):\n"); print(t_colin, row.names = FALSE)
graf_cor <- function(m, titulo) {
  df <- as.data.frame(as.table(m)); names(df) <- c("V1", "V2", "r")
  ggplot(df, aes(V1, V2, fill = r)) + geom_tile(color = "white") +
    geom_text(aes(label = formatC(r, format = "f", digits = 3, decimal.mark = ",")), size = 4) +
    scale_fill_gradient2(low = "#2c7bb6", mid = "white", high = "#d7191c", limits = c(-1, 1), name = "r") +
    coord_fixed() + theme_minimal() + labs(title = titulo, x = NULL, y = NULL)
}
guardar(ruta("e2", "c_correlacion", "graficos", "GRAFICO_correlacion_Pearson.png"), graf_cor(cor_p, "Correlaci\u00f3n de Pearson"), 6.5, 5.5, 250, "corr Pearson")
guardar(ruta("e2", "c_correlacion", "graficos", "GRAFICO_correlacion_Spearman.png"), graf_cor(cor_s, "Correlaci\u00f3n de Spearman"), 6.5, 5.5, 250, "corr Spearman")
disp <- do.call(rbind, lapply(VARIABLES_PRINCIPALES, function(v) do.call(rbind, lapply(COVARIABLES_INFO, function(cv)
  data.frame(Anomalia = v, Covariable = cv, x = datos[[cv]], y = datos[[v]])))))
guardar(ruta("e2", "c_correlacion", "graficos", "GRAFICO_dispersion_anomalias_vs_covariables.png"),
  ggplot(disp, aes(x, y)) + geom_point(alpha = .25, size = .5, color = "steelblue") + geom_smooth(method = "lm", color = "firebrick", se = FALSE) +
    facet_grid(Anomalia ~ Covariable, scales = "free") + theme_minimal() + labs(title = "Anomal\u00edas frente a altura elipsoidal y gravedad observada", x = NULL, y = "mGal"),
  9, 6, 250, "dispersi\u00f3n")

# AUTOCORRELACIÓN ESPACIAL CON LOS TRES MÉTODOS DE ABAJO
acc_glob <- list(); acc_lisa <- list()
if (CALCULAR_AUTOCORRELACION) {
  cat("\n>>> 2d. Autocorrelaci\u00f3n espacial (Moran, Geary, LISA)\n") #AQUÍ
  for (v in VARIABLES_PRINCIPALES) {
    d <- datos[, c("Longitud", "Latitud", v)]
    mo <- intentar(test_moran(d, v, "kNN8"), paste("Moran", v))
    if (is.null(mo)) next
    ge <- intentar(geary_global(mo$datos_j, v, mo$lw), paste("Geary", v))
    li <- intentar(analisis_local(mo$datos_j, v, mo$lw), paste("LISA", v))
    acc_glob[[v]] <- data.frame(Variable = v, Matriz_W = mo$matriz_W_etiqueta, Moran_I = mo$moran_I, Esperado_I = mo$esperado,
                                p_Moran = mo$p_valor, Geary_C = if (is.null(ge)) NA else ge$C, p_Geary = if (is.null(ge)) NA else ge$p,
                                Interpretacion = mo$interpretacion)
    cat(" ", v, ": I de Moran =", round(mo$moran_I, 4), "(p =", signif(mo$p_valor, 3), ")\n")
    if (!is.null(li)) {
      write.csv(li, ruta("e2", "d_autocorrelacion", "datos", paste0("LISA_", v, ".csv")), row.names = FALSE)
      acc_lisa[[v]] <- resumen_lisa(li, v)
      guardar(ruta("e2", "d_autocorrelacion", "graficos", paste0("GRAFICO_dispersion_Moran_", v, ".png")), grafico_dispersion_moran(li, v, mo$moran_I), 7, 6, 200, paste("dispersograma", v))
      guardar(ruta("e2", "d_autocorrelacion", "graficos", paste0("MAPA_LISA_clusteres_", v, ".png")), mapa_lisa(li, v, mo$matriz_W_etiqueta), 8, 8, 220, paste("LISA", v))
      guardar(ruta("e2", "d_autocorrelacion", "graficos", paste0("MAPA_LISA_significancia_", v, ".png")), mapa_significancia_lisa(li, v, mo$matriz_W_etiqueta), 8, 8, 220, paste("LISA signif.", v))
    }
  }
  if (!is.null(apilar(acc_glob))) write.csv(apilar(acc_glob), ruta("e2", "d_autocorrelacion", "tablas", "TABLA_Moran_y_Geary_global.csv"), row.names = FALSE)
  if (!is.null(apilar(acc_lisa))) write.csv(apilar(acc_lisa), ruta("e2", "d_autocorrelacion", "tablas", "TABLA_LISA_resumen.csv"), row.names = FALSE)
}


# PRUEBAS DE NORMALIDAD ANTES Y DESPUÉS DE TRANSFORMARLAS - TRANSFORMACIONES

titulo_etapa("ETAPA 3: NORMALIDAD Y NORMALIZACI\u00d3N")
NZ <- list(); acc_antes <- list(); acc_despues <- list(); acc_resumen_norm <- list()
for (v in VARIABLES_PRINCIPALES) {
  cat("\n--- Variable:", v, "(", NOMBRES_VARIABLE[[v]], ") ---\n")
  x <- datos[[v]]
  #  ANTES
  t_antes <- pruebas_normalidad(x, v, "ANTES (datos originales)")
  acc_antes[[v]] <- t_antes
  write.csv(t_antes, ruta("e3", "a_antes", "tablas", paste0("TABLA_pruebas_normalidad_antes_", v, ".csv")), row.names = FALSE)
  guardar(ruta("e3", "a_antes", "graficos", paste0("GRAFICO_normalidad_antes_", v, ".png")), grafico_normalidad(x, paste("Normalidad de", v, "- datos originales")), 14, 4.8, 200, paste("normalidad antes", v))
  normal_antes <- all(t_antes$Es_normal == "SI")
  cat("Pruebas ANTES:\n"); print(t_antes[, c("Prueba", "Estadistico", "p_valor", "Es_normal")], digits = 4, row.names = FALSE)
  cat(" ->", v, if (normal_antes) "SI es normal (las 3 pruebas no rechazan H0): no se normaliza.\n"
      else "NO es normal (al menos una prueba rechaza H0): se normaliza con 2 m\u00e9todos.\n")
  
  if (normal_antes) {
    ident <- list(nombre = "Sin transformaci\u00f3n", id = "ninguna", fwd = function(v) v, inv = function(z) z, desc = "ya es normal")
    NZ[[v]] <- list(elegido = ident, z = x, normal_antes = TRUE, metodos = list(), motivo = "ya era normal")
    acc_resumen_norm[[v]] <- data.frame(Variable = v, Normal_antes = "SI", Metodo_elegido = "ninguno", Normal_despues = "SI", Motivo = "ya era normal")
    next
  }
  #  TRANSFORMACIONES PARA NORMALIZAR
  metodos <- list(intentar(norm_anamorfosis(x), paste("anamorfosis", v)), intentar(norm_yeojohnson(x), paste("Yeo-Johnson", v)))
  metodos <- Filter(Negate(is.null), metodos)
  names(metodos) <- vapply(metodos, function(m) m$id, character(1))
  t_desp <- list(); W <- numeric(0)
  for (id in names(metodos)) {
    m <- metodos[[id]]; z <- m$fwd(x); m$z <- z; metodos[[id]] <- m
    tt <- pruebas_normalidad(z, v, paste0("DESPU\u00c9S (", m$nombre, ")")); tt$Metodo <- m$nombre; t_desp[[id]] <- tt
    W[id] <- tt$Estadistico[tt$Prueba == "Shapiro-Wilk"]
    write.csv(tt, ruta("e3", "b_normalizacion_y_despues", "tablas", paste0("TABLA_pruebas_normalidad_despues_", id, "_", v, ".csv")), row.names = FALSE)
    guardar(ruta("e3", "b_normalizacion_y_despues", "graficos", paste0("GRAFICO_normalidad_despues_", id, "_", v, ".png")),
            grafico_normalidad(z, paste("Normalidad de", v, "-", m$nombre)), 14, 4.8, 200, paste("normalidad despu\u00e9s", id, v))
    guardar(ruta("e3", "b_normalizacion_y_despues", "graficos", paste0("GRAFICO_antes_vs_despues_", id, "_", v, ".png")),
            gridExtra::arrangeGrob(grafico_antes_despues(x, z, "Original", m$nombre), top = paste(v, ":", m$nombre)), 11, 8, 200, paste("antes/despu\u00e9s", id, v))
    write.csv(data.frame(Longitud = datos$Longitud, Latitud = datos$Latitud, Valor_original = x, Valor_normalizado = z),
              ruta("e3", "b_normalizacion_y_despues", "datos", paste0("datos_normalizados_", id, "_", v, ".csv")), row.names = FALSE)
    cat("  M\u00e9todo:", m$nombre, "|", m$desc, "\n")
  }
  t_desp_all <- apilar(t_desp)
  cat("Pruebas DESPU\u00c9S:\n"); print(t_desp_all[, c("Metodo", "Prueba", "Estadistico", "p_valor", "Es_normal")], digits = 4, row.names = FALSE)
  
  # AQUÍ SE ELIGE CON QUÉ MÉTODO SE VA A NORMALIZAR
  man <- NORMALIZACION_ELEGIDA[v]
  id_el <- if (!is.na(man) && man %in% names(metodos)) man else names(W)[which.max(W)]
  motivo <- if (!is.na(man) && man %in% names(metodos)) "elegido manualmente" else paste0("mayor W de Shapiro-Wilk (", paste(sprintf("%s: %.4f", names(W), W), collapse = "; "), ")")
  el <- metodos[[id_el]]
  normal_despues <- all(t_desp[[id_el]]$Es_normal == "SI")
  NZ[[v]] <- list(elegido = el, z = el$z, normal_antes = FALSE, metodos = metodos, motivo = motivo)
  cat(" -> M\u00e9todo elegido:", el$nombre, "-", motivo, "\n -> \u00bfNormal despu\u00e9s?", if (normal_despues) "SI" else "NO (ver nota abajo)", "\n")
  err_inv <- max(abs(el$inv(el$z) - x), na.rm = TRUE)
  acc_despues[[v]] <- t_desp_all
  # GRÁFICOS ANTES Y DESPUÉS
  guardar(ruta("e3", "b_normalizacion_y_despues", "graficos", paste0("GRAFICO_comparacion_de_metodos_", v, ".png")),
    gridExtra::arrangeGrob(grobs = c(lapply(metodos, function(m) ggplot(data.frame(z = m$z), aes(z)) +
        geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "steelblue", color = "white", alpha = .8) +
        stat_function(fun = dnorm, args = list(mean = mean(m$z), sd = sd(m$z)), color = "firebrick") + theme_minimal() + labs(title = m$nombre, x = NULL, y = NULL)),
      lapply(metodos, function(m) ggplot(data.frame(z = m$z), aes(sample = z)) + stat_qq(size = .5, alpha = .4, color = "steelblue") +
        stat_qq_line(color = "firebrick") + theme_minimal() + labs(title = paste("Q-Q:", m$nombre), x = NULL, y = NULL))), ncol = length(metodos)),
    11, 8, 200, paste("comparaci\u00f3n m\u00e9todos", v))
  acc_resumen_norm[[v]] <- data.frame(Variable = v, Normal_antes = "NO", Metodo_elegido = el$nombre,
    Normal_despues = if (normal_despues) "SI" else "NO", Shapiro_W_antes = t_antes$Estadistico[1], Shapiro_W_despues = W[id_el],
    Asimetria_antes = t_antes$Asimetria[1], Asimetria_despues = moments::skewness(el$z), Curtosis_antes = t_antes$Curtosis[1],
    Curtosis_despues = moments::kurtosis(el$z), Error_maximo_inversa = err_inv, Motivo = motivo)
  if (!normal_despues)
    cat("   Nota: ", v, " sigue sin pasar todas las pruebas. Con miles de datos las pruebas rechazan por diferencias m\u00ednimas; revise los gr\u00e1ficos Q-Q.\n   Si la variable es bimodal ninguna transformaci\u00f3n monot\u00f3nica suaviza del todo; la anamorfosis es la que m\u00e1s se acerca.\n", sep = "")
}
t_antes_all <- apilar(acc_antes); t_desp_todas <- apilar(acc_despues); t_resnorm <- apilar(acc_resumen_norm)
if (!is.null(t_antes_all)) write.csv(t_antes_all, ruta("e3", "c_resumen", "tablas", "TABLA_normalidad_ANTES.csv"), row.names = FALSE)
if (!is.null(t_desp_todas)) write.csv(t_desp_todas, ruta("e3", "c_resumen", "tablas", "TABLA_normalidad_DESPUES.csv"), row.names = FALSE)
if (!is.null(t_resnorm)) write.csv(t_resnorm, ruta("e3", "c_resumen", "tablas", "TABLA_RESUMEN_normalidad.csv"), row.names = FALSE)
if (!is.null(t_resnorm)) { cat("\n===== RESUMEN DE LA ETAPA 3 =====\n"); print(t_resnorm[, c("Variable", "Normal_antes", "Metodo_elegido", "Normal_despues")], row.names = FALSE) }


#  COMPARACIÓN DE LAS VARIABLES ANTES Y DESPUÉS DE SER TRANSFORMADAS
cat("\n>>> 3d. Comparación original vs normalizada\n")
acc_cmp <- list()
for (v in VARIABLES_PRINCIPALES) {
  x <- datos[[v]]; z <- NZ[[v]]$z
  fila <- function(valor, serie) {
    mo <- tryCatch(test_moran(data.frame(Longitud = datos$Longitud, Latitud = datos$Latitud, val = valor), "val", "kNN8")$moran_I, error = function(e) NA_real_)
    sw <- prueba_shapiro(valor)
    data.frame(Variable = v, Serie = serie, Media = mean(valor), Mediana = median(valor), Desv_estandar = sd(valor),
               Minimo = min(valor), Maximo = max(valor), Asimetria = moments::skewness(valor), Curtosis = moments::kurtosis(valor),
               Shapiro_W = sw$Estadistico, Shapiro_p = sw$p_valor, I_Moran_kNN8 = mo)
  }
  acc_cmp[[v]] <- rbind(fila(x, "Original"), fila(z, paste0("Normalizada (", NZ[[v]]$elegido$nombre, ")")))
  mk <- function(valor, tit, ley) ggplot(data.frame(Longitud = datos$Longitud, Latitud = datos$Latitud, val = valor)[order(valor), ],
      aes(Longitud, Latitud, color = val)) + geom_point(size = .7) + escala_grav_color(ley) + coord_fixed(ratio = ratio_aspecto) +
      theme_minimal() + labs(title = tit)
  guardar(ruta("e3", "d_comparacion_original_vs_normalizada", "graficos", paste0("COMPARACION_mapas_", v, ".png")),
          gridExtra::arrangeGrob(mk(x, paste(v, "original"), "mGal"), mk(z, paste(v, "normalizada"), "sin unidades"), ncol = 2,
                                 top = paste(v, ": distribución espacial antes y después de normalizar")), 14, 7.5, 200, paste("comparación mapas", v))
  guardar(ruta("e3", "d_comparacion_original_vs_normalizada", "graficos", paste0("COMPARACION_distribucion_", v, ".png")),
          gridExtra::arrangeGrob(grafico_normalidad(x, paste(v, "original")), grafico_normalidad(z, paste(v, "normalizada")), nrow = 2), 14, 9, 180, paste("comparación distribución", v))
}
t_cmp <- apilar(acc_cmp)
if (!is.null(t_cmp)) { write.csv(t_cmp, ruta("e3", "d_comparacion_original_vs_normalizada", "tablas", "TABLA_comparacion_original_vs_normalizada.csv"), row.names = FALSE)
                       print(t_cmp, digits = 4, row.names = FALSE) }


# ANÁLISIS DE TENDENCIA
titulo_etapa("ETAPA 4: TENDENCIA")
TR <- list(); acc_tend <- list(); acc_coef <- list()
for (v in VARIABLES_PRINCIPALES) {
  cat("\n--- Variable:", v, "---\n")
  z <- NZ[[v]]$z
  dd <- proyectar_km(jitter_duplicados(data.frame(Longitud = datos$Longitud, Latitud = datos$Latitud, valor = z)))
  dd$Xs <- as.numeric(scale(dd$X)); dd$Ys <- as.numeric(scale(dd$Y))
  formula_orden <- function(k) {
    t <- c("Xs", "Ys")
    if (k >= 2) t <- c(t, "I(Xs^2)", "I(Ys^2)", "I(Xs*Ys)")
    if (k >= 3) t <- c(t, "I(Xs^3)", "I(Ys^3)", "I(Xs^2*Ys)", "I(Xs*Ys^2)")
    as.formula(paste("valor ~", paste(t, collapse = " + ")))
  }
  mods <- lapply(1:ORDEN_MAXIMO, function(k) lm(formula_orden(k), data = dd))
  fp <- function(mod) { s <- summary(mod); unname(pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)) }
  r2a <- sapply(mods, function(m) summary(m)$adj.r.squared)
  hay_tend <- if (is.na(FORZAR_TENDENCIA)) isTRUE(fp(mods[[1]]) < ALFA_TENDENCIA && summary(mods[[1]])$r.squared >= UMBRAL_R2_TENDENCIA) else isTRUE(FORZAR_TENDENCIA)
  orden <- ORDEN_TENDENCIA[[v]]
  if (is.na(orden)) { orden <- 1; while (orden < ORDEN_MAXIMO && (r2a[orden + 1] - r2a[orden]) >= MEJORA_R2_ORDEN) orden <- orden + 1 }
  m1 <- mods[[orden]]
  nombres_orden <- c("1 (plano lineal)", "2 (cuadr\u00e1tica)", "3 (c\u00fabica)")[1:ORDEN_MAXIMO]
  t_tend <- data.frame(Variable = v, Orden = nombres_orden,
                       R2 = sapply(mods, function(m) summary(m)$r.squared), R2_ajustado = r2a,
                       Mejora_R2_ajust_vs_anterior = c(NA, diff(r2a)), p_F = sapply(mods, fp), AIC = sapply(mods, AIC),
                       Cor_con_X = c(cor(dd$valor, dd$X), rep(NA, ORDEN_MAXIMO - 1)), Cor_con_Y = c(cor(dd$valor, dd$Y), rep(NA, ORDEN_MAXIMO - 1)),
                       Hay_tendencia = c(if (hay_tend) "SI" else "NO", rep("", ORDEN_MAXIMO - 1)),
                       Orden_usado = seq_len(ORDEN_MAXIMO) == orden,
                       Criterio = c(paste0("p(F) < ", ALFA_TENDENCIA, " y R2 >= ", UMBRAL_R2_TENDENCIA), rep(paste0("orden: sube si R2 aj. mejora >= ", MEJORA_R2_ORDEN), ORDEN_MAXIMO - 1)))
  acc_tend[[v]] <- t_tend
  cf <- as.data.frame(summary(m1)$coefficients); cf <- data.frame(Variable = v, Termino = rownames(cf), cf, row.names = NULL)
  names(cf) <- c("Variable", "Termino", "Coeficiente", "Error_estandar", "t", "p_valor"); acc_coef[[v]] <- cf
  write.csv(t_tend, ruta("e4", "", "tablas", paste0("TABLA_tendencia_", v, ".csv")), row.names = FALSE)
  write.csv(cf, ruta("e4", "", "tablas", paste0("TABLA_coeficientes_tendencia_", v, ".csv")), row.names = FALSE)
  print(t_tend[, c("Orden", "R2", "R2_ajustado", "Mejora_R2_ajust_vs_anterior", "p_F", "AIC", "Orden_usado")], digits = 4, row.names = FALSE)
  cat(" ->", v, if (hay_tend) paste0(": HAY tendencia. Se elimina un polinomio de orden ", orden, " en X, Y; el variograma se construye sobre los RESIDUOS. Cokriging UNIVERSAL.\n")
      else ": NO hay tendencia significativa. Se usa la variable centrada en su media. Cokriging SIMPLE.\n")
  dd$tendencia <- fitted(m1)
  dd$residuo <- if (hay_tend) residuals(m1) else dd$valor - mean(dd$valor)
  for (k in 1:ORDEN_MAXIMO) dd[[paste0("residuo_orden", k)]] <- residuals(mods[[k]])
  write.csv(data.frame(Longitud = dd$Longitud, Latitud = dd$Latitud, X_km = dd$X, Y_km = dd$Y, Valor_normalizado = dd$valor,
                       Tendencia = dd$tendencia, Residuo = dd$residuo), ruta("e4", "", "datos", paste0("residuos_", v, ".csv")), row.names = FALSE)
  g1 <- ggplot(dd, aes(X, valor)) + geom_point(alpha = .2, size = .4, color = "steelblue") + geom_smooth(method = "lm", color = "firebrick") +
    theme_minimal() + labs(title = "Variable normalizada vs Este", x = "X (km)", y = "valor")
  g2 <- ggplot(dd, aes(Y, valor)) + geom_point(alpha = .2, size = .4, color = "steelblue") + geom_smooth(method = "lm", color = "firebrick") +
    theme_minimal() + labs(title = "Variable normalizada vs Norte", x = "Y (km)", y = "valor")
  g3 <- ggplot(dd[order(dd$tendencia), ], aes(Longitud, Latitud, color = tendencia)) + geom_point(size = .6) + escala_grav_color("tendencia") +
    coord_fixed(ratio = ratio_aspecto) + theme_minimal() + labs(title = paste0("Superficie de tendencia (orden ", orden, ")"))
  g4 <- ggplot(dd[order(abs(dd$residuo)), ], aes(Longitud, Latitud, color = residuo)) + geom_point(size = .6) +
    scale_color_gradient2(low = "#2c7bb6", mid = "white", high = "#d7191c", midpoint = 0) +
    coord_fixed(ratio = ratio_aspecto) + theme_minimal() + labs(title = "Mapa de residuos")
  guardar(ruta("e4", "", "graficos", paste0("GRAFICO_tendencia_", v, ".png")), gridExtra::arrangeGrob(g1, g2, g3, g4, ncol = 2), 12, 11, 180, paste("tendencia", v))
  guardar(ruta("e4", "", "graficos", paste0("GRAFICO_residuos_hist_caja_QQ_", v, ".png")), grafico_hist_caja_qq(dd$residuo, v), 13, 4.6, 220, paste("residuos", v))
  TR[[v]] <- list(dd = dd, hay = hay_tend, orden = orden)
}
t_tendc <- apilar(acc_tend)
if (!is.null(t_tendc)) write.csv(t_tendc, ruta("e4", "", "tablas", "TABLA_RESUMEN_tendencia.csv"), row.names = FALSE)
if (!is.null(apilar(acc_coef))) write.csv(apilar(acc_coef), ruta("e4", "", "tablas", "TABLA_coeficientes_tendencia.csv"), row.names = FALSE)


#  ANISOTROPÍA 
titulo_etapa("ETAPA 5: ANISOTROP\u00cdA")
AN <- list(); acc_aniso <- list()
for (v in VARIABLES_PRINCIPALES) {
  cat("\n--- Variable:", v, "---\n")
  ddr <- TR[[v]]$dd; ddr$valor <- ddr$residuo
  hull <- chull(ddr$X, ddr$Y); dmax <- max(dist(cbind(ddr$X, ddr$Y)[hull, ]))
  cutoff <- FRACCION_DISTANCIA_VARIOGRAMA * dmax; width <- cutoff / N_BINS_VARIOGRAMA
  cat("  Distancia m\u00e1xima:", round(dmax, 1), "km -> variograma hasta", round(cutoff, 1), "km (distancia / 2), en", N_BINS_VARIOGRAMA, "intervalos\n")
  sp_r <- ddr; coordinates(sp_r) <- ~X + Y
  vo <- variogram(valor ~ 1, sp_r, cutoff = cutoff, width = width)
  write.csv(as.data.frame(vo), ruta("e5", "", "tablas", paste0("variograma_omnidireccional_", v, ".csv")), row.names = FALSE)
  vd4 <- as.data.frame(variogram(valor ~ 1, sp_r, cutoff = cutoff, width = width, alpha = c(0, 45, 90, 135)))
  write.csv(vd4, ruta("e5", "", "tablas", paste0("variogramas_direccionales_", v, ".csv")), row.names = FALSE)
  guardar(ruta("e5", "", "graficos", paste0("GRAFICO_variogramas_direccionales_", v, ".png")),
    ggplot() + geom_line(data = vd4, aes(dist, gamma, color = factor(dir.hor)), linewidth = .8) + geom_point(data = vd4, aes(dist, gamma, color = factor(dir.hor)), size = 1.5) +
      geom_line(data = as.data.frame(vo), aes(dist, gamma), color = "black", linewidth = 1.1, linetype = "dashed") +
      scale_color_brewer(palette = "Set1", name = "Direcci\u00f3n (\u00b0 desde N)") + expand_limits(y = 0) + theme_minimal() +
      labs(title = paste("Variogramas direccionales de los residuos -", v), subtitle = "L\u00ednea negra discontinua: omnidireccional", x = "Distancia (km)", y = "Semivarianza"),
    9, 5.5, 200, paste("variogramas direccionales", v))
  vd8 <- as.data.frame(variogram(valor ~ 1, sp_r, cutoff = cutoff, width = width, alpha = seq(0, 157.5, by = 22.5), tol.hor = 11.25))
  g_ref <- 0.5 * max(vo$gamma)
  ros <- rosa_anisotropia(vd8, g_ref, cutoff)
  write.csv(ros, ruta("e5", "", "tablas", paste0("rosa_distancias_", v, ".csv")), row.names = FALSE)
  guardar(ruta("e5", "", "graficos", paste0("GRAFICO_rosa_anisotropia_", v, ".png")), grafico_rosa(ros, v, g_ref), 7, 7, 200, paste("rosa", v))
  an <- if (USAR_ANISOTROPIA) intentar(estimar_anisotropia(sp_r, valor ~ 1, cutoff, width), paste("anisotrop\u00eda", v)) else NULL
  aniso <- NULL
  if (!is.null(an)) {
    write.csv(an$tabla, ruta("e5", "", "tablas", paste0("busqueda_angulo_razon_", v, ".csv")), row.names = FALSE)
    acc_aniso[[v]] <- data.frame(Variable = v, Angulo_eje_mayor_desde_norte = an$angulo, Razon_menor_mayor = an$razon,
                                 SSErr_isotropico = an$sse_iso, SSErr_anisotropico = an$sse_aniso, Mejora_pct = 100 * an$mejora,
                                 Umbral_pct = 100 * ANISOTROPIA_MEJORA_MIN, Hay_anisotropia = if (an$adoptada) "SI" else "NO")
    cat("  Eje de mayor continuidad:", an$angulo, "\u00b0 desde el norte | raz\u00f3n menor/mayor:", an$razon, "| mejora del ajuste:", round(100 * an$mejora, 1), "% ->",
        if (an$adoptada) "HAY anisotrop\u00eda: se corrige\n" else "NO se corrige (mejora insuficiente): modelo is\u00f3tropo\n")
    guardar(ruta("e5", "", "graficos", paste0("GRAFICO_busqueda_anisotropia_", v, ".png")),
      ggplot(an$tabla, aes(angulo, razon, color = SSErr)) + geom_point(size = 3) + scale_color_viridis_c(direction = -1) + theme_minimal() +
        geom_point(data = data.frame(angulo = an$angulo, razon = an$razon), shape = 4, size = 5, color = "red", stroke = 1.5) +
        labs(title = paste("B\u00fasqueda del \u00e1ngulo y la raz\u00f3n de anisotrop\u00eda -", v), subtitle = "Menor error = mejor ajuste (cruz roja = mejor)", x = "\u00c1ngulo del eje mayor (\u00b0 desde N)", y = "Raz\u00f3n menor/mayor"),
      8, 5.5, 200, paste("b\u00fasqueda anisotrop\u00eda", v))
    if (an$adoptada) aniso <- list(angulo = an$angulo, razon = an$razon)
  }
  AN[[v]] <- list(aniso = aniso, dmax = dmax, cutoff = cutoff, width = width)
}
t_an <- apilar(acc_aniso)
if (!is.null(t_an)) write.csv(t_an, ruta("e5", "", "tablas", "TABLA_RESUMEN_anisotropia.csv"), row.names = FALSE)


# VARIOGRAMA SOBRE LOS RESIDUOS
titulo_etapa("ETAPA 6: VARIOGRAMA")
acc_vgm <- list(); acc_modelos <- list(); acc_param <- list()
for (v in VARIABLES_PRINCIPALES) {
  cat("\n--- Variable:", v, "---\n")
  ddr <- TR[[v]]$dd; ddr$valor <- ddr$residuo; hay_tend <- TR[[v]]$hay
  aniso <- AN[[v]]$aniso; cutoff <- AN[[v]]$cutoff; width <- AN[[v]]$width; dmax <- AN[[v]]$dmax
  ddv <- coords_trabajo(ddr, aniso); sp_v <- ddv; coordinates(sp_v) <- ~U + V
  v_emp <- variogram(valor ~ 1, sp_v, cutoff = cutoff, width = width)
  modelos <- comparar_modelos(v_emp)
  mod <- ajustar_modelo_teorico(v_emp)
  if (!is.null(modelos)) { modelos <- data.frame(Variable = v, modelos); acc_modelos[[v]] <- modelos
    write.csv(modelos, ruta("e6", "", "tablas", paste0("modelos_comparados_", v, ".csv")), row.names = FALSE); print(modelos, digits = 4, row.names = FALSE) }
  nug <- sum(mod$psill[mod$model == "Nug"]); sil_p <- sum(mod$psill[mod$model != "Nug"]); rng <- mod$range[mod$model != "Nug"][1]
  mod_nombre <- as.character(mod$model[mod$model != "Nug"][1])
  acc_vgm[[v]] <- data.frame(Variable = v, Escala = paste0("normalizada (", NZ[[v]]$elegido$nombre, ")"),
    Base_del_variograma = if (hay_tend) paste0("residuos de la tendencia polin\u00f3mica de orden ", TR[[v]]$orden) else "variable centrada (sin tendencia)",
    Anisotropia_corregida = !is.null(aniso), Modelo = mod_nombre, Pepita = round(nug, 5), Meseta_parcial = round(sil_p, 5),
    Meseta_total = round(nug + sil_p, 5), Rango_km = round(rng, 2), Rango_practico_km = round(rango_practico(mod_nombre, rng), 2), Pepita_sobre_meseta_pct = round(100 * nug / (nug + sil_p), 2),
    Distancia_maxima_km = round(dmax, 1), Corte_variograma_km = round(cutoff, 1), Intervalos = N_BINS_VARIOGRAMA, Pares_totales = sum(v_emp$np))
  write.csv(acc_vgm[[v]], ruta("e6", "", "tablas", paste0("parametros_variograma_", v, ".csv")), row.names = FALSE)
  write.csv(as.data.frame(v_emp), ruta("e6", "", "tablas", paste0("variograma_empirico_", v, ".csv")), row.names = FALSE)
  print(t(acc_vgm[[v]]))
  guardar(ruta("e6", "", "graficos", paste0("GRAFICO_variograma_ajustado_", v, ".png")),
    grafico_variograma_ajustado(v_emp, mod, paste("Variograma de los residuos -", v),
      paste0("Modelo ", mod_nombre, " | pepita ", round(nug, 3), " | meseta ", round(nug + sil_p, 3), " | rango ", round(rng, 1), " km | hasta ", round(cutoff, 1), " km (distancia/2)",
             if (!is.null(aniso)) " | anisotrop\u00eda corregida" else "")), 9, 6, 220, paste("variograma", v))
  intentar({
    dtr <- TR[[v]]$dd; nm <- c("Sin quitar tendencia (variable centrada)", paste0("Residuos orden ", 1:ORDEN_MAXIMO))
    dtr$res0 <- dtr$valor - mean(dtr$valor)
    series <- lapply(0:ORDEN_MAXIMO, function(k) { tmp <- dtr; tmp$valor <- if (k == 0) tmp$res0 else tmp[[paste0("residuo_orden", k)]]; coordinates(tmp) <- ~X + Y
      r <- as.data.frame(variogram(valor ~ 1, tmp, cutoff = cutoff, width = width)); r$Serie <- nm[k + 1]; r })
    vv <- do.call(rbind, series); vv$Serie <- factor(vv$Serie, levels = nm)
    write.csv(vv, ruta("e6", "", "tablas", paste0("variogramas_segun_orden_de_tendencia_", v, ".csv")), row.names = FALSE)
    ggsave(ruta("e6", "", "graficos", paste0("GRAFICO_efecto_de_remover_la_tendencia_", v, ".png")),
      ggplot(vv, aes(dist, gamma, color = Serie)) + geom_line(linewidth = .9) + geom_point(size = 1.8) + expand_limits(y = 0) + theme_minimal() +
        labs(title = paste("Efecto de remover la tendencia sobre el variograma -", v), subtitle = paste0("Orden usado: ", TR[[v]]$orden, ". Un variograma que se estabiliza (meseta) indica que la tendencia ya fue removida."),
             x = "Distancia (km)", y = "Semivarianza"), width = 10, height = 5.8, dpi = 200)
  }, paste("efecto de la tendencia", v))
  acc_param[[v]] <- data.frame(Variable = v, Transformacion = NZ[[v]]$elegido$id, Expresion = NZ[[v]]$elegido$desc, Hay_tendencia = hay_tend, Orden_tendencia = TR[[v]]$orden,
    Anisotropia_adoptada = !is.null(aniso), Angulo_eje_mayor = if (!is.null(aniso)) aniso$angulo else NA,
    Razon_menor_mayor = if (!is.null(aniso)) aniso$razon else NA, Modelo_variograma = mod_nombre,
    Pepita = nug, Meseta_parcial = sil_p, Rango_km = rng, Distancia_maxima_km = dmax, Corte_variograma_km = cutoff,
    Cokriging = if (hay_tend) "universal (con tendencia)" else "simple")
}
t_vgm <- apilar(acc_vgm); t_mod <- apilar(acc_modelos); t_par <- apilar(acc_param)
if (!is.null(t_vgm)) write.csv(t_vgm, ruta("e6", "", "tablas", "TABLA_RESUMEN_variogramas.csv"), row.names = FALSE)
if (!is.null(t_mod)) write.csv(t_mod, ruta("e6", "", "tablas", "TABLA_modelos_comparados.csv"), row.names = FALSE)
if (!is.null(t_par)) write.csv(t_par, ruta("e6", "", "tablas", "PARAMETROS_PARA_COKRIGING.csv"), row.names = FALSE)


# RESUMEN DE LOS RESULTADOS ANTERIORES

titulo_etapa("RESUMEN GENERAL")
if (requireNamespace("writexl", quietly = TRUE)) {
  hojas <- Filter(Negate(is.null), list(`E1_Descriptiva` = t_desc, `E1_Por_fuente` = t_desc_fuente, `E1_Frecuencias` = t_frec,
    `E2_Atipicos` = apilar(acc_atip), `E2_Pearson` = data.frame(Variable = rownames(cor_p), cor_p), `E2_Colinealidad` = t_colin,
    `E2_Moran_Geary` = apilar(acc_glob), `E2_LISA` = apilar(acc_lisa), `E3_Normalidad_ANTES` = t_antes_all, `E3_Normalidad_DESPUES` = t_desp_todas,
    `E3_Resumen` = t_resnorm, `E3_Comparacion` = t_cmp, `E4_Tendencia` = t_tendc, `E5_Anisotropia` = t_an, `E6_Modelos` = t_mod, `E6_Variogramas` = t_vgm))
  intentar(writexl::write_xlsx(hojas, ruta("resumen", "", "", "TABLAS.xlsx")), "Excel de tablas")
}
conclusiones <- c("CONCLUSIONES POR VARIABLE", strrep("=", 60), "")
for (v in VARIABLES_PRINCIPALES) {
  rn <- t_resnorm[t_resnorm$Variable == v, ]; hay <- TR[[v]]$hay; an_v <- AN[[v]]$aniso
  conclusiones <- c(conclusiones, paste0(v, " (", NOMBRES_VARIABLE[[v]], ")"),
    paste0("  1. Normalidad original: ", if (rn$Normal_antes == "SI") "SI es normal." else "NO es normal (Shapiro-Wilk, Jarque-Bera y Anderson-Darling)."),
    paste0("  2. Normalizaci\u00f3n: ", if (rn$Normal_antes == "SI") "no fue necesaria." else paste0("m\u00e9todo elegido = ", rn$Metodo_elegido, "; normal despu\u00e9s: ", rn$Normal_despues, ".")),
    paste0("  3. Tendencia: ", if (hay) paste0("HAY (se elimin\u00f3 un polinomio de orden ", TR[[v]]$orden, "; el variograma se construy\u00f3 sobre los residuos) -> cokriging universal.") else "NO hay -> cokriging simple."),
    paste0("  4. Anisotrop\u00eda: ", if (is.null(an_v)) "NO se corrige (modelo is\u00f3tropo)." else paste0("SE corrige (eje mayor ", an_v$angulo, "\u00b0 desde el norte, raz\u00f3n ", an_v$razon, ").")),
    paste0("  5. Variograma: modelo ", t_vgm$Modelo[t_vgm$Variable == v], ", par\u00e1metro de rango ", t_vgm$Rango_km[t_vgm$Variable == v], " km (rango pr\u00e1ctico ", t_vgm$Rango_practico_km[t_vgm$Variable == v], " km), hasta ", t_vgm$Corte_variograma_km[t_vgm$Variable == v], " km (distancia/2)."), "")
}
cat(conclusiones, sep = "\n")
intentar({ con <- file(ruta("resumen", "", "", "CONCLUSIONES.txt"), open = "wt", encoding = "UTF-8"); writeLines(conclusiones, con); close(con) }, "conclusiones")
leeme <- c("GU\u00cdA DE LA CARPETA DE RESULTADOS", strrep("=", 60),
  "Cada etapa tiene su carpeta; dentro: tablas, graficos/ (figuras) y datos/ (archivos por punto).", "",
  "  00_RESUMEN_GENERAL            : Excel con todas las tablas, CONCLUSIONES.txt e \u00edndice de archivos.",
  "  01_ETAPA_estadistica_descriptiva: estad\u00edsticos (con percentiles), por fuente, frecuencias, histogramas, cajas.",
  "  02_ETAPA_analisis_exploratorio : a_mapas | b_atipicos | c_correlacion | d_autocorrelacion (Moran, Geary, LISA).",
  "  03_ETAPA_normalidad            : a_antes (3 pruebas + gr\u00e1ficos) | b_normalizacion_y_despues (2 m\u00e9todos + 3 pruebas) | c_resumen | d_comparacion_original_vs_normalizada (tabla y mapas lado a lado).",
  "  04_ETAPA_tendencia             : \u00bfhay tendencia? coeficientes, superficie, residuos.",
  "  05_ETAPA_anisotropia           : variogramas direccionales, diagrama de rosa, \u00e1ngulo y raz\u00f3n.",
  "  06_ETAPA_variograma            : variograma de los residuos (distancia/2), modelos, PARAMETROS_PARA_COKRIGING.csv.", "",
  "ORDEN: descriptiva -> exploratorio -> normalidad -> tendencia -> anisotrop\u00eda -> variograma.  El cokriging se hace despu\u00e9s.")
intentar({ con <- file(file.path(CARPETA_SALIDA, "00_LEEME.txt"), open = "wt", encoding = "UTF-8"); writeLines(leeme, con); close(con) }, "LEEME")
arch <- list.files(CARPETA_SALIDA, recursive = TRUE)
intentar(write.csv(data.frame(Archivo = arch, Tamano_KB = round(file.size(file.path(CARPETA_SALIDA, arch)) / 1024, 1)),
                   ruta("resumen", "", "", "INDICE_DE_RESULTADOS.csv"), row.names = FALSE), "\u00edndice")
cat("\n\n================ ETAPAS 1 A 6 TERMINADAS ================\nResultados en:", normalizePath(CARPETA_SALIDA), "\n")


# CONFIGURACIÓN PARA INTERPOLAR

MODELOS_KU        <- c("Sph", "Exp", "Gau", "Mat")   
METODOS_AJUSTE    <- c(OLS = 6, WLS = 2, WLS1 = 7)    
RECORTE           <- 0.10                             # media recortada, del libro, revisar!!!!
MAX_PUNTOS_ESTIMADORES <- 3000                        
USAR_REML         <- FALSE
N_REML            <- 800
N_CUANTILES_RETRO <- 50
RESOLUCION_GRILLA_KU_KM      <- 1                     
DIST_MAX_EXTRAPOLACION_KU_KM <- 15                   


# SISTEMAS DE REFERENCIA (AQUÍ ESTÁ EL 4326 (EL DE ORIGEN NACIONAL) Y EL LIMITE DEPARTAMENTAL PARA LOS MAPAS

CRS_USADO <- tryCatch({ sf::st_transform(sf::st_sfc(sf::st_point(c(-74, 4.6)), crs = 4326), CRS_PLANO); CRS_PLANO },
                      error = function(e) 3116)
if (!exists("anillos")) anillos <- NULL
anillos_km <- if (is.null(anillos)) NULL else lapply(anillos, function(a) {
  p <- sf::st_transform(sf::st_as_sf(data.frame(x = a[, 1], y = a[, 2]), coords = c("x", "y"), crs = 4326), CRS_USADO)
  sf::st_coordinates(p) / 1000
})

#  MEDIANA Y MEDIA RECORTADA

estimadores_mediana_recortada <- function(x, y, z, cutoff, nbins, trim, max_pts) {
  n <- length(z)
  if (n > max_pts) { set.seed(2026); s <- sort(sample(n, max_pts)); x <- x[s]; y <- y[s]; z <- z[s]; n <- max_pts }
  w <- cutoff / nbins; L <- list()
  for (i0 in seq(1, n - 1, by = 250)) {
    ii <- i0:min(i0 + 249, n - 1)
    H <- sqrt(outer(x[ii], x, "-")^2 + outer(y[ii], y, "-")^2)
    sel <- outer(ii, seq_len(n), "<") & H > 0 & H <= cutoff
    if (!any(sel)) next
    D <- abs(outer(z[ii], z, "-"))
    L[[length(L) + 1]] <- cbind(b = pmin(floor(H[sel] / w) + 1, nbins), rd = sqrt(D[sel]), h = H[sel])
  }
  P <- do.call(rbind, L); b <- factor(P[, "b"], levels = seq_len(nbins))
  np <- as.numeric(table(b)); dist <- as.numeric(tapply(P[, "h"], b, mean))
  med <- as.numeric(tapply(P[, "rd"], b, median)); rec <- as.numeric(tapply(P[, "rd"], b, mean, trim = trim))
  den <- 0.914 + 0.988 / np
  armar <- function(g) {
    v <- data.frame(np = np, dist = dist, gamma = g^4 / den, dir.hor = 0, dir.ver = 0, id = factor("var1"))
    v <- v[np > 0 & is.finite(v$gamma), ]; class(v) <- c("gstatVariogram", "data.frame"); v
  }
  list(Mediana = armar(med), Media_recortada = armar(rec), n_usados = n)
}
ini_modelo <- function(ve, m) vgm(psill = max(ve$gamma) * 0.8, model = m, range = max(ve$dist) / 3,
                                  nugget = min(ve$gamma) * 0.5, kappa = KAPPA_MATERN)
fila_ajuste <- function(f, ve, est, m, met) {
  g <- variogramLine(f, dist_vector = ve$dist)$gamma
  data.frame(Estimador = est, Modelo = m, Ajuste = met, Pepita = sum(f$psill[f$model == "Nug"]),
             Meseta_parcial = sum(f$psill[f$model != "Nug"]), Rango_km = f$range[f$model != "Nug"][1],
             Rango_practico_km = rango_practico(m, f$range[f$model != "Nug"][1]),
             Kappa = if (m == "Mat") KAPPA_MATERN else NA_real_, MSE = mean((ve$gamma - g)^2),
             Clave = paste(est, m, met), stringsAsFactors = FALSE)
}
ajustar_todos <- function(ve, est) {
  filas <- list(); fits <- list()
  for (m in MODELOS_KU) for (met in names(METODOS_AJUSTE)) {
    f <- tryCatch(suppressWarnings(fit.variogram(ve, ini_modelo(ve, m), fit.method = METODOS_AJUSTE[[met]])), error = function(e) NULL)
    if (is.null(f) || isTRUE(attr(f, "singular")) || any(f$psill < 0) || sum(f$psill[f$model != "Nug"]) <= 1e-12 ||
        any(f$range[f$model != "Nug"] <= 0)) next
    if (m == "Gau" && sum(f$psill[f$model == "Nug"]) < 0.01 * sum(f$psill)) next
    clave <- paste(est, m, met); fits[[clave]] <- f; filas[[clave]] <- fila_ajuste(f, ve, est, m, met)
  }
  list(tabla = apilar(filas), fits = fits)
}
# VARIOGRAMAS EXPERIMENTALES

elegir_variograma <- function(df, col, cutoff, nb) {
  width <- cutoff / nb
  s <- data.frame(U = df$U, V = df$V, z = df[[col]]); coordinates(s) <- ~U + V
  ve <- list(Clasico = variogram(z ~ 1, s, cutoff = cutoff, width = width),
             Robusto = variogram(z ~ 1, s, cutoff = cutoff, width = width, cressie = TRUE))
  mr <- tryCatch(estimadores_mediana_recortada(df$U, df$V, df[[col]], cutoff, nb, RECORTE, MAX_PUNTOS_ESTIMADORES), error = function(e) NULL)
  if (!is.null(mr)) { ve$Mediana <- mr$Mediana; ve$Media_recortada <- mr$Media_recortada }
  ve <- Filter(function(x) nrow(x) >= 3 && max(x$gamma) > 1e-12, ve)   # p. ej. la mediana de indicadoras puede ser 0
  fits <- list(); tabs <- list()
  for (e in names(ve)) { aj <- ajustar_todos(ve[[e]], e); fits <- c(fits, aj$fits); tabs[[e]] <- aj$tabla }
  t <- apilar(tabs)
  if (is.null(t)) return(NULL)
  t$Elegido <- FALSE; t$Elegido[which.min(t$MSE)] <- TRUE
  list(ve = ve, tabla = t, modelo = fits[[t$Clave[t$Elegido]]], mejor = t[t$Elegido, ], fits = fits)
}

# PARA LA VALIDACIÓN CRUZADA

criterio_cv <- function(obs, pred, var) {
  ok <- is.finite(obs) & is.finite(pred) & is.finite(var); n_na <- sum(!ok)
  obs <- obs[ok]; pred <- pred[ok]; var <- var[ok]
  e <- pred - obs; s <- sqrt(pmax(var, 1e-12)); z <- e / s
  data.frame(MPE = mean(e), ASEPE = mean(s), RMSPE = sqrt(mean(e^2)), MSPE = mean(z), RMSSPE = sqrt(mean(z^2)),
             MAPPE = mean(abs(e / obs)), CCPE = cor(obs, pred), R2 = 1 - sum(e^2) / sum((obs - mean(obs))^2),
             pseudoR2 = cor(obs, pred)^2, N_sin_prediccion = n_na)
}
# PARA INDICADORES
indice_gamma <- function(obs, pred) {
  ok <- is.finite(obs) & is.finite(pred); obs <- obs[ok]; pred <- pred[ok]
  C <- sum(pred >= 0.5 & obs > 0) + sum(pred < 0.5 & obs < 1)
  D <- sum(pred >= 0.5 & obs < 1) + sum(pred < 0.5 & obs > 0)
  E <- sum(pred == 0.5)
  (C - D) / (C + D + E)
}
# Selección con los criterios (estos se mencionan también en el libro (por favor, revisarr!!!))
seleccionar <- function(t) {
  t$Rango_RMSPE  <- rank(t$RMSPE, ties.method = "min")
  t$Rango_RMSSPE <- rank(abs(t$RMSSPE - 1), ties.method = "min")
  t$Rango_MSPE   <- rank(abs(t$MSPE), ties.method = "min")
  t$Puntaje      <- 2 * t$Rango_RMSPE + 2 * t$Rango_RMSSPE + 1 * t$Rango_MSPE
  t$Dif_RMSPE_ASEPE <- abs(t$RMSPE - t$ASEPE)
  t$Elegido <- FALSE; t$Elegido[order(t$Puntaje, t$RMSPE)[1]] <- TRUE
  t
}
# PARA LA SELECCIÓN DEL KRIGING INDICADOR
  t$Rango_Gamma <- rank(-t$Gamma, ties.method = "min", na.last = "keep")
  t$Rango_RMSPE <- rank(t$RMSPE, ties.method = "min", na.last = "keep")
  t$Rango_R2    <- rank(-t$R2, ties.method = "min", na.last = "keep")
  t$Puntaje     <- 3 * t$Rango_Gamma + 2 * t$Rango_RMSPE + 1 * t$Rango_R2
  t$Elegido <- FALSE
  if (any(is.finite(t$Puntaje))) t$Elegido[order(t$Puntaje, t$RMSPE, na.last = TRUE)[1]] <- TRUE
  t
}
# PREDICCIÓN (EL UMBRAL, NO LOS DATOS) 
ku_retro <- function(pred, var, inv) {
  s <- sqrt(pmax(var, 0)); q <- qnorm((seq_len(N_CUANTILES_RETRO) - 0.5) / N_CUANTILES_RETRO)
  M <- matrix(inv(as.vector(outer(pred, rep(1, length(q))) + outer(s, q))), nrow = length(pred))
  list(media = rowMeans(M), sd = apply(M, 1, sd), p025 = inv(pred - 1.959964 * s), p975 = inv(pred + 1.959964 * s))
}

for (pq in c("ggspatial", "metR")) if (!requireNamespace(pq, quietly = TRUE)) try(install.packages(pq), silent = TRUE)
PALETA_INCERTIDUMBRE <- c("#FFFFE5", "#FFF7BC", "#FEE391", "#FEC44F", "#FE9929", "#EC7014", "#CC4C02", "#8C2D04")
PALETA_PROBABILIDAD  <- c("#F7FBFF", "#FFF5B0", "#FED976", "#FD8D3C", "#E31A1C", "#800026")
FUENTE_DATOS         <- "Datos: Ecopetrol e IGAC (Base Conjunta)"
MOSTRAR_ESTACIONES   <- TRUE
ETIQUETAR_ISOLINEAS  <- TRUE
AREA_ESTUDIO         <- "Departamento de Cundinamarca y Bogotá D.C."
NOMBRE_MODELO        <- c(Sph = "esférico", Exp = "exponencial", Gau = "gaussiano", Mat = "Matérn")
SRC_TEXTO            <- paste0("Sistema de referencia: MAGNA-SIRGAS / Origen Nacional (EPSG:", CRS_USADO, ")")

limites_mapa <- function() {
  if (!exists("LIMITE_DEPTOS_SF") || is.null(LIMITE_DEPTOS_SF)) return(NULL)
  lim <- sf::st_transform(sf::st_geometry(LIMITE_DEPTOS_SF), CRS_USADO)
  list(partes = lim, union = sf::st_union(lim))
}
intervalo_isolineas <- function(z) {
  paso <- diff(range(z, na.rm = TRUE)) / N_ISOLINEAS
  cand <- c(0.5, 1, 2, 2.5, 5, 10, 20, 25, 50, 100); cand[which.min(abs(cand - paso))]
}
coord_mapa <- function() {
  if (COORDENADAS_MAPA == "planas")
    coord_sf(crs = sf::st_crs(CRS_USADO), datum = sf::st_crs(CRS_USADO), expand = FALSE,
             label_axes = list(bottom = "E", top = "E", left = "N", right = "N"))
  else coord_sf(crs = sf::st_crs(CRS_USADO), datum = sf::st_crs(4326), expand = FALSE,
                label_axes = list(bottom = "E", top = "E", left = "N", right = "N"))
}
capas_cartograficas <- function(p, d, res_m, estaciones = MOSTRAR_ESTACIONES, norte_escala = TRUE) {
  if (any(d$ext)) p <- p + geom_tile(data = d[d$ext, ], aes(x, y), fill = "white", alpha = 0.55, width = res_m, height = res_m, inherit.aes = FALSE)
  if (estaciones) p <- p + geom_point(data = data.frame(x = dd0$X * 1000, y = dd0$Y * 1000), aes(x, y), size = 0.05, alpha = 0.25, inherit.aes = FALSE)
  lm_ <- limites_mapa()
  if (!is.null(lm_)) p <- p + geom_sf(data = lm_$partes, fill = NA, colour = "grey10", linewidth = 0.3, linetype = "dashed", inherit.aes = FALSE) +
                            geom_sf(data = lm_$union, fill = NA, colour = "black", linewidth = 0.9, inherit.aes = FALSE)
  p <- p + coord_mapa()
  if (norte_escala && requireNamespace("ggspatial", quietly = TRUE))
    p <- p + ggspatial::annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.7, bar_cols = c("black", "white"),
                                         pad_x = grid::unit(0.3, "cm"), pad_y = grid::unit(0.3, "cm")) +
      ggspatial::annotation_north_arrow(location = "tr", height = grid::unit(1.2, "cm"), width = grid::unit(1, "cm"),
                                        style = ggspatial::north_arrow_fancy_orienteering())
  p + theme_bw(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5), plot.subtitle = element_text(size = 9, hjust = 0.5),
          plot.caption = element_text(size = 7, hjust = 0, colour = "grey25"),
          panel.ontop = TRUE, panel.background = element_blank(), panel.border = element_rect(colour = "black", linewidth = 0.8),
          panel.grid = element_line(colour = "grey35", linewidth = 0.2),
          axis.text = element_text(size = 7), axis.text.y.left = element_text(angle = 90, hjust = 0.5),
          axis.text.y.right = element_text(angle = 90, hjust = 0.5),
          legend.title = element_text(size = 9, face = "bold", hjust = 0.5), legend.text = element_text(size = 7)) +
    labs(x = if (COORDENADAS_MAPA == "planas") "Este (m)" else "Longitud", y = if (COORDENADAS_MAPA == "planas") "Norte (m)" else "Latitud")
}

#ES EL RÓTULO QUE SE DEFINIÓ AL INICIO
cartucho <- function(titulo_mapa) {
  gp <- function(...) grid::gpar(...)
  grid::grobTree(
    grid::rectGrob(x = 0.75, width = 0.5, gp = gp(fill = "white", col = "black", lwd = 1.2)),
    grid::segmentsGrob(x0 = 0.5, x1 = 1, y0 = 0.64, y1 = 0.64, gp = gp(lwd = 0.8)),
    grid::segmentsGrob(x0 = 0.5, x1 = 1, y0 = 0.31, y1 = 0.31, gp = gp(lwd = 0.8)),
    grid::textGrob(INSTITUCION, x = 0.75, y = 0.82, gp = gp(fontsize = 9.5, fontface = "bold")),
    grid::textGrob(titulo_mapa, x = 0.75, y = 0.475, gp = gp(fontsize = 9.5, fontface = "bold")),
    grid::textGrob(AUTOR_MAPA, x = 0.75, y = 0.155, gp = gp(fontsize = 9, fontface = "italic")))
}

titulo_cartucho <- function(v, metodo = NULL, que = "ANOMAL\u00cdA") {
  base <- switch(v, ABS = paste0("MAPA DE ", que, " DE BOUGUER SIMPLE (densidad ", DENSIDAD_BOUGUER, ")"),
                    AAL = paste0("MAPA DE ", que, " DE AIRE LIBRE"), toupper(paste("MAPA DE", NOMBRES_VARIABLE[[v]])))
  paste(c(base, if (!is.null(metodo)) toupper(metodo)), collapse = "\n")
}
leyenda_grav <- function(v) switch(v, ABS = "A. Bouguer\n(miligales)", AAL = "A. Aire Libre\n(miligales)", paste0(v, "\n(mGal)"))
mapa_geofisico <- function(res, col, titulo, subtitulo = NULL, leyenda = "mGal", tipo = "anomalia", nota = NULL, titulo_mapa = NULL) {
  res_m <- RESOLUCION_GRILLA_KU_KM * 1000
  d <- data.frame(x = res$X * 1000, y = res$Y * 1000, z = res[[col]], ext = res$Extrapolacion)
  d <- d[is.finite(d$z), ]
  zin <- d$z[!d$ext]; if (length(zin) < 10) zin <- d$z
  escala <- switch(tipo,
    anomalia = {
      br <- unique(round(stats::quantile(zin, seq(0, 1, length.out = N_CLASES_COLOR + 1), na.rm = TRUE), 1))
      scale_fill_stepsn(name = leyenda, colours = grDevices::colorRampPalette(PALETA_GEOFISICA)(length(br) - 1),
                        breaks = br, limits = range(br), oob = scales::squish, labels = function(b) format(b, nsmall = 1, decimal.mark = ","),
                        guide = guide_coloursteps(even.steps = TRUE, show.limits = TRUE, barheight = grid::unit(13, "cm"),
                                                  barwidth = grid::unit(0.6, "cm"), frame.colour = "black"))
    },
    incertidumbre = scale_fill_gradientn(name = leyenda, colours = PALETA_INCERTIDUMBRE, limits = c(0, stats::quantile(zin, 0.99, na.rm = TRUE)),
                                         oob = scales::squish, breaks = scales::breaks_pretty(6),
                                         guide = guide_colourbar(barheight = grid::unit(9, "cm"), barwidth = grid::unit(0.5, "cm"), frame.colour = "black")),
    probabilidad  = scale_fill_gradientn(name = leyenda, colours = PALETA_PROBABILIDAD, limits = c(0, 1), breaks = seq(0, 1, 0.2),
                                         labels = function(b) format(b, decimal.mark = ","),
                                         guide = guide_colourbar(barheight = grid::unit(9, "cm"), barwidth = grid::unit(0.5, "cm"), frame.colour = "black")),
    diferencia    = { m <- max(abs(stats::quantile(zin, c(0.01, 0.99), na.rm = TRUE)))
                      scale_fill_gradient2(name = leyenda, low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                                           limits = c(-m, m), oob = scales::squish, breaks = scales::breaks_pretty(6),
                                           guide = guide_colourbar(barheight = grid::unit(9, "cm"), barwidth = grid::unit(0.5, "cm"), frame.colour = "black")) })
  p <- ggplot() + geom_tile(data = d, aes(x, y, fill = z), width = res_m, height = res_m) + escala
  pie <- c(SRC_TEXTO, FUENTE_DATOS)
  if (tipo == "anomalia") {
    iso <- intervalo_isolineas(zin)
    brc <- seq(floor(min(d$z) / iso) * iso, ceiling(max(d$z) / iso) * iso, by = iso)
    p <- p + geom_contour(data = d, aes(x, y, z = z), breaks = brc, colour = "black", linewidth = 0.15, alpha = 0.8)
    if (ETIQUETAR_ISOLINEAS && requireNamespace("metR", quietly = TRUE)) {
      p2 <- p + metR::geom_text_contour(data = d, aes(x, y, z = z), breaks = brc[seq(1, length(brc), by = 5)], size = 2, stroke = 0.15)
      if (isTRUE(tryCatch({ ggplot_build(p2); TRUE }, error = function(e) FALSE))) p <- p2
    }
    pie <- c(pie, paste0("Isoanómalas cada ", format(iso, decimal.mark = ","), " mGal"))
  }
  if (any(d$ext)) pie <- c(pie, paste0("Zona atenuada: a más de ", DIST_MAX_EXTRAPOLACION_KU_KM, " km de la estación más cercana (extrapolación)"))
  if (MOSTRAR_ESTACIONES) pie <- c(pie, "Puntos: estaciones gravimétricas")
  if (!is.null(nota)) pie <- c(pie, nota)
  p <- capas_cartograficas(p, d, res_m) +
    labs(title = titulo, subtitle = paste(c(subtitulo, AREA_ESTUDIO), collapse = "\n"),
         caption = paste(strwrap(paste(pie, collapse = " · "), 150), collapse = "\n"))
  if (is.null(titulo_mapa) || !INCLUIR_CARTUCHO) return(p)
  gridExtra::arrangeGrob(ggplotGrob(p), cartucho(titulo_mapa), ncol = 1, heights = c(1, 0.11))
}
mapa_facetas <- function(largo, titulo, subt, leyenda, tipo = "probabilidad") {
  res_m <- RESOLUCION_GRILLA_KU_KM * 1000
  d <- data.frame(x = largo$X * 1000, y = largo$Y * 1000, z = largo$Valor, ext = largo$Ext, Panel = largo$Panel)
  d <- d[is.finite(d$z), ]
  escala <- if (tipo == "probabilidad")
    scale_fill_gradientn(name = leyenda, colours = PALETA_PROBABILIDAD, limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = function(b) format(b, decimal.mark = ","))
  else scale_fill_gradientn(name = leyenda, colours = PALETA_INCERTIDUMBRE, limits = c(0, stats::quantile(d$z[!d$ext], 0.99, na.rm = TRUE)), oob = scales::squish)
  p <- ggplot() + geom_tile(data = d, aes(x, y, fill = z), width = res_m, height = res_m) + escala
  if (any(d$ext)) p <- p + geom_tile(data = d[d$ext, ], aes(x, y), fill = "white", alpha = 0.55, width = res_m, height = res_m)
  lm_ <- limites_mapa()
  if (!is.null(lm_)) p <- p + geom_sf(data = lm_$union, fill = NA, colour = "black", linewidth = 0.5, inherit.aes = FALSE)
  p + facet_wrap(~Panel) + coord_sf(crs = sf::st_crs(CRS_USADO), datum = sf::st_crs(4326), expand = FALSE) + theme_bw(base_size = 9) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.text = element_text(size = 6),
          strip.text = element_text(size = 8), plot.caption = element_text(size = 7, hjust = 0, colour = "grey25")) +
    labs(title = titulo, subtitle = paste(c(subt, AREA_ESTUDIO), collapse = "\n"), x = "Longitud", y = "Latitud",
         caption = paste(c(SRC_TEXTO, FUENTE_DATOS, paste0("Zona atenuada: extrapolación (más de ", DIST_MAX_EXTRAPOLACION_KU_KM, " km de una estación)")), collapse = " · "))
}
ku_mapa <- function(res, col, titulo, subt, leyenda, opcion = "plasma", limites = NULL) {
  tipo <- if (!is.null(limites) && isTRUE(all.equal(as.numeric(limites), c(0, 1)))) "probabilidad" else if (grepl("Varianza|Desv", col)) "incertidumbre" else "anomalia"
  mapa_geofisico(res, col, titulo, subt, leyenda, tipo)
}
corregir_orden <- function(P) {
  P <- pmin(pmax(P, 0), 1); K <- ncol(P)
  if (K == 1) return(P)
  up <- P; down <- P
  for (k in 2:K) up[, k] <- pmax(up[, k], up[, k - 1])
  for (k in (K - 1):1) down[, k] <- pmin(down[, k], down[, k + 1])
  (up + down) / 2
}

# REFERENCIA PARA LA PREDICCIÓN (O SEA, LA GRILLA)

titulo_etapa("PREPARACI\u00d3N: GRILLA DE PREDICCI\u00d3N")
dd0 <- TR[[VARIABLES_PRINCIPALES[1]]]$dd
if (!is.null(anillos_km)) { todo <- do.call(rbind, anillos_km); xr <- range(todo[, 1]); yr <- range(todo[, 2]) } else { xr <- range(dd0$X); yr <- range(dd0$Y) }
grilla_ku <- expand.grid(X = seq(floor(xr[1]), ceiling(xr[2]), by = RESOLUCION_GRILLA_KU_KM),
                         Y = seq(floor(yr[1]), ceiling(yr[2]), by = RESOLUCION_GRILLA_KU_KM))
if (!is.null(anillos_km)) {
  dentro <- rep(FALSE, nrow(grilla_ku))
  for (a in anillos_km) dentro <- dentro | sp::point.in.polygon(grilla_ku$X, grilla_ku$Y, a[, 1], a[, 2]) > 0
} else { h <- chull(dd0$X, dd0$Y); dentro <- sp::point.in.polygon(grilla_ku$X, grilla_ku$Y, dd0$X[h], dd0$Y[h]) > 0 }
grilla_ku <- grilla_ku[dentro, ]
dmin <- numeric(nrow(grilla_ku))
for (i in seq(1, nrow(grilla_ku), by = 500)) {
  j <- i:min(i + 499, nrow(grilla_ku))
  dmin[j] <- sqrt(apply(outer(grilla_ku$X[j], dd0$X, "-")^2 + outer(grilla_ku$Y[j], dd0$Y, "-")^2, 1, min))
}
grilla_ku$Dist_dato_km <- dmin; grilla_ku$Extrapolacion <- dmin > DIST_MAX_EXTRAPOLACION_KU_KM
ll <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(data.frame(x = grilla_ku$X * 1000, y = grilla_ku$Y * 1000),
                                                       coords = c("x", "y"), crs = CRS_USADO), 4326))
grilla_ku$Longitud <- ll[, 1]; grilla_ku$Latitud <- ll[, 2]
cat("Grilla:", nrow(grilla_ku), "celdas de", RESOLUCION_GRILLA_KU_KM, "km |", sum(grilla_ku$Extrapolacion), "en zona de extrapolaci\u00f3n\n")


#  KRIGING INDICADOR Y COKRIGING INDICADOR (Está en el libro y lo hablamos en la reuu)
UMBRALES_CUANTILES    <- c(0.10, 0.25, 0.50, 0.75, 0.90)
INCLUIR_LIMITES_IQR   <- TRUE                          
UMBRALES_MANUALES     <- list(AAL = NULL, ABS = NULL)   
VARIABLES_CATEGORICAS <- c("Fuente")                    
MIN_PUNTOS_CLASE      <- 10
NMAX_IND_CANDIDATOS   <- c(10, 20, 40)                  
NMAX_COKI             <- 16                              
CORREGIR_ORDEN        <- TRUE
USAR_ANISOTROPIA_IND  <- FALSE

ETAPAS["e6b"] <- "06b_ETAPA_kriging_y_cokriging_indicador"
if (!exists("HACER_ETAPA_6B")) HACER_ETAPA_6B <- TRUE
t_umb <- NULL; t_vind <- NULL; t_kin <- NULL; t_cmpi <- NULL; t_resi <- NULL; t_cat <- NULL
if (HACER_ETAPA_6B) {
titulo_etapa("ETAPA 6b: KRIGING INDICADOR Y COKRIGING INDICADOR")
base_ind <- dd0[, c("Longitud", "Latitud", "X", "Y")]
for (cc in unique(c(VARIABLES_PRINCIPALES, VARIABLES_CATEGORICAS, "ID"))) if (cc %in% names(datos)) base_ind[[cc]] <- datos[[cc]]
hull <- chull(base_ind$X, base_ind$Y); dmax_ind <- max(dist(cbind(base_ind$X, base_ind$Y)[hull, ]))
cutoff_ind <- FRACCION_DISTANCIA_VARIOGRAMA * dmax_ind; nb_ind <- N_BINS_VARIOGRAMA
coords_ind <- function(df, v) {
  if (USAR_ANISOTROPIA_IND && !is.null(AN[[v]]$aniso)) return(coords_trabajo(df, AN[[v]]$aniso))
  df$U <- df$X; df$V <- df$Y; df
}
set.seed(2026); FOLDS_IND <- sample(rep(seq_len(K_FOLDS_COMPARACION_IND), length.out = nrow(base_ind)))

umbrales_de <- function(x, v) {
  u <- data.frame(Etiqueta = paste0("P", round(100 * UMBRALES_CUANTILES)), Umbral = quantile(x, UMBRALES_CUANTILES, names = FALSE))
  if (INCLUIR_LIMITES_IQR) {
    q <- quantile(x, c(.25, .75), names = FALSE); iqr <- q[2] - q[1]
    u <- rbind(u, data.frame(Etiqueta = c("Lim_inf_IQR", "Lim_sup_IQR"), Umbral = c(q[1] - 1.5 * iqr, q[2] + 1.5 * iqr)))
  }
  if (!is.null(UMBRALES_MANUALES[[v]]))
    u <- rbind(u, data.frame(Etiqueta = paste0("U", UMBRALES_MANUALES[[v]]), Umbral = UMBRALES_MANUALES[[v]]))
  u <- u[order(u$Umbral), ]; u <- u[!duplicated(round(u$Umbral, 6)), ]
  n1 <- vapply(u$Umbral, function(t) sum(x <= t), numeric(1))
  u$N_unos <- n1; u$N_ceros <- length(x) - n1; u$Proporcion_unos <- n1 / length(x)
  u$Usado <- u$N_unos >= MIN_PUNTOS_CLASE & u$N_ceros >= MIN_PUNTOS_CLASE
  rownames(u) <- NULL; u
}

N_VECINOS_LOCALES_COKI <- 300
cv_coki <- function(g_fit, ids, n_loc = N_VECINOS_LOCALES_COKI) {
  d0 <- g_fit$data[[ids[1]]]$data; xy <- sp::coordinates(d0); n <- nrow(xy)
  P <- matrix(NA_real_, n, length(ids)); Vv <- P
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  for (i in seq_len(n)) {
    d2 <- (xy[, 1] - xy[i, 1])^2 + (xy[, 2] - xy[i, 2])^2; d2[i] <- Inf
    sel <- order(d2)[seq_len(min(n_loc, n - 1))]
    g2 <- g_fit
    for (id in ids) g2$data[[id]]$data <- g_fit$data[[id]]$data[sel, ]
    pr <- NULL
    invisible(utils::capture.output(pr <- tryCatch(as.data.frame(predict(g2, d0[i, ], debug.level = 0)), error = function(e) NULL)))
    if (!is.null(pr)) { P[i, ] <- unlist(pr[1, paste0(ids, ".pred")]); Vv[i, ] <- unlist(pr[1, paste0(ids, ".var")]) }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  list(pred = P, var = Vv)
}

acc_umb <- list(); acc_vind <- list(); acc_ki_nmax <- list(); acc_cmp_ind <- list(); acc_res_ind <- list(); KI <- list()
for (v in VARIABLES_PRINCIPALES) {
  cat("\n================ Variable:", v, "================\n")
  d <- coords_ind(base_ind, v); x <- d[[v]]
  um_t <- umbrales_de(x, v); acc_umb[[v]] <- data.frame(Variable = v, um_t)
  um <- um_t[um_t$Usado, ]; K <- nrow(um); ids <- paste0("I", seq_len(K))
  if (K == 0) { cat("  Ning\u00fan umbral utilizable.\n"); next }
  cat("  Umbrales (mGal):", paste0(um$Etiqueta, " = ", round(um$Umbral, 2), collapse = " | "), "\n")
  write.csv(acc_umb[[v]], ruta("e6b", v, "tablas", paste0("TABLA_umbrales_", v, ".csv")), row.names = FALSE)

#        INDICADORES  
  Iobs <- matrix(sapply(um$Umbral, function(t) as.numeric(x <= t)), ncol = K, dimnames = list(NULL, ids))
  for (k in seq_len(K)) d[[ids[k]]] <- Iobs[, k]
  cod <- data.frame(Longitud = datos$Longitud, Latitud = datos$Latitud, Valor_mGal = x, Iobs); names(cod)[-(1:3)] <- paste0("I_", um$Etiqueta)
  write.csv(cod, ruta("e6b", v, "datos", paste0("variables_indicadoras_", v, ".csv")), row.names = FALSE)
  pts <- do.call(rbind, lapply(seq_len(K), function(k) data.frame(X = d$X, Y = d$Y, Panel = paste0(um$Etiqueta[k], " (", round(um$Umbral[k], 1), " mGal)"),
                                                                  I = factor(Iobs[, k], levels = 0:1))))
  pts$Panel <- factor(pts$Panel, levels = unique(pts$Panel))
  guardar(ruta("e6b", v, "graficos", paste0("MAPA_variables_indicadoras_", v, ".png")),
    ggplot(pts, aes(X, Y, color = I)) + geom_point(size = .3) + facet_wrap(~Panel) + coord_equal() + theme_minimal() +
      scale_color_manual(values = c("0" = "grey75", "1" = "#d7191c"), labels = c("0: Z > Zc", "1: Z <= Zc"), name = NULL) +
      guides(color = guide_legend(override.aes = list(size = 3))) + theme(axis.text = element_text(size = 6)) +
      labs(title = paste("Variables indicadoras de", v), x = "Este (km)", y = "Norte (km)"), 11, 9, 200, paste("indicadoras", v))
  sp_d <- d[, c("U", "V", ids)]; coordinates(sp_d) <- ~U + V
  gi <- coords_ind(grilla_ku, v); sp_g <- gi[, c("U", "V")]; coordinates(sp_g) <- ~U + V

  #VARIOGRAMA PARA CADA UMBRAL
  modelos <- list(); nmax_el <- numeric(K); E <- list(); L <- list()
  for (k in seq_len(K)) {
    vk <- intentar(elegir_variograma(d, ids[k], cutoff_ind, nb_ind), paste("variograma indicador", v, um$Etiqueta[k]))
    if (is.null(vk)) { s0 <- data.frame(U = d$U, V = d$V, z = d[[ids[k]]]); coordinates(s0) <- ~U + V
      ve0 <- variogram(z ~ 1, s0, cutoff = cutoff_ind, width = cutoff_ind / nb_ind); modelos[[k]] <- ajustar_modelo_teorico(ve0); mejor <- NULL
    } else { modelos[[k]] <- vk$modelo; mejor <- vk$mejor
      write.csv(vk$tabla, ruta("e6b", v, "tablas", paste0("ajustes_variograma_indicador_", um$Etiqueta[k], "_", v, ".csv")), row.names = FALSE)
      et <- paste0(um$Etiqueta[k], " (", round(um$Umbral[k], 1), " mGal)")
      E[[k]] <- data.frame(as.data.frame(vk$ve[[mejor$Estimador]])[, c("dist", "gamma")], Panel = et)
      L[[k]] <- data.frame(variogramLine(vk$modelo, maxdist = cutoff_ind), Panel = et) }
    m <- modelos[[k]]
    acc_vind[[paste(v, k)]] <- data.frame(Variable = v, Umbral = um$Etiqueta[k], Valor_umbral_mGal = um$Umbral[k],
      Estimador = if (is.null(mejor)) "Clasico" else mejor$Estimador, Modelo = as.character(m$model[m$model != "Nug"][1]),
      Ajuste = if (is.null(mejor)) "WLS1" else mejor$Ajuste, MSE = if (is.null(mejor)) NA else mejor$MSE,
      Pepita = sum(m$psill[m$model == "Nug"]), Meseta_parcial = sum(m$psill[m$model != "Nug"]), Rango_km = m$range[m$model != "Nug"][1],
      Rango_practico_km = rango_practico(as.character(m$model[m$model != "Nug"][1]), m$range[m$model != "Nug"][1]))
#     VALIDACIÓN CRUZADA
    filas <- list()
    for (nm in NMAX_IND_CANDIDATOS) {
      cv <- intentar(as.data.frame(krige.cv(as.formula(paste(ids[k], "~ 1")), sp_d, model = m, nmax = nm, verbose = FALSE)),
                     paste("validaci\u00f3n KI", v, um$Etiqueta[k], "nmax", nm))
      if (is.null(cv)) next
      filas[[as.character(nm)]] <- data.frame(Variable = v, Umbral = um$Etiqueta[k], Vecinos = nm,
        criterio_cv(cv$observed, cv$var1.pred, cv$var1.var), Gamma = indice_gamma(cv$observed, cv$var1.pred),
        Pct_fuera_0_1 = 100 * mean(cv$var1.pred < 0 | cv$var1.pred > 1))
    }
    tn <- apilar(filas)
    if (!is.null(tn) && any(is.finite(tn$Gamma))) { tn <- seleccionar_indicador(tn); acc_ki_nmax[[paste(v, k)]] <- tn; nmax_el[k] <- tn$Vecinos[tn$Elegido] } else nmax_el[k] <- max(NMAX_IND_CANDIDATOS)
    cat("  ", um$Etiqueta[k], ": modelo", acc_vind[[paste(v, k)]]$Modelo, "| vecinos elegidos =", nmax_el[k],
        if (!is.null(tn)) paste("| Gamma =", round(tn$Gamma[tn$Elegido], 3), "| RMSPE =", round(tn$RMSPE[tn$Elegido], 3),
                                "| RMSSPE =", round(tn$RMSSPE[tn$Elegido], 3), "| sin predicción =", tn$N_sin_prediccion[tn$Elegido]) else "", "\n")
  }
  if (length(E) > 0) {
    EE <- do.call(rbind, E); LL <- do.call(rbind, L); EE$Panel <- factor(EE$Panel, levels = unique(EE$Panel)); LL$Panel <- factor(LL$Panel, levels = levels(EE$Panel))
    guardar(ruta("e6b", v, "graficos", paste0("GRAFICO_variogramas_indicadores_", v, ".png")),
      ggplot() + geom_point(data = EE, aes(dist, gamma), shape = 1) + geom_line(data = LL, aes(dist, gamma), color = "firebrick") +
        facet_wrap(~Panel, scales = "free_y") + expand_limits(y = 0) + theme_minimal() +
        labs(title = paste("Variogramas indicadores -", v), subtitle = "Puntos: estimador elegido; l\u00ednea: modelo de menor MSE", x = "Distancia (km)", y = "Semivarianza"),
      11, 8, 200, paste("variogramas indicadores", v))
  }

  # KRIGING INDICADOR EN LA GRILLA
  P_ki <- matrix(NA_real_, nrow(grilla_ku), K); V_ki <- P_ki
  for (k in seq_len(K)) {
    pr <- intentar(krige(as.formula(paste(ids[k], "~ 1")), sp_d, sp_g, model = modelos[[k]], nmax = nmax_el[k], debug.level = 0),
                   paste("kriging indicador", v, um$Etiqueta[k]))
    if (!is.null(pr)) { P_ki[, k] <- pr$var1.pred; V_ki[, k] <- pr$var1.var }
  }
  fuera_ki <- colSums(P_ki < 0 | P_ki > 1, na.rm = TRUE)
  P_ki_c <- if (CORREGIR_ORDEN) corregir_orden(P_ki) else pmin(pmax(P_ki, 0), 1)

  # COKRIGING INDICADOR
  P_co <- NULL; gf <- NULL; fuera_co <- rep(NA, K)
  if (K >= 2) {
    g <- NULL
    for (k in seq_len(K)) g <- gstat(g, id = ids[k], formula = as.formula(paste(ids[k], "~ 1")), data = sp_d, nmax = NMAX_COKI)
    vx <- intentar(variogram(g, cutoff = cutoff_ind, width = cutoff_ind / nb_ind), paste("variogramas cruzados", v))
    mref <- modelos[[ceiling(K / 2)]]; est <- mref[mref$model != "Nug", ][1, ]
    if (!is.null(vx)) {
      g <- gstat(g, model = vgm(psill = 0.1, model = as.character(est$model), range = est$range, nugget = 0.05, kappa = KAPPA_MATERN), fill.all = TRUE)
      gf <- intentar(fit.lmc(vx, g, fit.ranges = FALSE, correct.diagonal = 1.01), paste("LMC indicadores", v))
    }
    if (!is.null(gf)) {
      intentar({ png(ruta("e6b", v, "graficos", paste0("GRAFICO_LMC_cokriging_indicador_", v, ".png")), width = 3200, height = 3000, res = 220)
        print(plot(vx, model = gf, main = paste("Cokriging indicador: variogramas directos y cruzados -", v), xlab = "Distancia (km)")); dev.off() },
        paste("gr\u00e1fico LMC", v))
      pr <- intentar(as.data.frame(predict(gf, sp_g, debug.level = 0)), paste("cokriging indicador", v))
      if (!is.null(pr)) { P_co <- as.matrix(pr[, paste0(ids, ".pred"), drop = FALSE]); fuera_co <- colSums(P_co < 0 | P_co > 1, na.rm = TRUE)
        P_co <- if (CORREGIR_ORDEN) corregir_orden(P_co) else pmin(pmax(P_co, 0), 1) }
    }
  }

#     COMPARAMOS KRIGING Y COKRIGING INDICADORES
  cmp <- list()
  Pcv <- matrix(NA_real_, nrow(d), K); Vcv <- Pcv
  for (k in seq_len(K)) {
    cv <- intentar(as.data.frame(krige.cv(as.formula(paste(ids[k], "~ 1")), sp_d, model = modelos[[k]], nmax = nmax_el[k],
                                          verbose = FALSE)), paste("validaci\u00f3n KI", v, k))
    if (!is.null(cv)) { Pcv[, k] <- cv$var1.pred; Vcv[, k] <- cv$var1.var }
  }
  if (!is.null(gf)) cat("  Cokriging indicador, leave one out (ver barra de avance):\n")
  cvc <- if (!is.null(gf)) intentar(cv_coki(gf, ids), paste("validaci\u00f3n CoKI", v)) else NULL
  for (k in seq_len(K)) {
    if (any(is.finite(Pcv[, k]))) cmp[[paste("KI", k)]] <- data.frame(Variable = v, Metodo = "Kriging indicador", Umbral = um$Etiqueta[k],
      criterio_cv(Iobs[, k], Pcv[, k], Vcv[, k]), Gamma = indice_gamma(Iobs[, k], Pcv[, k]), Pct_fuera_0_1_grilla = 100 * fuera_ki[k] / nrow(grilla_ku))
    if (!is.null(cvc)) cmp[[paste("CoKI", k)]] <- data.frame(Variable = v, Metodo = "Cokriging indicador", Umbral = um$Etiqueta[k],
      criterio_cv(Iobs[, k], cvc$pred[, k], cvc$var[, k]), Gamma = indice_gamma(Iobs[, k], cvc$pred[, k]), Pct_fuera_0_1_grilla = 100 * fuera_co[k] / nrow(grilla_ku))
  }
  t_cmp <- apilar(cmp)
  if (!is.null(t_cmp)) {
    acc_cmp_ind[[v]] <- t_cmp

        ok_u <- is.finite(t_cmp$Gamma) & is.finite(t_cmp$RMSPE)
    comunes <- Reduce(intersect, split(t_cmp$Umbral[ok_u], t_cmp$Metodo[ok_u]))
    tc <- t_cmp[t_cmp$Umbral %in% comunes, ]
    ag <- aggregate(cbind(MPE, ASEPE, RMSPE, MSPE, RMSSPE, R2, Gamma, Pct_fuera_0_1_grilla) ~ Variable + Metodo, data = tc, FUN = mean)
    ag$Umbrales_comparados <- paste(comunes, collapse = ", ")
    ag <- seleccionar_indicador(ag); acc_res_ind[[v]] <- ag
    cat("\n  Comparaci\u00f3n (promedio de los umbrales", paste(comunes, collapse = ", "), "| validaci\u00f3n leave one out):\n")
    print(ag[, c("Metodo", "RMSPE", "ASEPE", "MSPE", "RMSSPE", "R2", "Gamma", "Elegido")], digits = 4, row.names = FALSE)
    cat("  -> M\u00e1s adecuado para", v, ":", ag$Metodo[ag$Elegido], "\n")
  }

  #   GRÁFICOS
  etq <- paste0("P(", v, " > ", round(um$Umbral, 1), ") [", um$Etiqueta, "]")
  for (met in c("KI", "CoKI")) {
    P <- if (met == "KI") P_ki_c else P_co
    if (is.null(P)) next
    nombre <- if (met == "KI") "Kriging indicador" else "Cokriging indicador"
    sal <- data.frame(grilla_ku[, c("Longitud", "Latitud", "X", "Y", "Extrapolacion")])
    for (k in seq_len(K)) { sal[[paste0("P_no_supera_", um$Etiqueta[k])]] <- P[, k]; sal[[paste0("P_supera_", um$Etiqueta[k])]] <- 1 - P[, k] }
    if (met == "KI") for (k in seq_len(K)) sal[[paste0("Varianza_", um$Etiqueta[k])]] <- V_ki[, k]
    kl <- which(um$Etiqueta == "Lim_inf_IQR"); kh <- which(um$Etiqueta == "Lim_sup_IQR")
    if (length(kl) + length(kh) > 0) sal$P_atipico <- (if (length(kl)) P[, kl] else 0) + (if (length(kh)) 1 - P[, kh] else 0)
    write.csv(sal, ruta("e6b", v, "datos", paste0("probabilidades_", met, "_", v, ".csv")), row.names = FALSE)
    lg <- do.call(rbind, lapply(seq_len(K), function(k) data.frame(X = sal$X, Y = sal$Y, Panel = etq[k], Valor = 1 - P[, k], Ext = sal$Extrapolacion)))
    lg$Panel <- factor(lg$Panel, levels = etq)
    guardar(ruta("e6b", v, "graficos", paste0("MAPA_probabilidad_superar_umbral_", met, "_", v, ".png")),
            mapa_facetas(lg, paste(nombre, "-", NOMBRES_VARIABLE[[v]]), "Probabilidad de que la anomal\u00eda supere cada umbral", "Probabilidad"), 12, 10, 220, paste("mapa", met, v))
    if (met == "KI") {
      lv <- do.call(rbind, lapply(seq_len(K), function(k) data.frame(X = sal$X, Y = sal$Y, Panel = etq[k], Valor = V_ki[, k], Ext = sal$Extrapolacion)))
      lv$Panel <- factor(lv$Panel, levels = etq)
      guardar(ruta("e6b", v, "graficos", paste0("MAPA_varianza_error_KI_", v, ".png")),
              mapa_facetas(lv, paste("Kriging indicador: varianza del error -", NOMBRES_VARIABLE[[v]]), NULL, "Varianza", "incertidumbre"), 12, 10, 220, paste("varianza KI", v))
    }
    if (!is.null(sal$P_atipico))
      guardar(ruta("e6b", v, "graficos", paste0("MAPA_probabilidad_atipico_", met, "_", v, ".png")),
              ku_mapa(transform(sal, Valor = P_atipico), "Valor", paste(nombre, "- probabilidad de valor at\u00edpico -", v),
                      "Fuera de los l\u00edmites Q1 - 1,5 IQR y Q3 + 1,5 IQR", "Probabilidad", "plasma", c(0, 1)), 8.5, 8.5, 220, paste("at\u00edpico", met, v))
  }
  KI[[v]] <- list(umbrales = um, P_ki = P_ki_c, V_ki = V_ki, P_coki = P_co, modelos = modelos, nmax = nmax_el, lmc = gf)
}

# VARIABLES CATEGÓRICAS
acc_cat <- list()
for (cv_ in intersect(VARIABLES_CATEGORICAS, names(base_ind))) {
  cat("\n--- Variable categ\u00f3rica:", cv_, "---\n")
  dc <- base_ind[!is.na(base_ind[[cv_]]), ]; dc$U <- dc$X; dc$V <- dc$Y
  cats <- sort(unique(as.character(dc[[cv_]])))
  if (length(cats) < 2 || length(cats) > 15) { cat("  Se omite (", length(cats), "categor\u00edas)\n"); next }
  B <- sapply(cats, function(cc) as.numeric(as.character(dc[[cv_]]) == cc)); colnames(B) <- paste0("B", seq_along(cats))
  for (k in seq_along(cats)) dc[[paste0("B", k)]] <- B[, k]
  tab <- data.frame(Variable = cv_, Categoria = cats, Codigo = paste0(cv_, "_", make.names(cats)), N = colSums(B), Proporcion = colMeans(B))
  acc_cat[[cv_]] <- tab; print(tab, row.names = FALSE)
  bin <- data.frame(Longitud = dc$Longitud, Latitud = dc$Latitud, Categoria = dc[[cv_]], B); names(bin)[-(1:3)] <- tab$Codigo
  write.csv(bin, ruta("e6b", paste0("categorica_", cv_), "datos", paste0("variables_binarias_", cv_, ".csv")), row.names = FALSE)
  spc <- dc[, c("U", "V", colnames(B))]; coordinates(spc) <- ~U + V
  spg <- data.frame(U = grilla_ku$X, V = grilla_ku$Y); coordinates(spg) <- ~U + V
  Pc <- matrix(NA_real_, nrow(grilla_ku), length(cats))
  for (k in seq_along(cats)) {
    if (tab$N[k] < MIN_PUNTOS_CLASE || tab$N[k] > nrow(dc) - MIN_PUNTOS_CLASE) next
    vk <- intentar(elegir_variograma(dc, paste0("B", k), cutoff_ind, nb_ind), paste("variograma de", cats[k]))
    if (is.null(vk)) next
    pr <- intentar(krige(as.formula(paste0("B", k, " ~ 1")), spc, spg, model = vk$modelo, nmax = max(NMAX_IND_CANDIDATOS), debug.level = 0),
                   paste("kriging indicador de", cats[k]))
    if (!is.null(pr)) Pc[, k] <- pr$var1.pred
  }
  Pc <- pmin(pmax(Pc, 0), 1); s <- rowSums(Pc, na.rm = TRUE); Pc <- Pc / ifelse(s > 0, s, NA)
  sal <- data.frame(grilla_ku[, c("Longitud", "Latitud", "X", "Y", "Extrapolacion")], Pc); names(sal)[-(1:5)] <- paste0("P_", make.names(cats))
  write.csv(sal, ruta("e6b", paste0("categorica_", cv_), "datos", paste0("probabilidad_categorias_", cv_, ".csv")), row.names = FALSE)
  lg <- do.call(rbind, lapply(seq_along(cats), function(k) data.frame(X = sal$X, Y = sal$Y, Panel = paste("P(", cv_, "=", cats[k], ")"),
                                                                      Valor = Pc[, k], Ext = sal$Extrapolacion)))
  guardar(ruta("e6b", paste0("categorica_", cv_), "graficos", paste0("MAPA_probabilidad_categorias_", cv_, ".png")),
          mapa_facetas(lg, paste("Kriging indicador de la variable categ\u00f3rica", cv_), "Probabilidad de cada categor\u00eda", "Probabilidad"), 11, 6, 220, paste("categ\u00f3rica", cv_))
}

t_umb <- apilar(acc_umb); t_vind <- apilar(acc_vind); t_kin <- apilar(acc_ki_nmax); t_cmpi <- apilar(acc_cmp_ind); t_resi <- apilar(acc_res_ind); t_cat <- apilar(acc_cat)
if (!is.null(t_vind)) write.csv(t_vind, ruta("e6b", "", "tablas", "TABLA_variogramas_indicadores.csv"), row.names = FALSE)
if (!is.null(t_kin))  write.csv(t_kin,  ruta("e6b", "", "tablas", "TABLA_KI_validacion_LOO_por_vecindario.csv"), row.names = FALSE)
if (!is.null(t_cmpi)) write.csv(t_cmpi, ruta("e6b", "", "tablas", "TABLA_KI_vs_CoKI_por_umbral.csv"), row.names = FALSE)
if (!is.null(t_resi)) write.csv(t_resi, ruta("e6b", "", "tablas", "TABLA_COMPARACION_KI_vs_CoKI.csv"), row.names = FALSE)
if (requireNamespace("writexl", quietly = TRUE)) {
  hojas <- Filter(Negate(is.null), list(Umbrales = t_umb, Variogramas = t_vind, KI_vecindarios_LOO = t_kin,
                                        KI_vs_CoKI_umbral = t_cmpi, Comparacion = t_resi, Categoricas = t_cat))
  if (length(hojas) > 0) intentar(writexl::write_xlsx(hojas, ruta("e6b", "", "", "TABLAS_KRIGING_INDICADOR.xlsx")), "Excel KI")
}
cat("\n================ ETAPA 6b TERMINADA ================\n")
} else cat("\n================ ETAPA 6b OMITIDA (HACER_ETAPA_6B = FALSE): se conservan los resultados ya guardados ================\n")



# KRIGING UNIVERSAL
ORDENES_KU      <- NULL           
NMAX_CANDIDATOS <- c(20, 40)      
INCLUIR_MATERN_ETAPA6 <- TRUE     
FORZAR_MATERN_ETAPA6  <- FALSE    
ESCALA_KU       <- "normalizada" 
omax_de    <- function(nm) if (BUSQUEDA_CUADRANTES) ceiling(nm / 4) else 0
maxdist_de <- function(v) if (BUSQUEDA_CUADRANTES) AN[[v]]$cutoff else Inf

ETAPAS["e7"] <- "07_ETAPA_kriging_universal"
titulo_etapa("ETAPA 7: KRIGING UNIVERSAL")
terminos_ku <- function(k) {
  if (k == 0) return("1")
  t <- c("Xs", "Ys")
  if (k >= 2) t <- c(t, "I(Xs*Ys)", "I(Xs^2)", "I(Ys^2)")
  if (k >= 3) t <- c(t, "I(Xs^3)", "I(Ys^3)", "I(Xs^2*Ys)", "I(Xs*Ys^2)")
  paste(t, collapse = " + ")
}
nombre_orden <- function(k) c("0 (sin tendencia: kriging ordinario)", "1 (z ~ x + y)", "2 (z ~ x + y + xy + x² + y²)", "3 (cúbica)")[k + 1]
orden_etapa4 <- function(v) if (isTRUE(TR[[v]]$hay)) TR[[v]]$orden else 0
tendencia_global <- function(d, z, k) {
  dd_ <- data.frame(z = z, Xs = d$Xs, Ys = d$Ys)
  reg <- lm(as.formula(paste("z ~", terminos_ku(k))), data = dd_)
  list(reg = reg, tend = fitted(reg), res = residuals(reg))
}

KU <- list(); NMAX_KU_ELEGIDO <- setNames(rep(NA_real_, length(VARIABLES_PRINCIPALES)), VARIABLES_PRINCIPALES)
acc_ajustes <- list(); acc_crit <- list(); acc_elegido <- list()


#  INTERPOLACIÓN POR VARIABLE
for (v in VARIABLES_PRINCIPALES) {
  cat("\n\n================ Variable:", v, "(", NOMBRES_VARIABLE[[v]], ") ================\n")
  dd <- TR[[v]]$dd
  if (ESCALA_KU == "original") dd$valor <- datos[[v]]
  inv <- if (ESCALA_KU == "normalizada") NZ[[v]]$elegido$inv else function(z) z
  unidad <- if (ESCALA_KU == "normalizada") paste0("escala normalizada (", NZ[[v]]$elegido$nombre, ")") else "mGal"
  aniso <- AN[[v]]$aniso; cutoff <- AN[[v]]$cutoff; nb <- N_BINS_VARIOGRAMA; width <- cutoff / nb
  d <- coords_trabajo(dd, aniso); d$orig <- datos[[v]]
  ordenes <- if (is.null(ORDENES_KU)) orden_etapa4(v) else ORDENES_KU
  cand <- list(); info_orden <- list(); cv_guardadas <- list()

  for (k in ordenes) {
    cat("\n--- Tendencia de orden", nombre_orden(k), if (k == orden_etapa4(v)) "(la que indicó la etapa 4)" else "", "---\n")
    sub <- paste0("orden_", k)
    tg <- tendencia_global(d, d$valor, k); d$tend <- tg$tend; d$res <- tg$res
    cat("  Tendencia global: R2 =", round(summary(tg$reg)$r.squared, 4), "\n")
    sp_r <- d[, c("U", "V", "res")]; coordinates(sp_r) <- ~U + V

    # VARIOGRAMAS
    ve <- list(Clasico = variogram(res ~ 1, sp_r, cutoff = cutoff, width = width),
               Robusto = variogram(res ~ 1, sp_r, cutoff = cutoff, width = width, cressie = TRUE))
    mr <- intentar(estimadores_mediana_recortada(d$U, d$V, d$res, cutoff, nb, RECORTE, MAX_PUNTOS_ESTIMADORES),
                   paste("estimadores mediana y media recortada", v))
    if (!is.null(mr)) { ve$Mediana <- mr$Mediana; ve$Media_recortada <- mr$Media_recortada
      if (mr$n_usados < nrow(d)) cat("  (Mediana y media recortada calculadas con una submuestra de", mr$n_usados, "puntos)\n") }
    write.csv(do.call(rbind, lapply(names(ve), function(e) data.frame(Estimador = e, as.data.frame(ve[[e]])[, c("np", "dist", "gamma")]))),
              ruta("e7", v, "tablas", paste0("variogramas_experimentales_", v, "_", sub, ".csv")), row.names = FALSE)

    # MODELOS - ESTIMADORES - AJUSTE
    fits <- list(); tabs <- list()
    for (e in names(ve)) { aj <- ajustar_todos(ve[[e]], e); fits <- c(fits, aj$fits); tabs[[e]] <- aj$tabla }
    t_aj <- apilar(tabs)
    if (is.null(t_aj)) { cat("  Ningún ajuste válido para este orden.\n"); next }
    t_aj$Mejor_del_estimador <- FALSE
    for (e in unique(t_aj$Estimador)) { ii <- which(t_aj$Estimador == e); t_aj$Mejor_del_estimador[ii[which.min(t_aj$MSE[ii])]] <- TRUE }
    acc_ajustes[[paste(v, k)]] <- data.frame(Variable = v, Orden = k, t_aj)
    write.csv(t_aj, ruta("e7", v, "tablas", paste0("ajustes_variograma_", v, "_", sub, ".csv")), row.names = FALSE)
    info_orden[[as.character(k)]] <- list(tg = tg, fits = fits)

    # GRÁFICOS
    pe <- do.call(rbind, lapply(names(ve), function(e) data.frame(Estimador = e, dist = ve[[e]]$dist, gamma = ve[[e]]$gamma)))
    mej <- t_aj[t_aj$Mejor_del_estimador, ]
    pl <- do.call(rbind, lapply(seq_len(nrow(mej)), function(i)
      data.frame(Estimador = mej$Estimador[i], variogramLine(fits[[mej$Clave[i]]], maxdist = cutoff),
                 Etiqueta = paste0(mej$Modelo[i], " ", mej$Ajuste[i], " · rango práctico ", round(mej$Rango_practico_km[i], 1), " km"))))
    guardar(ruta("e7", v, "graficos", paste0("GRAFICO_estimadores_experimentales_", v, "_", sub, ".png")),
      ggplot() + geom_point(data = pe, aes(dist, gamma), color = "black", shape = 1) +
        geom_line(data = pl, aes(dist, gamma), color = "firebrick", linewidth = .8) +
        geom_text(data = unique(pl[, c("Estimador", "Etiqueta")]), aes(x = Inf, y = -Inf, label = Etiqueta), hjust = 1.05, vjust = -0.6, size = 3) +
        facet_wrap(~Estimador) + expand_limits(y = 0) + theme_minimal() +
        labs(title = paste("Variogramas experimentales de los residuos -", v, "- tendencia de orden", k),
             subtitle = "Clásico, robusto, mediana y media recortada; línea = mejor modelo teórico de cada uno",
             x = "Distancia (km)", y = "Semivarianza"), 10, 7.5, 220, paste("estimadores", v, k))

    mat_mej <- do.call(rbind, lapply(unique(t_aj$Estimador), function(e) {
      ii <- which(t_aj$Estimador == e & t_aj$Modelo == "Mat"); if (length(ii) == 0) return(NULL)
      t_aj[ii[which.min(t_aj$MSE[ii])], ] }))
    mej_cv <- mej
    if (!is.null(mat_mej)) mej_cv <- rbind(mej_cv, mat_mej[!mat_mej$Clave %in% mej$Clave, , drop = FALSE])
    if (INCLUIR_MATERN_ETAPA6) {
      v6 <- variogram(res ~ 1, sp_r, cutoff = cutoff, width = AN[[v]]$width)
      m6 <- tryCatch(suppressWarnings(fit.variogram(v6, vgm(psill = max(v6$gamma) * 0.8, model = "Mat", range = max(v6$dist) / 3,
                                                            nugget = min(v6$gamma) * 0.5, kappa = KAPPA_MATERN))), error = function(e) NULL)
      if (!is.null(m6) && !isTRUE(attr(m6, "singular"))) {
        f6 <- fila_ajuste(m6, v6, "Clasico", "Mat", "Etapa6")
        f6$Mejor_del_estimador <- FALSE
        fits[[f6$Clave]] <- m6; info_orden[[as.character(k)]]$fits[[f6$Clave]] <- m6
        mej_cv <- rbind(mej_cv, f6[, names(mej_cv)])
      }
    }
    cat("  Candidatos de la validación cruzada:", paste(mej_cv$Estimador, mej_cv$Modelo, mej_cv$Ajuste, collapse = " | "), "\n")
    for (i in seq_len(nrow(mej_cv))) for (nm in NMAX_CANDIDATOS) {
      modelo <- fits[[mej_cv$Clave[i]]]
      cvk <- intentar(as.data.frame(krige.cv(res ~ 1, sp_r, model = modelo, nmax = nm, omax = omax_de(nm), maxdist = maxdist_de(v), verbose = FALSE)),
                      paste("validación LOO", v, "orden", k, mej_cv$Estimador[i], "nmax", nm))
      if (is.null(cvk)) next
      pred <- d$tend + cvk$var1.pred                      # predicción = tendencia + residuo
      cr <- criterio_cv(d$valor, pred, cvk$var1.var)
      rt <- ku_retro(pred, cvk$var1.var, inv); e_mg <- rt$media - d$orig
      clave <- paste(k, mej_cv$Clave[i], nm)
      cand[[clave]] <- data.frame(Variable = v, Orden = k, Tendencia = nombre_orden(k), Vecinos = nm,
        Estimador = mej_cv$Estimador[i], Modelo = mej_cv$Modelo[i], Ajuste = mej_cv$Ajuste[i], Clave_modelo = mej_cv$Clave[i],
        Rango_practico_km = mej_cv$Rango_practico_km[i], MSE_variograma = mej_cv$MSE[i], cr,
        MPE_mGal = mean(e_mg), RMSPE_mGal = sqrt(mean(e_mg^2)), R2_mGal = 1 - sum(e_mg^2) / sum((d$orig - mean(d$orig))^2))
      cv_guardadas[[clave]] <- data.frame(Observado = d$valor, Predicho = pred, Desv = sqrt(pmax(cvk$var1.var, 0)),
                                          Observado_mGal = d$orig, Predicho_mGal = rt$media)
      cat("  ", mej_cv$Estimador[i], mej_cv$Modelo[i], mej_cv$Ajuste[i], "| nmax =", nm, ": RMSPE =", round(cr$RMSPE, 4),
          "| RMSSPE =", round(cr$RMSSPE, 3), "| MSPE =", round(cr$MSPE, 4), "| R2 =", round(cr$R2, 4),
          "| RMSPE =", round(sqrt(mean(e_mg^2)), 2), "mGal\n")
    }
  }

  t_c <- apilar(cand)
  if (is.null(t_c)) { cat("  No hubo candidatos válidos para", v, "\n"); next }
  t_c <- seleccionar(t_c)
  if (FORZAR_MATERN_ETAPA6) {        
    im <- which(t_c$Ajuste == "Etapa6")
    if (length(im) > 0) {
      t_c$Elegido <- FALSE; t_c$Elegido[im[order(t_c$Puntaje[im], t_c$RMSPE[im])[1]]] <- TRUE
      cat("  (FORZAR_MATERN_ETAPA6 = TRUE: se interpola con el Matérn de la etapa 6; la tabla conserva el puntaje de todos)\n")
    } else cat("  ⚠ No hay Matérn de la etapa 6 válido para", v, "; se usa el elegido por validación cruzada\n")
  }
  acc_crit[[v]] <- t_c
  write.csv(t_c, ruta("e7", v, "tablas", paste0("TABLA_criterios_validacion_cruzada_", v, ".csv")), row.names = FALSE)
  el <- t_c[t_c$Elegido, ]; k <- el$Orden; nm <- el$Vecinos
  tg <- info_orden[[as.character(k)]]$tg; modelo <- info_orden[[as.character(k)]]$fits[[el$Clave_modelo]]
  d$tend <- tg$tend; d$res <- tg$res
  sp_r <- d[, c("U", "V", "res")]; coordinates(sp_r) <- ~U + V
  cat("\n===== Criterios de validación cruzada (leave one out) -", v, "=====\n")
  print(t_c[, c("Orden", "Vecinos", "Estimador", "Modelo", "Ajuste", "MPE", "ASEPE", "RMSPE", "MSPE", "RMSSPE", "R2", "RMSPE_mGal", "Puntaje", "Elegido")], digits = 4, row.names = FALSE)
  cat(" -> Elegido: tendencia de orden", nombre_orden(k), "| variograma", el$Modelo, "(", el$Estimador, ",", el$Ajuste, ") |", nm, "vecinos\n")
  if (k != orden_etapa4(v))
    cat("  ⚠ COHERENCIA: la etapa 4 indicó orden", orden_etapa4(v), "y aquí se eligió orden", k, "\n")
  acc_elegido[[v]] <- data.frame(Variable = v, Escala = unidad, Tendencia = nombre_orden(k),
    Formula_tendencia = paste("valor ~", terminos_ku(k)), R2_tendencia = summary(tg$reg)$r.squared,
    Vecinos = nm, Busqueda = if (BUSQUEDA_CUADRANTES) "cuadrantes" else "vecinos más cercanos",
    Estimador_experimental = el$Estimador, Modelo = el$Modelo, Ajuste = el$Ajuste,
    Pepita = sum(modelo$psill[modelo$model == "Nug"]), Meseta_parcial = sum(modelo$psill[modelo$model != "Nug"]),
    Rango_km = modelo$range[modelo$model != "Nug"][1], Rango_practico_km = rango_practico(el$Modelo, modelo$range[modelo$model != "Nug"][1]),
    Kappa = if (el$Modelo == "Mat") KAPPA_MATERN else NA,
    MPE = el$MPE, ASEPE = el$ASEPE, RMSPE = el$RMSPE, MSPE = el$MSPE, RMSSPE = el$RMSSPE, R2 = el$R2, RMSPE_mGal = el$RMSPE_mGal)

  t_aj <- acc_ajustes[[paste(v, k)]]
  tm <- t_aj[t_aj$Estimador == el$Estimador & t_aj$Modelo == el$Modelo, ]
  if (el$Ajuste == "Etapa6") tm <- tm[0, ]   # el Matérn de la etapa 6 ya está graficado en la etapa 6
  ve_el <- if (el$Estimador %in% c("Clasico", "Robusto")) variogram(res ~ 1, sp_r, cutoff = cutoff, width = width, cressie = el$Estimador == "Robusto") else NULL
  if (!is.null(ve_el) && nrow(tm) > 0) {
    lm_ <- do.call(rbind, lapply(seq_len(nrow(tm)), function(i)
      data.frame(variogramLine(info_orden[[as.character(k)]]$fits[[tm$Clave[i]]], maxdist = cutoff), Ajuste = paste0(tm$Ajuste[i], " (MSE = ", signif(tm$MSE[i], 4), ")"))))
    guardar(ruta("e7", v, "graficos", paste0("GRAFICO_variograma_elegido_OLS_WLS_WLS1_", v, ".png")),
      ggplot() + geom_point(data = as.data.frame(ve_el), aes(dist, gamma), shape = 1, size = 2) +
        geom_line(data = lm_, aes(dist, gamma, color = Ajuste, linetype = Ajuste), linewidth = .9) + expand_limits(y = 0) + theme_minimal() +
        labs(title = paste0("Variograma experimental (", gsub("_", " ", el$Estimador), ") de los residuos - ", v),
             subtitle = paste("Modelo", NOMBRE_MODELO[[el$Modelo]], "ajustado por OLS, WLS y WLS1"), x = "Distancia (km)", y = "Semivarianza", color = NULL, linetype = NULL),
      9, 6, 220, paste("variograma elegido", v))
  }

  sp_xy <- d[, c("X", "Y", "res")]; coordinates(sp_xy) <- ~X + Y
  intentar({
    vm <- variogram(res ~ 1, sp_xy, cutoff = cutoff, width = width, map = TRUE)
    ang <- if (!is.null(aniso)) aniso$angulo else 45
    dirs <- sort(c(ang, (ang + 90) %% 180))
    vdir <- variogram(res ~ 1, sp_xy, cutoff = cutoff, width = width, alpha = dirs)
    png(ruta("e7", v, "graficos", paste0("GRAFICO_mapa_variograma_y_direccionales_", v, ".png")), width = 3200, height = 1500, res = 220)
    print(plot(vm, main = paste("Mapa del variograma de los residuos -", v)), split = c(1, 1, 2, 1), more = TRUE)
    print(plot(vdir, model = modelo, main = paste0("Variogramas direccionales (", paste(dirs, collapse = "° y "), "°)"),
               xlab = "Distancia (km)", ylab = "Semivarianza"), split = c(2, 1, 2, 1), more = FALSE)
    dev.off()
  }, paste("mapa del variograma", v))

  gk <- coords_trabajo(grilla_ku, aniso)
  gk$Xs <- (gk$X - mean(dd$X)) / sd(dd$X); gk$Ys <- (gk$Y - mean(dd$Y)) / sd(dd$Y)
  tend_g <- as.numeric(predict(tg$reg, newdata = gk))
  sp_g <- gk[, c("U", "V")]; coordinates(sp_g) <- ~U + V
  pr <- intentar(as.data.frame(krige(res ~ 1, sp_r, sp_g, model = modelo, nmax = nm, omax = omax_de(nm), maxdist = maxdist_de(v), debug.level = 0)),
                 paste("kriging de los residuos", v))
  if (!is.null(pr)) {
    pred <- tend_g + pr$var1.pred
    rt <- ku_retro(pred, pr$var1.var, inv)
    res <- data.frame(grilla_ku[, c("Longitud", "Latitud", "X", "Y", "Dist_dato_km", "Extrapolacion")],
                      Tendencia = tend_g, Residuo_kriging = pr$var1.pred, Prediccion = pred, Varianza_error = pr$var1.var,
                      Pred_mGal = rt$media, Desv_mGal = rt$sd, IC95_inf_mGal = rt$p025, IC95_sup_mGal = rt$p975)
    write.csv(res, ruta("e7", v, "datos", paste0("superficie_KU_", v, ".csv")), row.names = FALSE)
    if (requireNamespace("terra", quietly = TRUE)) for (cc in c("Pred_mGal", "Desv_mGal"))
      intentar(terra::writeRaster(terra::rast(data.frame(x = res$X * 1000, y = res$Y * 1000, z = res[[cc]]), type = "xyz", crs = sf::st_crs(CRS_USADO)$wkt),
                                  ruta("e7", v, "datos", paste0("KU_", v, "_", cc, ".tif")), overwrite = TRUE), paste("GeoTIFF", cc))
    tit_v  <- paste0(NOMBRES_VARIABLE[[v]], " (", v, ")")
    sub_ku <- paste0("Kriging universal · tendencia de orden ", k, " removida · variograma ", NOMBRE_MODELO[[el$Modelo]],
                     " de los residuos · ", nm, " vecinos por cuadrantes")
    guardar(ruta("e7", v, "graficos", paste0("MAPA_KU_prediccion_", v, ".png")),
            mapa_geofisico(res, "Pred_mGal", tit_v, sub_ku, leyenda_grav(v), "anomalia",
                           titulo_mapa = titulo_cartucho(v, "Interpolación por kriging universal")), 9, 11, 300, paste("mapa KU", v))
    guardar(ruta("e7", v, "graficos", paste0("MAPA_KU_incertidumbre_", v, ".png")),
            mapa_geofisico(res, "Desv_mGal", paste("Incertidumbre de la predicción -", tit_v), paste(sub_ku, "· desviación estándar"),
                           "Desv. est.\n(miligales)", "incertidumbre",
                           titulo_mapa = titulo_cartucho(v, "Incertidumbre del kriging universal (desviación estándar)")), 9, 11, 300, paste("incertidumbre KU", v))
    guardar(ruta("e7", v, "graficos", paste0("MAPA_KU_prediccion_e_incertidumbre_", v, ".png")),
            gridExtra::arrangeGrob(mapa_geofisico(res, "Pred_mGal", paste("(a)", tit_v), sub_ku, leyenda_grav(v), "anomalia"),
                                   mapa_geofisico(res, "Desv_mGal", "(b) Desviación estándar de la predicción", sub_ku, "Desv. est.\n(miligales)", "incertidumbre"), ncol = 2),
            18, 10, 250, paste("mapas KU", v))
    KU[[v]] <- list(superficie = res, orden = k, nmax = nm, modelo = modelo, reg = tg$reg, tend = tg$tend, res = tg$res, tend_g = tend_g)
    cat("  Superficie:", nrow(res), "celdas | rango predicho:", paste(round(range(res$Pred_mGal), 2), collapse = " a "), "mGal\n")
  }
  cvg <- cv_guardadas[[paste(k, el$Clave_modelo, nm)]]
  if (!is.null(cvg)) {
    guardar(ruta("e7", v, "graficos", paste0("GRAFICO_validacion_cruzada_", v, ".png")),
      gridExtra::arrangeGrob(
        ggplot(cvg, aes(Observado_mGal, Predicho_mGal)) + geom_point(alpha = .25, size = .6, color = "steelblue") + geom_abline(color = "firebrick") +
          coord_equal() + theme_minimal() + labs(title = "Observado vs predicho (LOO)", x = "Observado (mGal)", y = "Predicho (mGal)"),
        ggplot(cvg, aes((Predicho - Observado) / Desv)) + geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "steelblue", color = "white") +
          stat_function(fun = dnorm, color = "firebrick") + theme_minimal() +
          labs(title = "Errores estandarizados", subtitle = "Deben parecerse a una N(0, 1): MSPE ~ 0 y RMSSPE ~ 1", x = "(predicho - observado) / desviación", y = "Densidad"),
        ncol = 2, top = paste("Validación cruzada del kriging universal -", v)), 12, 5.5, 220, paste("validación KU", v))
  }

  acc_param[[v]]$Modelo_variograma <- el$Modelo
  acc_param[[v]]$Pepita <- sum(modelo$psill[modelo$model == "Nug"])
  acc_param[[v]]$Meseta_parcial <- sum(modelo$psill[modelo$model != "Nug"])
  acc_param[[v]]$Rango_km <- modelo$range[modelo$model != "Nug"][1]
  acc_param[[v]]$Kappa <- if (el$Modelo == "Mat") KAPPA_MATERN else 1
  NMAX_KU_ELEGIDO[v] <- nm
}


# ULTIMAS TABLAS
t_ajustes <- apilar(acc_ajustes); t_crit <- apilar(acc_crit); t_eleg <- apilar(acc_elegido)
if (!is.null(t_ajustes)) write.csv(t_ajustes, ruta("e7", "", "tablas", "TABLA_ajustes_variograma_todos.csv"), row.names = FALSE)
if (!is.null(t_crit)) write.csv(t_crit, ruta("e7", "", "tablas", "TABLA_criterios_validacion_cruzada.csv"), row.names = FALSE)
if (!is.null(t_eleg)) { write.csv(t_eleg, ruta("e7", "", "tablas", "TABLA_MODELO_ELEGIDO_KU.csv"), row.names = FALSE)
                        cat("\n===== MODELOS DE KRIGING UNIVERSAL ELEGIDOS =====\n"); print(t(t_eleg)) }
if (requireNamespace("writexl", quietly = TRUE)) {
  hojas <- Filter(Negate(is.null), list(Ajustes_variograma = t_ajustes, Criterios_validacion = t_crit, Modelo_elegido = t_eleg))
  if (length(hojas) > 0) intentar(writexl::write_xlsx(hojas, ruta("e7", "", "", "TABLAS_KRIGING_UNIVERSAL.xlsx")), "Excel KU")
}
cat("\nCriterios: MPE ~ 0; RMSPE y ASEPE peque\u00f1os y parecidos; MSPE ~ 0; RMSSPE ~ 1 (mayor peso); R2 alto.\n",
    "\n================ ETAPA 7 TERMINADA ================\n")


#  COKRIGING LAS COVARIABLES ESTÁN ARRIBA
COVARIABLES_COKRIGING <- c("Elipsoidal", "Grav_Obs") # Y AQUÍ 
NMAX_COK              <- 20       
UMBRAL_COLINEALIDAD   <- 0.98     

ETAPAS[c("e8", "e9")] <- c("08_ETAPA_cokriging", "09_ETAPA_validacion_y_comparacion")
titulo_etapa("ETAPA 8: COKRIGING")
faltan_cov <- setdiff(COVARIABLES_COKRIGING, names(datos))
if (length(faltan_cov) > 0) stop("Covariables que no están en la base: ", paste(faltan_cov, collapse = ", "))
a_sp <- function(df) { df <- as.data.frame(df); coordinates(df) <- ~U + V; df }
inv_de <- function(v) if (ESCALA_KU == "normalizada") NZ[[v]]$elegido$inv else function(z) z
preparar_variable <- function(v) {
  dd <- TR[[v]]$dd; if (ESCALA_KU == "original") dd$valor <- datos[[v]]
  aniso <- AN[[v]]$aniso
  d <- coords_trabajo(dd, aniso); d$orig <- datos[[v]]
  d$tend <- KU[[v]]$tend; d$res <- KU[[v]]$res
  g <- coords_trabajo(grilla_ku, aniso)
  g$Xs <- (g$X - mean(dd$X)) / sd(dd$X); g$Ys <- (g$Y - mean(dd$Y)) / sd(dd$Y)
  list(d = d, g = g)
}

COK <- list(); acc_lmc <- list(); acc_cok_mod <- list(); G_COK <- list()
for (v in VARIABLES_PRINCIPALES) {
  if (is.null(KU[[v]])) { cat("\n", v, ": no hay kriging universal elegido; se omite el cokriging.\n"); next }
  cat("\n--- Variable:", v, "| covariables:", paste(COVARIABLES_COKRIGING, collapse = ", "), "---\n")
  pv <- preparar_variable(v); inv <- inv_de(v); k <- KU[[v]]$orden
  modelo_ku <- KU[[v]]$modelo
  completos <- complete.cases(datos[, COVARIABLES_COKRIGING, drop = FALSE])
  if (sum(!completos) > 0) cat("  Aviso:", sum(!completos), "estaciones sin covariables quedan fuera del cokriging\n")
  d <- pv$d[completos, ]
  r2 <- summary(lm(datos[[v]][completos] ~ ., data = datos[completos, COVARIABLES_COKRIGING, drop = FALSE]))$r.squared
  cat("  R2 de", v, "explicada por las covariables:", round(r2, 4), "\n")
  if (r2 > UMBRAL_COLINEALIDAD)
    cat("  ⚠ Colinealidad muy alta: la anomalía es casi una combinación exacta de las covariables (por su propia fórmula).\n",
        "   El LMC puede quedar al límite de ser definido positivo. Revise la tabla del LMC y considere usar una sola covariable.\n")
#     TENDENCIA
    tg_v <- tendencia_global(d, d$valor, k)
  g <- gstat(NULL, id = v, formula = res ~ 1, data = a_sp(data.frame(U = d$U, V = d$V, res = tg_v$res)),
             nmax = NMAX_COK, omax = omax_de(NMAX_COK), maxdist = maxdist_de(v))
  for (cv in COVARIABLES_COKRIGING) {
    x <- datos[[cv]][completos]; xz <- (x - mean(x)) / sd(x)      # estandarizada: evita problemas de escala
    tg_c <- tendencia_global(d, xz, k)                            # tendencia de la covariable, mismo orden
    g <- gstat(g, id = cv, formula = res ~ 1, data = a_sp(data.frame(U = d$U, V = d$V, res = tg_c$res)),
               nmax = NMAX_COK, omax = omax_de(NMAX_COK), maxdist = maxdist_de(v))
  }
  ids <- c(v, COVARIABLES_COKRIGING)
  v_cross <- intentar(variogram(g, cutoff = AN[[v]]$cutoff, width = AN[[v]]$cutoff / N_BINS_VARIOGRAMA), paste("variogramas cruzados", v))
  if (is.null(v_cross)) next
  est <- modelo_ku[modelo_ku$model != "Nug", ][1, ]
  base <- vgm(psill = 1, model = as.character(est$model), range = est$range, nugget = 0.1, kappa = if (is.finite(est$kappa)) est$kappa else KAPPA_MATERN)
  g <- gstat(g, model = base, fill.all = TRUE)
  g_fit <- intentar(fit.lmc(v_cross, g, fit.ranges = FALSE, correct.diagonal = 1.01), paste("ajuste LMC", v))
  if (is.null(g_fit)) next
  G_COK[[v]] <- list(g = g_fit, completos = completos, d = d, tend = tg_v$tend)
  write.csv(as.data.frame(v_cross), ruta("e8", v, "tablas", paste0("variogramas_directos_y_cruzados_", v, ".csv")), row.names = FALSE)
  intentar({
    png(ruta("e8", v, "graficos", paste0("GRAFICO_LMC_variogramas_", v, ".png")), width = 2600, height = 2400, res = 220)
    print(plot(v_cross, model = g_fit, main = paste("Modelo lineal de corregionalización (residuos) -", v), xlab = "Distancia (km)", ylab = "Semivarianza"))
    dev.off()
  }, paste("gráfico LMC", v))
  for (j in seq_len(nrow(g_fit$model[[v]]))) {
    M <- matrix(NA_real_, length(ids), length(ids), dimnames = list(ids, ids))
    for (i1 in seq_along(ids)) for (i2 in seq_along(ids)) {
      nmb <- if (i1 == i2) ids[i1] else intersect(c(paste(ids[i1], ids[i2], sep = "."), paste(ids[i2], ids[i1], sep = ".")), names(g_fit$model))[1]
      M[i1, i2] <- g_fit$model[[nmb]]$psill[j]
    }
    ev <- eigen(M, symmetric = TRUE, only.values = TRUE)$values
    R <- suppressWarnings(cov2cor(M))
    acc_lmc[[paste(v, j)]] <- data.frame(Variable = v, Estructura = as.character(g_fit$model[[v]]$model[j]),
      Par = c(ids, combn(ids, 2, paste, collapse = " x ")),
      Meseta = c(diag(M), M[lower.tri(M)][order(col(M)[lower.tri(M)])]),
      Correlacion = c(rep(1, length(ids)), R[lower.tri(R)][order(col(R)[lower.tri(R)])]),
      Autovalor_minimo = min(ev), Definida_positiva = min(ev) > 0)
    cat("  Estructura", as.character(g_fit$model[[v]]$model[j]), ": autovalor mínimo =", signif(min(ev), 4),
        if (min(ev) > 0) "(definida positiva)\n" else "(⚠ NO definida positiva)\n")
  }
  acc_cok_mod[[v]] <- data.frame(Variable = v, Covariables = paste(COVARIABLES_COKRIGING, collapse = ", "),
    Tendencia = nombre_orden(k), Modelo_base = as.character(est$model), Rango_km = est$range,
    Rango_practico_km = rango_practico(as.character(est$model), est$range),
    Vecinos_por_variable = NMAX_COK, R2_colinealidad = r2, N_estaciones = nrow(d))
  pr <- intentar(as.data.frame(predict(g_fit, a_sp(pv$g[, c("U", "V")]), debug.level = 0)), paste("cokriging", v))
  if (!is.null(pr)) {
    tend_g <- as.numeric(predict(tg_v$reg, newdata = pv$g))
    pred <- tend_g + pr[[paste0(v, ".pred")]]; vv <- pr[[paste0(v, ".var")]]
    rt <- ku_retro(pred, vv, inv)
    res <- data.frame(grilla_ku[, c("Longitud", "Latitud", "X", "Y", "Dist_dato_km", "Extrapolacion")],
                      Tendencia = tend_g, Prediccion = pred, Varianza_error = vv,
                      Pred_mGal = rt$media, Desv_mGal = rt$sd, IC95_inf_mGal = rt$p025, IC95_sup_mGal = rt$p975)
    COK[[v]] <- res
    write.csv(res, ruta("e8", v, "datos", paste0("superficie_CoK_", v, ".csv")), row.names = FALSE)
    tit_v   <- paste0(NOMBRES_VARIABLE[[v]], " (", v, ")")
    nom_cov <- c(Elipsoidal = "altura elipsoidal", Grav_Obs = "gravedad observada")
    sub_cok <- paste0("Cokriging · tendencia de orden ", k, " removida · covariables: ",
                      paste(ifelse(COVARIABLES_COKRIGING %in% names(nom_cov), nom_cov[COVARIABLES_COKRIGING], COVARIABLES_COKRIGING), collapse = " y "))
    guardar(ruta("e8", v, "graficos", paste0("MAPA_CoK_prediccion_", v, ".png")),
            mapa_geofisico(res, "Pred_mGal", tit_v, sub_cok, leyenda_grav(v), "anomalia",
                           titulo_mapa = titulo_cartucho(v, "Interpolación por cokriging")), 9, 11, 300, paste("mapa CoK", v))
    guardar(ruta("e8", v, "graficos", paste0("MAPA_CoK_incertidumbre_", v, ".png")),
            mapa_geofisico(res, "Desv_mGal", paste("Incertidumbre de la predicción -", tit_v), paste(sub_cok, "· desviación estándar"),
                           "Desv. est.\n(miligales)", "incertidumbre",
                           titulo_mapa = titulo_cartucho(v, "Incertidumbre del cokriging (desviación estándar)")), 9, 11, 300, paste("incertidumbre CoK", v))
    cat("  Superficie:", nrow(res), "celdas | rango predicho:", paste(round(range(res$Pred_mGal), 2), collapse = " a "), "mGal\n")
  }
}
if (!is.null(apilar(acc_lmc))) write.csv(apilar(acc_lmc), ruta("e8", "", "tablas", "TABLA_LMC_corregionalizacion.csv"), row.names = FALSE)
if (!is.null(apilar(acc_cok_mod))) write.csv(apilar(acc_cok_mod), ruta("e8", "", "tablas", "TABLA_parametros_CoK.csv"), row.names = FALSE)


#  VALIDACIÓN CRUZADA LEAVE ONE OUT 
N_VECINOS_LOCALES_COK <- 300   

titulo_etapa("ETAPA 9: VALIDACIÓN CRUZADA LEAVE ONE OUT Y COMPARACIÓN")

cv_cok_loo <- function(g_fit, ids, n_loc) {
  d0 <- g_fit$data[[ids[1]]]$data; xy <- sp::coordinates(d0); n <- nrow(xy)
  pred <- rep(NA_real_, n); var <- pred
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  for (i in seq_len(n)) {
    d2 <- (xy[, 1] - xy[i, 1])^2 + (xy[, 2] - xy[i, 2])^2; d2[i] <- Inf
    sel <- order(d2)[seq_len(min(n_loc, n - 1))]
    g2 <- g_fit
    for (id in ids) g2$data[[id]]$data <- g_fit$data[[id]]$data[sel, ]
    pr <- NULL
    invisible(utils::capture.output(pr <- tryCatch(as.data.frame(predict(g2, d0[i, ], debug.level = 0)), error = function(e) NULL)))
    if (!is.null(pr)) { pred[i] <- pr[[paste0(ids[1], ".pred")]]; var[i] <- pr[[paste0(ids[1], ".var")]] }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  data.frame(observed = d0$res, pred = pred, var = var)
}

acc_cv <- list(); acc_pts <- list(); esq <- "Leave one out"
fila_cv <- function(v, metodo, esq, obs, pred, var, orig, inv) {
  ok <- is.finite(pred) & is.finite(var)
  rt <- ku_retro(pred[ok], var[ok], inv); e <- rt$media - orig[ok]
  list(tabla = data.frame(Variable = v, Metodo = metodo, Validacion = esq, criterio_cv(obs, pred, var),
                          MPE_mGal = mean(e), RMSPE_mGal = sqrt(mean(e^2)), R2_mGal = 1 - sum(e^2) / sum((orig[ok] - mean(orig[ok]))^2)),
       puntos = data.frame(Variable = v, Metodo = metodo, Validacion = esq, Observado_mGal = orig[ok], Predicho_mGal = rt$media,
                           Error_estandarizado = (pred[ok] - obs[ok]) / sqrt(pmax(var[ok], 1e-12))))
}
for (v in VARIABLES_PRINCIPALES) {
  if (is.null(KU[[v]])) next
  cat("\n--- Variable:", v, "---\n")
  pv <- preparar_variable(v); inv <- inv_de(v)
  sp_d <- a_sp(pv$d[, c("U", "V", "res")])
  cvk <- intentar(as.data.frame(krige.cv(res ~ 1, sp_d, model = KU[[v]]$modelo, nmax = KU[[v]]$nmax, omax = omax_de(KU[[v]]$nmax), maxdist = maxdist_de(v),
                                         verbose = FALSE)), paste("validación KU", v))
  if (!is.null(cvk)) {
    r <- fila_cv(v, "Kriging universal", esq, pv$d$valor, pv$d$tend + cvk$var1.pred, cvk$var1.var, pv$d$orig, inv)
    acc_cv[[paste(v, "KU")]] <- r$tabla; acc_pts[[paste(v, "KU")]] <- r$puntos
    cat("  Kriging universal: RMSSPE =", round(r$tabla$RMSSPE, 3), "| MSPE =", round(r$tabla$MSPE, 3), "| R2 =", round(r$tabla$R2, 3),
        "| RMSPE =", round(r$tabla$RMSPE_mGal, 3), "mGal\n")
  }
  gc_ <- G_COK[[v]]
  if (is.null(gc_)) next
  cat("  Cokriging (una predicción por estación; ver barra de avance):\n")
  cvc <- intentar(cv_cok_loo(gc_$g, c(v, COVARIABLES_COKRIGING), N_VECINOS_LOCALES_COK), paste("validación cokriging", v))
  if (!is.null(cvc)) {
    r <- fila_cv(v, "Cokriging", esq, gc_$tend + cvc$observed, gc_$tend + cvc$pred, cvc$var, gc_$d$orig, inv)
    acc_cv[[paste(v, "CoK")]] <- r$tabla; acc_pts[[paste(v, "CoK")]] <- r$puntos
    cat("  Cokriging: RMSSPE =", round(r$tabla$RMSSPE, 3), "| MSPE =", round(r$tabla$MSPE, 3), "| R2 =", round(r$tabla$R2, 3),
        "| RMSPE =", round(r$tabla$RMSPE_mGal, 3), "mGal\n")
  }
}
t_cv <- apilar(acc_cv); t_pts <- apilar(acc_pts); t_comp <- NULL
if (!is.null(t_cv)) {
  t_comp <- do.call(rbind, lapply(split(t_cv, list(t_cv$Variable, t_cv$Validacion), drop = TRUE), seleccionar))
  rownames(t_comp) <- NULL
  write.csv(t_comp, ruta("e9", "", "tablas", "TABLA_COMPARACION_KU_vs_CoK.csv"), row.names = FALSE)
  write.csv(t_pts, ruta("e9", "", "datos", "validacion_cruzada_por_punto.csv"), row.names = FALSE)
  cat("\n===== COMPARACIÓN =====\n")
  print(t_comp[, c("Variable", "Validacion", "Metodo", "MPE", "ASEPE", "RMSPE", "MSPE", "RMSSPE", "R2", "RMSPE_mGal", "Elegido")], digits = 4, row.names = FALSE)
  for (v in unique(t_comp$Variable)) for (esq in unique(t_comp$Validacion)) {
    el <- t_comp[t_comp$Variable == v & t_comp$Validacion == esq & t_comp$Elegido, ]
    if (nrow(el)) cat(" ->", v, "|", esq, ": método más adecuado =", el$Metodo, "\n")
  }
  guardar(ruta("e9", "", "graficos", "GRAFICO_criterios_KU_vs_CoK.png"),
    gridExtra::arrangeGrob(
      ggplot(t_cv, aes(Validacion, RMSPE_mGal, fill = Metodo)) + geom_col(position = "dodge") + facet_wrap(~Variable, scales = "free_y") +
        theme_minimal() + theme(axis.text.x = element_text(angle = 10, hjust = 1)) + labs(title = "RMSPE (mGal): menor es mejor", x = NULL, y = "mGal", fill = NULL),
      ggplot(t_cv, aes(Validacion, RMSSPE, fill = Metodo)) + geom_col(position = "dodge") + geom_hline(yintercept = 1, linetype = "dashed") +
        facet_wrap(~Variable) + theme_minimal() + theme(axis.text.x = element_text(angle = 10, hjust = 1)) +
        labs(title = "RMSSPE: debe ser cercano a 1 (línea)", x = NULL, y = "RMSSPE", fill = NULL), nrow = 2),
    10, 9, 220, "criterios")
  for (v in unique(t_pts$Variable)) {
    pp <- t_pts[t_pts$Variable == v, ]
    guardar(ruta("e9", "", "graficos", paste0("GRAFICO_observado_vs_predicho_", v, ".png")),
      ggplot(pp, aes(Observado_mGal, Predicho_mGal)) + geom_point(alpha = .2, size = .5, color = "steelblue") + geom_abline(color = "firebrick") +
        facet_grid(Validacion ~ Metodo) + coord_equal() + theme_minimal() +
        labs(title = paste("Validación cruzada -", NOMBRES_VARIABLE[[v]]), x = "Observado (mGal)", y = "Predicho (mGal)"), 10, 9.5, 200, paste("obs-pred", v))
  }
}
for (v in VARIABLES_PRINCIPALES) if (!is.null(KU[[v]]) && !is.null(COK[[v]])) {
  ku <- KU[[v]]$superficie
  dif <- data.frame(ku[, c("Longitud", "Latitud", "X", "Y", "Extrapolacion")], Diferencia_mGal = COK[[v]]$Pred_mGal - ku$Pred_mGal)
  write.csv(dif, ruta("e9", "", "datos", paste0("diferencia_CoK_menos_KU_", v, ".csv")), row.names = FALSE)
  dma <- mean(abs(dif$Diferencia_mGal[!dif$Extrapolacion]), na.rm = TRUE)
  guardar(ruta("e9", "", "graficos", paste0("MAPA_diferencia_CoK_menos_KU_", v, ".png")),
          mapa_geofisico(dif, "Diferencia_mGal", paste0("Diferencia cokriging − kriging universal · ", NOMBRES_VARIABLE[[v]], " (", v, ")"),
                         paste0("Diferencia media absoluta (zona con datos): ", format(round(dma, 2), decimal.mark = ","), " mGal"),
                         "Diferencia\n(miligales)", "diferencia",
                         titulo_mapa = titulo_cartucho(v, "Diferencia cokriging menos kriging universal")), 9, 11, 300, paste("diferencia", v))
}


# RESUMEN FINAL 
if (requireNamespace("writexl", quietly = TRUE)) {
  hojas <- Filter(Negate(is.null), list(
    E6b_Umbrales = t_umb, E6b_Variogramas_ind = t_vind, E6b_KI_vecindarios = t_kin, E6b_KI_vs_CoKI = t_resi,
    E7_Ajustes_variograma = t_ajustes, E7_Criterios_LOO = t_crit, E7_Modelo_elegido = t_eleg,
    E8_Parametros_CoK = apilar(acc_cok_mod), E8_LMC = apilar(acc_lmc), E9_Comparacion = t_comp))
  if (length(hojas) > 0) intentar(writexl::write_xlsx(hojas, ruta("resumen", "", "", "TABLAS_INTERPOLACION.xlsx")), "Excel final")
}
cat("\n\n================ PROCESO COMPLETO TERMINADO  ================\nResultados en:", normalizePath(CARPETA_SALIDA), "\n",)
if (!requireNamespace("terra", quietly = TRUE)) 
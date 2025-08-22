# --------------------------------------------------------------
# Análisis de Tolerancia Térmica en Opuntia spp.
# 
# Propósito:
# Este script integra datos espaciales y térmicos para evaluar la
# tolerancia al calor de especies de Opuntia en Querétaro y el Bajío.
# Permite clasificar a las especies en categorías de respuesta térmica
# mediante estadística no paramétrica, visualizaciones y análisis
# bioinformático. Los resultados apoyan investigaciones en
# biogeografía, ecología térmica
#
# Autor: Jorge Ibares
# Fecha: 2025-07-30
# --------------------------------------------------------------

# ===== 1. CARGA DE LIBRERÍAS =====
library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(stringr)
library(viridis)
library(FSA)

# ===== 2. CARGA DE DATOS =====
Opuntia<- st_read("RUTA") #
Temp <- raster("RUTA")

# ===== 3. EXTRACCIÓN DE TEMPERATURA =====
Opuntia$temperatura <- raster::extract(Temp, Opuntia)
Opuntia$especie_abreviada <- word(Opuntia$acceptedSc, 1, 2)
Opuntia_filtrado <- Opuntia %>% filter(!is.na(temperatura))

# ===== 4. BOXPLOT =====
ggplot(Opuntia_filtrado, aes(x = especie_abreviada, y = temperatura, fill = especie_abreviada)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.5) +
  labs(title = "Distribución de temperatura por especie de Opuntias",
       x = "Especie", y = "Temperatura (°C)") +
  scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        legend.position = "none")

# ===== 5. GRÁFICO DE DENSIDAD =====
ggplot(Opuntia_filtrado, aes(x = temperatura, fill = especie_abreviada)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densidad de temperatura por especie de Phaseolus",
       x = "Temperatura (°C)", y = "Densidad") +
  scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9) +
  theme_minimal(base_size = 13)

# ===== 6. PRUEBA ESTADÍSTICA GENERAL =====
kruskal_resultado <- kruskal.test(temperatura ~ especie_abreviada, data = Opuntia_filtrado)
print(kruskal_resultado)

dunn_result <- dunnTest(temperatura ~ especie_abreviada, data = Opuntia_filtrado, method = "bh")
print(dunn_result)

# ===== 7. RESUMEN POR ESPECIE =====
resumen_especies <- Opuntia_filtrado %>%
  group_by(especie_abreviada) %>%
  summarise(
    n = n(),
    media = mean(temperatura, na.rm = TRUE),
    mediana = median(temperatura, na.rm = TRUE),
    sd = sd(temperatura, na.rm = TRUE),
    min = min(temperatura, na.rm = TRUE),
    max = max(temperatura, na.rm = TRUE)
  )

# ===== 8. CLASIFICACIÓN ECOLÓGICA / BIOINFORMÁTICA =====
resumen_especies <- resumen_especies %>%
  mutate(
    clasificacion_termica = case_when(
      media > 30 & max > 35 ~ "Altamente termotolerante",
      media >= 27 & media <= 30 ~ "Moderadamente tolerante",
      media < 27 & max < 33 ~ "Sensible al calor",
      media < 25 & max < 30 ~ "Altamente sensible",
      TRUE ~ "Sin clasificar"
    )
  )

# ===== 9. SUBCLASIFICACIÓN ADICIONAL =====
resumen_especies <- resumen_especies %>%
  mutate(
    subclasificacion = case_when(
      clasificacion_termica == "Sin clasificar" & sd > 5 ~ "Alta variabilidad térmica",
      clasificacion_termica == "Sin clasificar" & sd <= 5 ~ "Baja variabilidad térmica",
      clasificacion_termica == "Sin clasificar" & min >= 22 ~ "Temperaturas mínimas altas",
      clasificacion_termica == "Sin clasificar" & min < 22 ~ "Temperaturas mínimas bajas",
      TRUE ~ NA_character_
    )
  )

# Mostrar resultado
print(resumen_especies %>% select(especie_abreviada, media, max, clasificacion_termica, subclasificacion))

# ===== 10. VISUALIZACIÓN FINAL DE CLASIFICACIÓN TÉRMICA =====
ggplot(resumen_especies, aes(x = reorder(especie_abreviada, media), y = media, fill = clasificacion_termica)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  labs(title = "Clasificación térmica ",
       x = "Especie", y = "Temperatura media (°C)", fill = "Clasificación") +
  scale_fill_manual(values = c(
    "Altamente termotolerante" = "#D73027",
    "Moderadamente tolerante" = "#FC8D59",
    "Sensible al calor" = "#91BFDB",
    "Altamente sensible" = "#4575B4",
    "Sin clasificar" = "gray50"
  )) +
  theme_minimal(base_size = 13) +
  theme(axis.text.y = element_text(size = 9))


library(readxl)
Tablas_de_crecimiento <- read_excel("C:\\Users\\jolum\\Downloads\\Tablas de crecimiento.xlsx", 
                                    sheet = "Hoja2")
IS_weibull <- Tablas_de_crecimiento %>% 
  filter(modelo =="weibull")

IS_schumacher <- Tablas_de_crecimiento %>% 
  filter(modelo =="schumacher")

IS_chapman <- Tablas_de_crecimiento %>% 
  filter(modelo =="chapman")

Plot_Chapma<- ggplot() +
  geom_smooth(data = IS_chapman, aes(x = `Edad en Años`, y = Excelente , color = "Excelente"), se = FALSE) +
  geom_smooth(data = IS_chapman, aes(x = `Edad en Años`, y = Bueno, color = "Bueno"), se = FALSE) +
  geom_smooth(data = IS_chapman, aes(x = `Edad en Años`, y = Medio, color = "Medio"), se = FALSE) +
  geom_smooth(data = IS_chapman, aes(x = `Edad en Años`, y = Malo , color = "Malo"), se = FALSE) +
  geom_smooth(data = IS_chapman, aes(x = `Edad en Años`, y = Pésimo, color = "Pésimo"), se = FALSE) +
  geom_point(data = Clean_BD_teca, aes(x = Edad_a, y = Altura_m, shape = `NS. Sitio`), size = 2) +
  scale_color_manual(name = "Categoría",
                     values = c("Excelente" = "darkgreen", "Bueno" = "#43CD80",
                                "Medio" = "orange", "Malo" = "#FFD700", "Pésimo" = "red"),
                     labels = c("Excelente", "Bueno", "Medio", "Malo", "Pésimo"),
                     breaks = c("Excelente", "Bueno", "Medio", "Malo", "Pésimo")) +
  scale_shape_manual(name = "Fincas",
                     values = c(20,6,18,21,3,4,13,17,14,2,16,15)) +
  labs(x = "Edad (años)", y = "Altura dominante (m)", color = "Finca", shape = "Finca") +
  #ggtitle("Curvas de crecimiento por modelo de Chapma Richards y alturas observadas en la Costa Sur para" ~ italic("Tectona grandis")) +
  ylim(0, NA) +
  theme_minimal_grid() +
  theme(legend.position = "none" , 
        axis.title.x = element_text(size = 14, family = "Times New Roman"),  # Ajustar el tamaño de la letra del eje x
        axis.title.y = element_text(size = 14, family = "Times New Roman"),  # Ajustar el tamaño de la letra del eje y
        axis.text.x = element_text(size = 12, family = "Times New Roman"),   # Ajustar el tamaño de los textos del eje x
        axis.text.y = element_text(size = 12, family = "Times New Roman"))   # Ajustar el tamaño de los textos del eje y
Plot_Chapma

names(teca)

Plot_Weibull<- ggplot() +
  geom_smooth(data = IS_weibull, aes(x = `Edad en Años`, y = Excelente , color = "Excelente"), se = FALSE) +
  geom_smooth(data = IS_weibull, aes(x = `Edad en Años`, y = Bueno, color = "Bueno"), se = FALSE) +
  geom_smooth(data = IS_weibull, aes(x = `Edad en Años`, y = Medio, color = "Medio"), se = FALSE) +
  geom_smooth(data = IS_weibull, aes(x = `Edad en Años`, y = Malo , color = "Malo"), se = FALSE) +
  geom_smooth(data = IS_weibull, aes(x = `Edad en Años`, y = Pésimo, color = "Pésimo"), se = FALSE) +
  geom_point(data = Clean_BD_teca, aes(x = Edad_a, y = Altura_m, shape = `NS. Sitio`), size = 2) +
  scale_color_manual(name = "Categoría",
                     values = c("Excelente" = "darkgreen", "Bueno" = "#43CD80",
                                "Medio" = "orange", "Malo" = "#FFD700", "Pésimo" = "red"),
                     labels = c("Excelente", "Bueno", "Medio", "Malo", "Pésimo"),
                     breaks = c("Excelente", "Bueno", "Medio", "Malo", "Pésimo")) +
  scale_shape_manual(name = "Fincas",
                     values = c(20,6,18,21,3,4,13,17,14,13,16,2)) +
  labs(x = "Edad (años)", y = "Altura dominante (m)", color = "Finca", shape = "Finca") +
  #ggtitle("Curvas de crecimiento por modelo de Weibull y alturas observadas en la Costa Sur para" ~ italic("Tectona grandis")) +
  ylim(0, NA) +
  theme_minimal_grid() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 14, family = "Times New Roman"),  # Ajustar el tamaño de la letra del eje x
        axis.title.y = element_text(size = 14, family = "Times New Roman"),  # Ajustar el tamaño de la letra del eje y
        axis.text.x = element_text(size = 12, family = "Times New Roman"),   # Ajustar el tamaño de los textos del eje x
        axis.text.y = element_text(size = 12, family = "Times New Roman"))   # Ajustar el tamaño de los textos del eje 
Plot_Weibull





Plot_Schuma<- ggplot() +
  geom_smooth(data = IS_schumacher, aes(x = `Edad en Años`, y = Excelente , color = "Excelente"), se = FALSE) +
  geom_smooth(data = IS_schumacher, aes(x = `Edad en Años`, y = Bueno, color = "Bueno"), se = FALSE) +
  geom_smooth(data = IS_schumacher, aes(x = `Edad en Años`, y = Medio, color = "Medio"), se = FALSE) +
  geom_smooth(data = IS_schumacher, aes(x = `Edad en Años`, y = Malo , color = "Malo"), se = FALSE) +
  geom_smooth(data = IS_schumacher, aes(x = `Edad en Años`, y = Pésimo, color = "Pésimo"), se = FALSE) +
  geom_point(data = Clean_BD_teca, aes(x = Edad_a, y = Altura_m, shape = `NS. Sitio`), size = 2) +
  scale_color_manual(name = "Categoría",
                     values = c("Excelente (IS" = "darkgreen", "Bueno" = "#43CD80",
                                "Medio" = "orange", "Malo" = "#FFD700", "Pésimo" = "red"),
                     labels = c("Excelente", "Bueno", "Medio", "Malo", "Pésimo"),
                     breaks = c("Excelente", "Bueno", "Medio", "Malo", "Pésimo")) +
  scale_shape_manual(name = "Fincas",
                     values = c(20,6,18,21,3,4,13,17,14,2,16,15)) +
  labs(x = "Edad (años)", y = "Altura dominante (m)", color = "Finca", shape = "Finca") +
  #ggtitle("Curvas de crecimiento por modelo de Schumaher y alturas observadas en la Costa Sur para" ~ italic("Tectona grandis")) +
  ylim(0, NA) +
  theme_minimal() +
  theme(legend.position = "right", 
        legend.text = element_text(size = 12, family = "Times New Roman"), 
        legend.title = element_text(size = 12, family = "Times New Roman"),
        axis.title.x = element_text(size = 14, family = "Times New Roman"),  # Ajustar el tamaño de la letra del eje x
        axis.title.y = element_text(size = 14, family = "Times New Roman"),  # Ajustar el tamaño de la letra del eje y
        axis.text.x = element_text(size = 14, family = "Times New Roman"),   # Ajustar el tamaño de los textos del eje x
        axis.text.y = element_text(size = 14, family = "Times New Roman"))   # Ajustar el tamaño de los textos del eje 

Plot_Schuma



paleta_colores <- brewer.pal(n = 8, name = "Set1")

paleta_colores <- brewer.pal(n = 8, name = "Set3")
print(paleta_colores)

Plot_Weibull<- ggplot() +
  geom_smooth(data = IS_weibull, aes(x = `Edad en Años`, y = Pésimo), se = FALSE, color = "red") +
  geom_smooth(data = IS_weibull, aes(x = `Edad en Años`, y = Malo), se = FALSE, color = "#FFD700") +
  geom_smooth(data = IS_weibull, aes(x = `Edad en Años`, y = Medio), se = FALSE, color = "orange") +
  geom_smooth(data = IS_weibull, aes(x = `Edad en Años`, y = Bueno), se = FALSE, color = "#43CD80") +
  geom_smooth(data = IS_weibull, aes(x = `Edad en Años`, y = Excelente), se = FALSE, color = "darkgreen") +
  geom_point(data = Clean_BD_teca, aes(x = Edad_a, y = Altura_m, colour = `NS. Sitio`, shape = `NS. Sitio` ), size = 3) +
  scale_color_manual(name = "Fincas", 
                     values = c( "#FFD700", "#9E0142", "red", "#E7298A", "#43CD80", "#377EB8",
                                 "#BC80BD", "blue","black", "#BC80BD", "#E7298A", "#252525",
                                 "#E41A1C", "#377EB8", "#4DAF4A", "red", "#A65628")) +
  scale_shape_manual(name = "Fincas", 
                     values = c(15,16,17,18,19,20,15,16,17,18,19,20,15,16,17,18,19,20)) +
  labs(x = "Edad en" ~ italic("años"), y = "Altura dominante en" ~ italic("m"),color = "Finca", shape = "Finca") +
 # ggtitle("Alturas dominantes en la región Sur de Guatemala" ~ italic("Tectona grandis")) +
  theme_linedraw() +
  ylim(0, NA) +
  theme(legend.position = "right", 
        legend.text = element_text(size = 12, family = "Times New Roman"), 
        legend.title = element_text(size = 14, family = "Times New Roman"))+
  #guides(color = guide_legend(override.aes = list(size = 5)))+
  theme(plot.title=element_text(family='Times New Roman', face='bold', colour='black', size=24),
        plot.subtitle = element_text(family='Times New Roman', face=1, colour='black', size=18),
        plot.caption = element_text(family='Times New Roman', face=1, colour='black', size=18))
Plot_Weibull

Plot_schumacher<- ggplot() +
  geom_smooth(data = IS_schumacher, aes(x = `Edad en Años`, y = Pésimo), se = FALSE, color = "red") +
  geom_smooth(data = IS_schumacher, aes(x = `Edad en Años`, y = Malo), se = FALSE, color = "#FFD700") +
  geom_smooth(data = IS_schumacher, aes(x = `Edad en Años`, y = Medio), se = FALSE, color = "orange") +
  geom_smooth(data = IS_schumacher, aes(x = `Edad en Años`, y = Bueno), se = FALSE, color = "#43CD80") +
  geom_smooth(data = IS_schumacher, aes(x = `Edad en Años`, y = Excelente), se = FALSE, color = "darkgreen") +
  geom_point(data = Clean_BD_teca, aes(x = Edad_a, y = Altura_m, colour = `NS. Sitio`, shape = `NS. Sitio` ), size = 3) +
  scale_color_manual(name = "Fincas", 
                     values = c( "#FFD700", "#9E0142", "red", "#E7298A", "#43CD80", "#377EB8",
                                 "#BC80BD", "blue","black", "#BC80BD", "#E7298A", "#252525",
                                 "#E41A1C", "#377EB8", "#4DAF4A", "red", "#A65628")) +
  scale_shape_manual(name = "Fincas", 
                     values = c(15,16,17,18,19,20,15,16,17,18,19,20,15,16,17,18,19,20)) +
  labs(x = "Edad en" ~ italic("años"), y = "Altura dominante en" ~ italic("m"),color = "Finca", shape = "Finca") +
  # ggtitle("Alturas dominantes en la región Sur de Guatemala" ~ italic("Tectona grandis")) +
  theme_linedraw() +
  ylim(0, NA) +
  theme(legend.position = "right", 
        legend.text = element_text(size = 12, family = "Times New Roman"), legend.title = element_text(size = 14, family = "Times New Roman"))+
  #guides(color = guide_legend(override.aes = list(size = 5)))+
  theme(plot.title=element_text(family='Times New Roman', face='bold', colour='black', size=24),
        plot.subtitle = element_text(family='Times New Roman', face=1, colour='black', size=18),
        plot.caption = element_text(family='Times New Roman', face=1, colour='black', size=18))
Plot_schumacher

IS_chapman
Plot_chapman<- ggplot() +
  geom_smooth(data = IS_chapman, aes(x = `Edad en Años`, y = Pésimo), se = FALSE, color = "red") +
  geom_smooth(data = IS_chapman, aes(x = `Edad en Años`, y = Malo), se = FALSE, color = "#FFD700") +
  geom_smooth(data = IS_chapman, aes(x = `Edad en Años`, y = Medio), se = FALSE, color = "orange") +
  geom_smooth(data = IS_chapman, aes(x = `Edad en Años`, y = Bueno), se = FALSE, color = "#43CD80") +
  geom_smooth(data = IS_chapman, aes(x = `Edad en Años`, y = Excelente), se = FALSE, color = "darkgreen") +
  geom_point(data = Clean_BD_teca, aes(x = Edad_a, y = Altura_m, colour = `NS. Sitio`, shape = `NS. Sitio` ), size = 3) +
  scale_color_manual(name = "Fincas", 
                     values = c( "#FFD700", "#9E0142", "red", "#E7298A", "#43CD80", "#377EB8",
                                 "#BC80BD", "blue","black", "#BC80BD", "#E7298A", "#252525",
                                 "#E41A1C", "#377EB8", "#4DAF4A", "red", "#A65628")) +
  scale_shape_manual(name = "Fincas", 
                     values = c(15,16,17,18,19,20,15,16,17,18,19,20,15,16,17,18,19,20)) +
  labs(x = "Edad en" ~ italic("años"), y = "Altura dominante en" ~ italic("m"),color = "Finca", shape = "Finca") +
  # ggtitle("Alturas dominantes en la región Sur de Guatemala" ~ italic("Tectona grandis")) +
  theme_linedraw() +
  ylim(0, NA) +
  theme(legend.position = "right", 
        legend.text = element_text(size = 12, family = "Times New Roman"), legend.title = element_text(size = 14, family = "Times New Roman"))+
  #guides(color = guide_legend(override.aes = list(size = 5)))+
  theme(plot.title=element_text(family='Times New Roman', face='bold', colour='black', size=24),
        plot.subtitle = element_text(family='Times New Roman', face=1, colour='black', size=18),
        plot.caption = element_text(family='Times New Roman', face=1, colour='black', size=18))
Plot_chapman

library(cowplot)
plot_grid(
  Plot_Chapma, Plot_Weibull, Plot_Schuma,#pII_04.03, pII_04.04,
  labels = c("A.", "B.", "C."),
  #"C. Calidad del fuste de 11 a 15 años de edad", "D. Calidad del fuste de 17 a 20 años de edad"),
  nrow = 2,  # Una fila
  align = "h",  # Alinear horizontalmente
  label_fontfamily = "serif",
  label_fontface = "plain",
  label_colour = "black",
  label_x = 1, label_y = 1,
  hjust = -0.5, vjust = -0.5,
  rel_heights  = c(1, 1)) #+
  #draw_label(
    #label = "Comparación de curvas de indice de sitio en función de 3 modelos para Tectona grandis",
    #x = 0, y = 1,  # PosiciÃ³n del tÃ­tulo
    #fontfamily = "Times New Roman", fontface = "italic", size = 24,  # Estilo del tÃ­tulo
    #hjust = 0, vjust = 1.5  # Ajuste vertical
  #)

#boxplot___

Tablas_de_crecimiento <- read_excel("C:\\Users\\jolum\\Downloads\\Tablas de crecimiento.xlsx", 
                                    sheet = "Hoja3")


# Convertir la columna de categorías en un factor y ordenar las categorías
Tablas_de_crecimiento$Categoria <- factor(Tablas_de_crecimiento$Categoria, levels = c("Pésimo", "Malo", "Medio", "Bueno", "Excelente"))

# Crear el gráfico de box plot con las cajas más delgadas y agregar la media
box_plot <- ggplot(Tablas_de_crecimiento, aes(x = Categoria, y = IS, fill = Categoria)) +
  geom_boxplot(width = 0.5) +  # Ajustar el ancho de las cajas
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +  # Agregar la media como un punto
  stat_summary(fun.data = function(x) { 
    return(data.frame(y = mean(x), label = round(mean(x), 2))) 
  }, geom = "text", aes(label = ..label..), vjust = -0.5, hjust = -0.8, color = "black", size = 4) +  # Etiquetar la media
  labs( x = "Categoría",
       y = "IS (m)") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "orange", "yellow", "lightgreen", "darkgreen")) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 14, family = "Times New Roman"),  # Ajustar el tamaño de la letra del eje x
        axis.title.y = element_text(size = 14, family = "Times New Roman"),  # Ajustar el tamaño de la letra del eje y
        axis.text.x = element_text(size = 12, family = "Times New Roman"),   # Ajustar el tamaño de los textos del eje x
        axis.text.y = element_text(size = 12, family = "Times New Roman"))   # Ajustar el tamaño de los textos del eje y

# Mostrar el gráfico
print(box_plot)


#_____ grafico de columnas 
# Instalar y cargar las librerías necesarias
# Instalar y cargar las librerías necesarias
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("reshape2")) install.packages("reshape2")
library(reshape2)

# Crear el data frame con los datos proporcionados
datos <- data.frame(
  Categoria = c("pesimo", "Malo", "Medio", "Bueno", "Excelente"),
  Schumacher = c(0, 0, 39, 59, 2),
  Chapman_Richards = c(1, 4, 36, 29, 31)
)

# Convertir 'Categoria' en factor con el orden deseado
datos$Categoria <- factor(datos$Categoria, levels = c("pesimo", "Malo", "Medio", "Bueno", "Excelente"))

# Transformar el data frame a formato largo
datos_largos <- melt(datos, id.vars = "Categoria", variable.name = "Modelo", value.name = "Porcentaje")

# Crear el gráfico de columnas combinado
grafico <- ggplot(datos_largos, aes(x = Categoria, y = Porcentaje, fill = Modelo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +  # Gráfico de barras con barras agrupadas
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5, family = "Times New Roman") +  # Agregar etiquetas de porcentaje
  labs(
       x = "Categoría",
       y = "Porcentaje") +
  theme_minimal() +
  scale_fill_manual(values = c("Schumacher" = "red", "Chapman_Richards" = "blue")) +
  theme(legend.title = element_blank(),  # Eliminar título de la leyenda
        legend.position = "bottom",
        axis.title.x = element_text(size = 14, family = "Times New Roman"),
        axis.title.y = element_text(size = 14, family = "Times New Roman"),
        axis.text.x = element_text(size = 12, family = "Times New Roman"),
        axis.text.y = element_text(size = 12, family = "Times New Roman"))

# Mostrar el gráfico
print(grafico)




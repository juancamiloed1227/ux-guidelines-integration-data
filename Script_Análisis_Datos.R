#Instalar librerias
install.packages("dplyr")
install.packages("openxlsx")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("likert")

#Directorio de trabajo
setwd("D:/Users/jaospina/Dropbox/ACTIVIDADES/ACTIVIDADES_2025-01/Análisis_Artículo_Carlos_JuanCamilo")


#Cargar librerias
library(dplyr)
library(openxlsx)
library(tidyr)
library(ggplot2)
library(likert)

#Cargar datos
datos = read.xlsx("Datos_Análisis.xlsx", sheet = 1)
names(datos)

names(datos) <- c(
  "Group",
  "Frequent Use",
  "Perceived Complexity",
  "Ease of Use",
  "Response Accuracy",
  "Task Speed",
  "User Trust",
  "Quick Learning",
  "Response Clarity",
  "Information Value",
  "Recommendation"
)


# Definir niveles en inglés
likert_levels <- c("Strongly Disagree", 
                   "Disagree", 
                   "Neither Agree nor Disagree", 
                   "Agree", 
                   "Strongly Agree")

# Supón que 'datos' es tu data.frame y la primera columna es 'Grupo'
# Aplica la conversión a todas las columnas excepto 'Grupo'
datos_factor <- datos  # copiar el original

for (col in colnames(datos)[-1]) {
  datos_factor[[col]] <- factor(datos[[col]],
                                levels = 1:5,
                                labels = likert_levels)
}

respuestas <- datos_factor[,-1]
grupo <- datos_factor$Group

# Crear objeto likert con agrupación
likert_obj <- likert(items = respuestas, grouping = grupo)

pdf("Figure2_Likert.pdf", width = 12, height = 8)
plot(likert_obj)  
dev.off()

# Convert to long format
data_long <- datos %>%
  pivot_longer(cols = -Group, names_to = "Variable", values_to = "Value")

# Coefficient of variation function
cv <- function(x) sd(x) / mean(x) * 100

# Calculate summary statistics by group
summary_stats <- data_long %>%
  group_by(Group) %>%
  summarise(across(
    .cols = where(is.numeric),
    .fns = list(
      mean = mean,
      median = median,
      SD = sd,
      CV = cv
    ),
    .names = "{.col}_{.fn}"
  ))

# Display the summary table
print(summary_stats)


#



# Boxplots por ítem
pdf("Figure1_Boxplots_by_Item.pdf", width = 12, height = 8)
ggplot(data_long, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(
    title = "",
    x = "Group",
    y = "Score"
  ) +
  theme_minimal()
dev.off()


vars <- names(datos)[-1]  # excluye la columna 'Grupo'

# Aplicar test de Wilcoxon por variable
pvalores <- sapply(vars, function(var) {
  wilcox.test(datos[[var]] ~ datos$Grupo)$p.value
})


# Resultados en tabla
resultado <- data.frame(Variable = vars, P_Valor = round(pvalores, 4))
resultado <- resultado[order(resultado$P_Valor), ]  # Orden por significancia



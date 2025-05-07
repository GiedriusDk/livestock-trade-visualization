library(tidyverse)
library(eurostat)
library(leaflet)
library(sf)
library(scales)
library(cowplot)
library(ggthemes)
library(ggplot2)
library(ggpubr)

# Read the Excel file
excel_data <- readxl::read_excel("gyvuliai_01.xlsx")

# Get Eurostat geospatial data for Europe
SHP_0 <- get_eurostat_geospatial(resolution = 10, nuts_level = 0, year = 2021)

# Define mapping between country names in Excel and Eurostat data
eu_salys <- c("VOKIETIJOS FEDERACINĖ RESPUBLIKA" = "Deutschland",
                     "PRANCŪZIJOS RESPUBLIKA" = "France",
                     "LATVIJOS RESPUBLIKA" = "Latvija",
                     "LENKIJOS RESPUBLIKA" = "Polska",
                     "ESTIJOS RESPUBLIKA" = "Eesti",
                     "SUOMIJOS RESPUBLIKA" = "Suomi/Finland",
                     "LIETUVOS RESPUBLIKA" = "Lietuva",
                     "ČEKIJOS RESPUBLIKA" = "Česko",
                     "ITALIJOS RESPUBLIKA" = "Italia",
                     "BULGARIJOS RESPUBLIKA" = "Bulgaria",
                     "NYDERLANDŲ KARALYSTĖ" = "Nederland",
                     "LIUKSEMBURGO DIDŽIOJI HERCOGYSTĖ" = "Luxembourg",  
                     "VENGRIJOS RESPUBLIKA" = "Magyarország",
                     "ISPANIJOS KARALYSTĖ" = "España",
                     "PORTUGALIJOS RESPUBLIKA" = "Portugal",
                     "AIRIJA" = "Éire/Ireland",
                     "ŠVEDIJOS KARALYSTĖ" = "Sverige",
                     "SLOVĖNIJOS RESPUBLIKA" = "Slovenija",
                     "RUMUNIJA" = "România",
                     "DANIJOS KARALYSTĖ" = "Danmark",
                     "SLOVAKIJOS RESPUBLIKA" = "Slovensko",
                     "GRAIKIJOS RESPUBLIKA" = "Elláda")

# Define thresholds for coloring
pirmas_limitas <- 10
antras_limitas <- 50
trecias_limitas <- 100
ketvirtas_limitas <- 500
penktas_limitas <- 1000

# Define function to assign colors based on livestock counts
assign_color <- function(count) {
  if (count >= 1 && count < pirmas_limitas)
    return("lightgreen")
  else if (count >= pirmas_limitas && count < antras_limitas) {
    return("limegreen")
  } else if (count >= antras_limitas && count < trecias_limitas) {
    return("darkgreen")
  } else if (count >= trecias_limitas && count < ketvirtas_limitas) {
    return("lightblue")
  } else if (count >= ketvirtas_limitas && count < penktas_limitas) {
    return("cyan")
  } else if (count >= penktas_limitas) {
    return("darkblue")
  }else {
    return(NA)
  }
}
spalvos_importas = c("darkblue", "cyan", "lightblue", "darkgreen", "limegreen", "lightgreen")

etikete <- c("+1000", "500-1000", "200-500", "50-100", "10-50", "1-10")


# Aggregate the data by country and calculate total livestock counts
filtruota_imp_2022 <- excel_data %>%
  filter(`Šalies pavadinimas` %in% names(eu_salys) &
           `Ūkinių gyvūnų eksporto, importo metai` == 2022 &
           `Ūkinių gyvūnų eksporto, importo požymis` == "Importas") %>%
  group_by(country = eu_salys[`Šalies pavadinimas`]) %>%
  summarise(total_livestock_counts = sum(`Ūkinių gyvūnų skaičius`))

# Add a column for colors based on livestock counts
filtruota_imp_2022 <- filtruota_imp_2022 %>%
  mutate(color = sapply(total_livestock_counts, assign_color))

# Merge the Eurostat geospatial data with aggregated data
data_imp_2022 <- SHP_0 %>%
  left_join(filtruota_imp_2022, by = c("NAME_LATN" = "country"))


plot1 <- data_imp_2022 %>%
  ggplot() +
  geom_sf(aes(fill = color)) +
  theme_minimal() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  labs(title = "Ūkinių gyvūnų importas 2022 metais (ES šalyse)") +
  scale_fill_manual(values = spalvos_importas,
                    breaks = spalvos_importas,
                    labels = etikete) +
  guides(fill = guide_legend(title = "Ūkinių gyvūnų sk.")) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )



filtruota_imp_2019 <- excel_data %>%
  filter(`Šalies pavadinimas` %in% names(eu_salys) &
           `Ūkinių gyvūnų eksporto, importo metai` == 2019 &
           `Ūkinių gyvūnų eksporto, importo požymis` == "Importas") %>%
  group_by(country = eu_salys[`Šalies pavadinimas`]) %>%
  summarise(total_livestock_counts = sum(`Ūkinių gyvūnų skaičius`))

filtruota_imp_2019 <- filtruota_imp_2019 %>%
  mutate(color = sapply(total_livestock_counts, assign_color))


data_imp_2019 <- SHP_0 %>%
  left_join(filtruota_imp_2019, by = c("NAME_LATN" = "country"))

plot2 <- data_imp_2019 %>%
  ggplot() +
  geom_sf(aes(fill = color)) +
  theme_minimal() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  labs(title = "Ūkinių gyvūnų importas 2019 metais (ES šalyse)") +
  scale_fill_manual(values = spalvos_importas,
                    breaks = spalvos_importas,
                    labels = etikete) +
  guides(fill = guide_legend(title = "Ūkinių gyvūnų sk.")) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )




assign_color_ex <- function(count) {
  if (count >= 1 && count < pirmas_limitas)
    return("yellow")
  else if (count >= pirmas_limitas && count < antras_limitas) {
    return("orange")
  } else if (count >= antras_limitas && count < trecias_limitas) {
    return("magenta")
  } else if (count >= trecias_limitas && count < ketvirtas_limitas) {
    return("purple4")
  } else if (count >= ketvirtas_limitas && count < penktas_limitas) {
    return("red")
  } else if (count >= penktas_limitas) {
    return("darkred")
  }else {
    return(NA)
  }
}
spalvos_eksportas = c("darkred", "red", "purple4", "magenta", "orange", "yellow")


filtruota_eks_2022 <- excel_data %>%
  filter(`Šalies pavadinimas` %in% names(eu_salys) &
           `Ūkinių gyvūnų eksporto, importo metai` == 2022 &
           `Ūkinių gyvūnų eksporto, importo požymis` == "Eksportas") %>%
  group_by(country = eu_salys[`Šalies pavadinimas`]) %>%
  summarise(total_livestock_counts = sum(`Ūkinių gyvūnų skaičius`))

filtruota_eks_2022 <- filtruota_eks_2022 %>%
  mutate(color = sapply(total_livestock_counts, assign_color_ex))

data_eks_2022 <- SHP_0 %>%
  left_join(filtruota_eks_2022, by = c("NAME_LATN" = "country"))


plot3 <- data_eks_2022 %>%
  ggplot() +
  geom_sf(aes(fill = color)) +
  theme_minimal() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  labs(title = "Ūkinių gyvūnų eksportas 2022 metais (ES šalyse)") +
  scale_fill_manual(values = spalvos_eksportas,
                    breaks = spalvos_eksportas,
                    labels = etikete) +
  guides(fill = guide_legend(title = "Ūkinių gyvūnų sk.")) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )




filtruota_eks_2019 <- excel_data %>%
  filter(`Šalies pavadinimas` %in% names(eu_salys) &
           `Ūkinių gyvūnų eksporto, importo metai` == 2019 &
           `Ūkinių gyvūnų eksporto, importo požymis` == "Eksportas") %>%
  group_by(country = eu_salys[`Šalies pavadinimas`]) %>%
  summarise(total_livestock_counts = sum(`Ūkinių gyvūnų skaičius`))

filtruota_eks_2019 <- filtruota_eks_2019 %>%
  mutate(color = sapply(total_livestock_counts, assign_color_ex))

data_eks_2019 <- SHP_0 %>%
  left_join(filtruota_eks_2019, by = c("NAME_LATN" = "country"))

plot4 <- data_eks_2019 %>%
  ggplot() +
  geom_sf(aes(fill = color)) +
  theme_minimal() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  labs(title = "Ūkinių gyvūnų eksportas 2019 metais (ES šalyse)") +
  scale_fill_manual(values = spalvos_eksportas,
                    breaks = spalvos_eksportas,
                    labels = etikete) +
  guides(fill = guide_legend(title = "Ūkinių gyvūnų sk.")) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )




ggarrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)




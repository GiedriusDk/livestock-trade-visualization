# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)  # for ggarrange function

# Read the Excel file
data <- read_excel("gyvuliai_01.xlsx")

# Define country names
Latvija <- c("LATVIJOS RESPUBLIKA")
Lenkija <- c("LENKIJOS RESPUBLIKA")
Vokietija <- c("VOKIETIJOS FEDERACINĖ RESPUBLIKA")

# Filter and summarize data for Lenkija (Poland)
filtered_data_exportas_lenkija <- data %>%
  filter(`Ūkinių gyvūnų eksporto, importo metai` >= 2019 & 
           `Ūkinių gyvūnų eksporto, importo metai` <= 2022 &
           `Ūkinių gyvūnų eksporto, importo požymis` == "Eksportas" &
           `Šalies pavadinimas` %in% Lenkija) %>%
  group_by(`Ūkinių gyvūnų eksporto, importo metai`) %>%
  summarise(total_animals = sum(`Ūkinių gyvūnų skaičius`))

filtered_data_importas_lenkija <- data %>%
  filter(`Ūkinių gyvūnų eksporto, importo metai` >= 2019 & 
           `Ūkinių gyvūnų eksporto, importo metai` <= 2022 &
           `Ūkinių gyvūnų eksporto, importo požymis` == "Importas" &
           `Šalies pavadinimas` %in% Lenkija) %>%
  group_by(`Ūkinių gyvūnų eksporto, importo metai`) %>%
  summarise(total_animals = sum(`Ūkinių gyvūnų skaičius`))

# Combine the import and export data for Lenkija
combined_data_lenkija <- bind_rows(
  filtered_data_importas_lenkija %>% mutate(type = "Importas"),
  filtered_data_exportas_lenkija %>% mutate(type = "Eksportas")
)

# Filter and summarize data for Latvija (Latvia)
filtered_data_exportas_latvija <- data %>%
  filter(`Ūkinių gyvūnų eksporto, importo metai` >= 2019 & 
           `Ūkinių gyvūnų eksporto, importo metai` <= 2022 &
           `Ūkinių gyvūnų eksporto, importo požymis` == "Eksportas" &
           `Šalies pavadinimas` %in% Latvija) %>%
  group_by(`Ūkinių gyvūnų eksporto, importo metai`) %>%
  summarise(total_animals = sum(`Ūkinių gyvūnų skaičius`))

filtered_data_importas_latvija <- data %>%
  filter(`Ūkinių gyvūnų eksporto, importo metai` >= 2019 & 
           `Ūkinių gyvūnų eksporto, importo metai` <= 2022 &
           `Ūkinių gyvūnų eksporto, importo požymis` == "Importas" &
           `Šalies pavadinimas` %in% Latvija) %>%
  group_by(`Ūkinių gyvūnų eksporto, importo metai`) %>%
  summarise(total_animals = sum(`Ūkinių gyvūnų skaičius`))

# Combine the import and export data for Latvija
combined_data_latvija <- bind_rows(
  filtered_data_importas_latvija %>% mutate(type = "Importas"),
  filtered_data_exportas_latvija %>% mutate(type = "Eksportas")
)

# Filter and summarize data for Vokietija (Germany)
filtered_data_exportas_vokietija <- data %>%
  filter(`Ūkinių gyvūnų eksporto, importo metai` >= 2019 & 
           `Ūkinių gyvūnų eksporto, importo metai` <= 2022 &
           `Ūkinių gyvūnų eksporto, importo požymis` == "Eksportas" &
           `Šalies pavadinimas` %in% Vokietija) %>%
  group_by(`Ūkinių gyvūnų eksporto, importo metai`) %>%
  summarise(total_animals = sum(`Ūkinių gyvūnų skaičius`))

filtered_data_importas_vokietija <- data %>%
  filter(`Ūkinių gyvūnų eksporto, importo metai` >= 2019 & 
           `Ūkinių gyvūnų eksporto, importo metai` <= 2022 &
           `Ūkinių gyvūnų eksporto, importo požymis` == "Importas" &
           `Šalies pavadinimas` %in% Vokietija) %>%
  group_by(`Ūkinių gyvūnų eksporto, importo metai`) %>%
  summarise(total_animals = sum(`Ūkinių gyvūnų skaičius`))

# Combine the import and export data for Vokietija
combined_data_vokietija <- bind_rows(
  filtered_data_importas_vokietija %>% mutate(type = "Importas"),
  filtered_data_exportas_vokietija %>% mutate(type = "Eksportas")
)


# Create the density plot for Lenkija (Poland)
plot_lenkija <- ggplot(combined_data_lenkija, aes(x = `Ūkinių gyvūnų eksporto, importo metai`, y = total_animals, fill = type)) +
  geom_density(stat = "identity", alpha = 0.5, position = 'identity') +
  scale_fill_manual(values = c("Eksportas" = "red", "Importas" = "blue")) +
  labs(title = "Lenkijos eksportas ir importas",
       x = "Metai",
       y = "Gyvūnų skaičius",
       fill = "Tipas") +
  scale_y_continuous(breaks = seq(0, max(combined_data_lenkija$total_animals), by = 3000)) +
  theme_minimal()

# Create the density plot for Latvija (Latvia)
plot_latvija <- ggplot(combined_data_latvija, aes(x = `Ūkinių gyvūnų eksporto, importo metai`, y = total_animals, fill = type)) +
  geom_density(stat = "identity", alpha = 0.5, position = 'identity') +
  scale_fill_manual(values = c("Eksportas" = "red", "Importas" = "blue")) +
  labs(title = "Latvijos eksportas ir importas",
       x = "Metai",
       y = "Gyvūnų skaičius",
       fill = "Tipas") +
  scale_y_continuous(breaks = seq(0, max(combined_data_latvija$total_animals), by = 3000)) +
  theme_minimal()

# Create the density plot for Vokietija (Germany)
plot_vokietija <- ggplot(combined_data_vokietija, aes(x = `Ūkinių gyvūnų eksporto, importo metai`, y = total_animals, fill = type)) +
  geom_density(stat = "identity", alpha = 0.5, position = 'identity') +
  scale_fill_manual(values = c("Eksportas" = "red", "Importas" = "blue")) +
  labs(title = "Vokietijos eksportas ir importas",
       x = "Metai",
       y = "Gyvūnų skaičius",
       fill = "Tipas") +
  scale_y_continuous(breaks = seq(0, max(combined_data_vokietija$total_animals), by = 1000)) +
  theme_minimal()



# Arrange the three plots side by side
ggarrange(plot_lenkija, plot_latvija, plot_vokietija, ncol = 3, nrow = 1)

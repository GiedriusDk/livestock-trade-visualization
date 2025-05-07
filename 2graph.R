# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)  # for ggarrange function

# Read the Excel file
data <- read_excel("gyvuliai_01.xlsx")

EU_salys <- c("VOKIETIJOS FEDERACINĖ RESPUBLIKA",
              "PRANCŪZIJOS RESPUBLIKA",
              "LATVIJOS RESPUBLIKA",
              "LENKIJOS RESPUBLIKA",
              "ESTIJOS RESPUBLIKA",
              "SUOMIJOS RESPUBLIKA",
              "LIETUVOS RESPUBLIKA",
              "ČEKIJOS RESPUBLIKA",
              "ITALIJOS RESPUBLIKA",
              "BULGARIJOS RESPUBLIKA",
              "NYDERLANDŲ KARALYSTĖ",
              "LIUKSEMBURGO DIDŽIOJI HERCOGYSTĖ",  
              "VENGRIJOS RESPUBLIKA",
              "ISPANIJOS KARALYSTĖ",
              "PORTUGALIJOS RESPUBLIKA",
              "AIRIJA",
              "ŠVEDIJOS KARALYSTĖ",
              "SLOVĖNIJOS RESPUBLIKA",
              "RUMUNIJA",
              "DANIJOS KARALYSTĖ",
              "SLOVAKIJOS RESPUBLIKA",
              "GRAIKIJOS RESPUBLIKA")


# Filter data for the year 2019, importo požymis, and calculate sum of animals for each age group
filtered_data_exportas <- data %>%
  filter(`Ūkinių gyvūnų eksporto, importo metai` >= 2019 & 
           `Ūkinių gyvūnų eksporto, importo metai` <= 2022 &
           `Ūkinių gyvūnų eksporto, importo požymis` == "Eksportas" &
           `Šalies pavadinimas` %in% EU_salys) %>%
  group_by(`Ūkinių gyvūnų eksporto, importo metai`, `Ūkinių gyvūnų amžiaus grupė`) %>%
  summarise(total_animals = sum(`Ūkinių gyvūnų skaičius`))

# Custom labels and colors
etikete <- c("1 m. ir daugiau", "6 mėn. - 1 m.", "0-6 mėn.")
spalvos <- c("green", "blue", "red")

# Plot with custom legend labels
plot1 <- ggplot(filtered_data_exportas, aes(x=`Ūkinių gyvūnų eksporto, importo metai`, y=total_animals, color=`Ūkinių gyvūnų amžiaus grupė`)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = total_animals), position = position_dodge(0.5), vjust = -0.5) +
  labs(title = "Europos Sąjungos ūkinių gyvūlių eksportas",
       x = "Ūkinių gyvūnų eksporto, importo metai",
       y = "Ūkinių gyvūnų skaičius",
       color = "Ūkinių gyvūnų amžiaus grupė") +
  scale_color_manual(values = spalvos,
                     breaks = etikete,
                     labels = etikete) +
  theme_minimal()

# Print the plot
print(plot1)


# Filter data for the year 2019, importo požymis, and calculate sum of animals for each age group
filtered_data_importas <- data %>%
  filter(`Ūkinių gyvūnų eksporto, importo metai` >= 2019 & 
           `Ūkinių gyvūnų eksporto, importo metai` <= 2022 &
           `Ūkinių gyvūnų eksporto, importo požymis` == "Importas" &
           `Šalies pavadinimas` %in% EU_salys) %>%
  group_by(`Ūkinių gyvūnų eksporto, importo metai`, `Ūkinių gyvūnų amžiaus grupė`) %>%
  summarise(total_animals = sum(`Ūkinių gyvūnų skaičius`))

# Plot the second graph for "importas"
plot2 <- ggplot(filtered_data_importas, aes(x=`Ūkinių gyvūnų eksporto, importo metai`, y=total_animals, color=`Ūkinių gyvūnų amžiaus grupė`)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = total_animals), position = position_dodge(0.5), vjust = -0.5) +  # Adjust the position of labels
  labs(title = "Europos Sąjungos ūkinių gyvūlių importas",
       x = "Ūkinių gyvūnų eksporto, importo metai",
       y = "Ūkinių gyvūnų skaičius",
       color = "Ūkinių gyvūnų amžiaus grupė") +
  scale_color_manual(values = spalvos,
                     breaks = etikete,
                     labels = etikete) +
  theme_minimal()



# Arrange both graphs using ggarrange
ggarrange(plot1, plot2, ncol = 2, nrow = 1)


filtered <- data %>%
  filter(`Ūkinių gyvūnų eksporto, importo metai` == 2019 & 
           `Ūkinių gyvūnų eksporto, importo požymis` == "Eksportas" &
           `Šalies pavadinimas` %in% EU_salys)
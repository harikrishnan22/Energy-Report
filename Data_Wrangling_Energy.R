library(tidyr)
library(magrittr)
library(dplyr)

# Energy comparison - most economic countries
energy <- read.csv("c:/Energy Visualization/Energy_Comparison_Economic.csv", header = TRUE, sep = ",", check.names = FALSE)

df <- energy %>% select(-c(3))

colnames(df)[1] <- "Country"
colnames(df)[2] <- "Product"

energy_df <- df %>% pivot_longer(-c(Country, Product), names_to = "Year", values_to = "Production (Quad)")

energy_final <- energy_df %>% pivot_wider(names_from = Product, values_from = `Production (Quad)`)

energy_final[3] <- energy_final[3] * 0.000039652608749183
energy_final[4] <- energy_final[4] * 0.000039652608749183
energy_final[5] <- energy_final[5] * 0.000039652608749183
energy_final[6] <- energy_final[6] * 0.000039652608749183
energy_final[7] <- energy_final[7] * 0.000039652608749183

write.csv(energy_final, "Energy_Comparison_Economic_Flourish.csv")

# Renewable energy comparison - most economic countries
renewable_world <- read.csv("c:/Energy Visualization/Renewable_Economic.csv", header = TRUE, sep = ",", check.names = FALSE)

colnames(renewable_world)[1] <- "Country"

renewable_world[-c(1, 2, 3)] <- renewable_world[-c(1, 2, 3)] * 0.000039652608749183

write.csv(renewable_world, "Renewable_Economic_Flourish.csv")


# Renewable energy comparison - US
renewable_us <- read.csv("c:/Energy Visualization/Renewable_US.csv", header = TRUE, sep = ",", check.names = FALSE, na.strings = "N/A")

colnames(renewable_us)[1] <- "Year"

write.csv(renewable_us, "Renewable_US_Flourish.csv")

# Pie
renewable_us <- read.csv("c:/Energy Visualization/Renewable_US.csv", header = TRUE, sep = ",", check.names = FALSE, na.strings = "N/A")

colnames(renewable_us)[1] <- "Year"

renewable_df <- renewable_us %>% pivot_longer(-c(Year), names_to = "Type", values_to = "Production (Quad)")

write.csv(renewable_df, "Renewable_US_Pie_Flourish.csv")

# Renewable energy flow - us
flow <- read.csv("c:/Energy Visualization/Energy_Flow.csv", header = TRUE, sep = ",", check.names = FALSE, na.strings = "")

flow <- flow[-1]

flow_df <- flow %>%
  group_by(Description) %>%
  summarise(Consumption = mean(Value, na.rm = TRUE)) %>%
  drop_na()

flow_df <- flow_df %>%
  separate(col = Description, into = c("Source", "Destination"), sep = "Consumed by the") %>%
  drop_na()

write.csv(flow_df, "Renewable_Flow_Flourish.csv")

# Cost - us
cost <- read.csv("c:/Energy Visualization/Cost.csv", header = TRUE, sep = ",", check.names = FALSE, na.strings = "")

colnames(cost)[1] <- "Technology"

cost_df <- cost %>%
  group_by(Technology, Year) %>%
  summarise(Value = mean(`Calculated LCOE`, na.rm = TRUE)) %>%
  drop_na()

colnames(cost_df)[3] <- "LCOE ($/kWh)"

cost_final <- cost_df %>% pivot_wider(names_from = Technology, values_from = `LCOE ($/kWh)`)

write.csv(cost_final, "Renewable_Cost_Flourish.csv")

# electric vehicles - us
ev <- read.csv("c:/Energy Visualization/Electric_Vehicles.csv", header = TRUE, sep = ",", check.names = FALSE, na.strings = "Z")

colnames(ev)[1] <- "Type"

ev_df <- ev %>% pivot_longer(-Type, names_to = "Year", values_to = "Sales")

ev_final <- ev_df %>% pivot_wider(names_from = Type, values_from = Sales)

write.csv(ev_final, "Electic_Vehicles_Datawrapper.csv")

# Kütüphaneleri yükle
install.packages(c("httr", "readxl", "jsonlite", "dplyr", "tidyr", "openxlsx", "forecast", "wbstats", "ggplot2", "stringr", "rsdmx", "stats4","writexl"))
library(httr)
library(readxl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(openxlsx)
library(forecast)
library(wbstats)
library(ggplot2)
library(stringr)
library(rsdmx)
library(stats4)
library(writexl)


#####Energy Instidude (Geçmiş Kömür Talebi)
if (!file.exists("merged_narrow.csv")) {
  url <- "https://www.energyinst.org/__data/assets/file/0003/1540551/merged_narrow.csv"
  GET(url, write_disk("merged_narrow.csv", overwrite = TRUE))
}
df <- read.csv("merged_narrow.csv", stringsAsFactors = FALSE)
filtered_df <- df %>%
  filter(Country == "Total World", 
         Year >= 1965, Year <= 2023, 
         Var == "coalcons_ej") %>%
  select(Country, Year, Var, Value) %>%
  rename(Demand = Value)
wide_df <- filtered_df %>%
  pivot_wider(names_from = c(Country, Var), values_from = Demand)

###### Dünya Bankası (GDP Büyümesi and Nüfus Büyümesi)
if (!file.exists("world_bank_data.csv")) {
  raw_data <- wb_data(
    indicator = c("NY.GDP.MKTP.KD.ZG", "SP.POP.GROW"),
    country = "WLD",
    mrv = 59
  )
  
  wb_data <- raw_data %>%
    select(date, NY.GDP.MKTP.KD.ZG, SP.POP.GROW) %>% 
    rename(Year = date, GDP_Growth = NY.GDP.MKTP.KD.ZG, Population_Growth = SP.POP.GROW)
  
  write.csv(wb_data, "world_bank_data.csv", row.names = FALSE)  # Dosyayı kaydet
} else {
  wb_data <- read.csv("world_bank_data.csv")  # Dosya mevcutsa dosyadan oku
}

df_joined <- inner_join(wb_data, wide_df, by = "Year") %>%
  filter(Year >= 1965 & Year <= 2023)

#####OECD (GDP Büyümesi Projeksiyonları)
if (!file.exists("oecd_data.csv")) {
  url1 <- "https://sdmx.oecd.org/archive/rest/data/OECD,DF_EO114_LTB,/ESP+CHN+IND+USA+DEU+FRA+OECDG20.GDPVD.S0.A?endPeriod=2060&dimensionAtObservation=AllDimensions"
  oecd_data <- readSDMX(url1)
  write.csv(as.data.frame(oecd_data), "oecd_data.csv", row.names = FALSE)
} else {
  oecd_data <- read.csv("oecd_data.csv")
}

df_gdp <- as.data.frame(oecd_data)

df_gdpf_s0 <- df_gdp %>%
  filter(SCENARIO == "S0") %>%
  select(-SCENARIO) %>%
  group_by(TIME_PERIOD, LOCATION) %>%
  summarise(obsValue = mean(obsValue, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = LOCATION, values_from = obsValue) %>%
  rename(Year = TIME_PERIOD) %>%
  arrange(Year)

GDP_Growth_f <- df_gdpf_s0 %>%
  mutate(Year = as.numeric(Year)) %>%
  arrange(Year) %>%
  mutate(across(c(OECDG20), ~ (./lag(.) - 1) * 100, .names = "Growth_{.col}")) %>%
  rename(GDP_Growth = Growth_OECDG20)

##### Dünya Bankası   (Nüfus Büyümesi Projeksiyonları)
if (!file.exists("population_data.csv")) {
  url <- "https://api.worldbank.org/v2/country/WLD/indicator/SP.POP.GROW?source=40&format=json&date=2023:2050&per_page=100"
  response <- GET(url)
  data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  pop_df <- as.data.frame(data[[2]])[, c("countryiso3code", "date", "value")]
  colnames(pop_df) <- c("country", "Year", "Population_Growth")
  write.csv(pop_df, "population_data.csv", row.names = FALSE)
} else {
  pop_df <- read.csv("population_data.csv")
}

POP_GR_f <- pop_df %>% mutate(Year = as.numeric(Year))

future_years <- 2024:2030

df_joined_f <- inner_join(GDP_Growth_f, POP_GR_f, by = "Year") %>%
  filter(Year >= 2024 & Year <= 2030)

# Model ve Tahmin 
full_data <- df_joined %>%
  select(Year, GDP_Growth, Population_Growth, `Total World_coalcons_ej`) %>%
  rename(Demand = `Total World_coalcons_ej`)

xreg_full <- full_data %>%
  select(GDP_Growth, Population_Growth) %>%
  as.matrix()

xreg_future <- df_joined_f %>%
  select(GDP_Growth, Population_Growth) %>%
  as.matrix()

# ARIMA
ARIMA1 <- auto.arima(full_data$Demand, xreg = xreg_full)
# ETS
ETS1 <- ets(full_data$Demand)
# RW
RW1 <- rwf(full_data$Demand, drift = TRUE, h = length(future_years))

# Tahmin
Arima_f <- forecast(ARIMA1, h = length(future_years), xreg = xreg_future)
ETS_f <- forecast(ETS1, h = length(future_years))
RW_f <- forecast(RW1, h = length(future_years), drift = TRUE)

# Tahminlerin Ortalaması
mean_forecast <- data.frame(
  Year = future_years,
  Demand = rowMeans(cbind(
    as.numeric(Arima_f$mean),
    as.numeric(ETS_f$mean),
    as.numeric(RW_f$mean)
  )),
  Type = "Ortalama Tahmin"
)


# Grafik çizimi

plot_df <- bind_rows(
  full_data %>% select(Year, Demand) %>% mutate(Type = "Kömür Talebi (EJ)"),
  data.frame(Year = future_years, Demand = as.numeric(Arima_f$mean), Type = "Senaryo 1"),
  data.frame(Year = future_years, Demand = as.numeric(RW_f$mean), Type = "Senaryo 2"),
  data.frame(Year = future_years, Demand = as.numeric(ETS_f$mean), Type = "Senaryo 3"),
  mean_forecast
) %>%
  filter(Year >= 1995)  

p <- ggplot(plot_df, aes(x = Year, y = Demand, color = Type)) +
  geom_line(linewidth = 1) +
  labs(title = "Dünya Kömür Talebi: Geçmiş Görünüm ve Gelecek Projeksiyonları", x = "Yıl", y = "Talep (EJ)") +
  scale_x_continuous(breaks = seq(min(plot_df$Year), max(plot_df$Year), by = 1)) +
  theme_minimal() +
  theme(
    legend.position = "bottom", # Legend'ı alta al
    legend.text = element_text(size = 8), # Yazı boyutunu küçült
    legend.title = element_text(size = 8), # Başlık boyutunu küçült
    axis.text.x = element_text(size = 8) # X ekseni yazı boyutunu küçültür
  )
print(p)

plot_df1 <- bind_rows(
  full_data %>% select(Year, Demand) %>% mutate(Type = "Kömür Talebi (EJ)"),
  data.frame(Year = future_years, Demand = as.numeric(Arima_f$mean), Type = "Senaryo 1"),
  data.frame(Year = future_years, Demand = as.numeric(RW_f$mean), Type = "Senaryo 2"),
  data.frame(Year = future_years, Demand = as.numeric(ETS_f$mean), Type = "Senaryo 3"),
  mean_forecast
) %>%
  filter(Year >= 2020)  

p1 <- ggplot(plot_df1, aes(x = Year, y = Demand, color = Type)) +
  geom_line(linewidth = 1) +
  labs(title = "Dünya Kömür Talebi: Geçmiş Görünüm ve Gelecek Projeksiyonları", x = "Yıl", y = "Talep (EJ)") +
  scale_x_continuous(breaks = seq(min(plot_df1$Year), max(plot_df1$Year), by = 1)) +
  theme_minimal()
print(p1)

#veriyi dışarı aktar
openxlsx::write.xlsx(plot_df, "World_Coal_Forecast3.xlsx", colNames = TRUE)


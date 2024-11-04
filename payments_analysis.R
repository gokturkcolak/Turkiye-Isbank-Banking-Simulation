library(tidyverse)
library(readxl)
library(janitor)

payment_dataset <- read_excel("gorev2_veriseti.xlsx")
payment_dataset_clean <- payment_dataset %>% clean_names()
exchange_rates <- read_excel("gorev2_veriseti.xlsx", sheet = "Döviz Kurları")
exchange_rates_clean <- exchange_rates %>% clean_names()

unique(payment_dataset_clean$issuer_ulke_kodu)
glimpse(exchange_rates_clean)

# Döviz kuru sütununu yeniden adlandırıyoruz
exchange_rates_clean <- exchange_rates_clean %>% 
  select(doviz_kodu = usd, usd_kuru = x1_3)

# Döviz kurları veri setini ödeme veri seti ile birleştiriyoruz
data_usd <- payment_dataset_clean %>% 
  left_join(exchange_rates_clean, by = "doviz_kodu") %>% 
  mutate(islem_tutari_usd = islem_tutari_doviz_kodu_cinsinden * usd_kuru)

# Para birimlerinin kullanım sıklığını gösteren grafik
data_usd %>%
  count(doviz_kodu) %>%
  ggplot(aes(x = reorder(doviz_kodu, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Frequency of Currency Usage",
       x = "Currency Code",
       y = "Usage Frequency") +
  theme_minimal()

# Ülke kodlarının sıklığının hesaplandığı veri seti
country_frequency <- data_usd %>%
  group_by(issuer_ulke_kodu) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

# Ülke kodlarının sıklığını gösteren grafik
ggplot(country_frequency, aes(x = reorder(issuer_ulke_kodu, -frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Frequency of Country Codes",
       x = "Country Code",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


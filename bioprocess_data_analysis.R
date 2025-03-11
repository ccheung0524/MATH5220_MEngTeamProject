library(ggplot2)
library(tidyr)

library(readxl)
bioprocess_data <- read_excel("bioprocess_data.xlsx", sheet='Data')
View(bioprocess_data)

bioprocess_data_clean <- bioprocess_data

bioprocess_data_clean$yield <- as.numeric(as.character(bioprocess_data_clean$yield))
bioprocess_data_clean$titer <- as.numeric(as.character(bioprocess_data_clean$titer))
bioprocess_data_clean$oxygen <- as.numeric(as.character(bioprocess_data_clean$oxygen))

bioprocess_data_clean <- bioprocess_data_clean[
  !is.na(bioprocess_data_clean$yield) & !is.na(bioprocess_data_clean$titer) & 
    !is.na(bioprocess_data_clean$oxygen), 
]

bioprocess_data_clean$oxygen <- as.factor(bioprocess_data_clean$oxygen)

ggplot(bioprocess_data_clean, aes(x=yield, y=titer, color=media)) +
  geom_point() +
  xlim(0,2)+
  ylim(0,10)

#sum(is.na(bioprocess_data_clean$yield))
sum(is.na(bioprocess_data_clean$oxygen))

ggplot(bioprocess_data_clean, aes(x=media, y=yield, fill=media)) +
  geom_boxplot()+
  scale_y_log10()+
  labs(title='Yield vs Media',
       y='Yield (g/g)',
       x='Media Type')

ggplot(bioprocess_data_clean, aes(x=media, y=titer, fill=media)) +
  geom_boxplot()+
  scale_y_log10()+
  labs(title='Titer vs Media',
       y='Titer (g/L)',
       x='Media Type')

ggplot(bioprocess_data_clean, aes(x=oxygen, y=yield, fill=oxygen)) +
  geom_boxplot()+
  scale_y_log10()+
  labs(title='Oxygen vs Yield',
       y='Yield (g/g)',
       x='Oxygenation')

ggplot(bioprocess_data_clean, aes(x=oxygen, y=titer, fill=oxygen)) +
  geom_boxplot()+
  scale_y_log10()+
  labs(title='Oxygen vs Titer',
       y='Titer (g/L)',
       x='Oxygenation')

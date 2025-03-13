### MATH 5220 Final Project Spring 2025, Christopher Cheung, Matthew Guzman, and Rachel Wehrman ###
# available on GitHub: https://github.com/ccheung0524/MATH5220_MEngTeamProject 

## load libraries
library(ggplot2)
library(tidyr)
library(readxl)

## import data
bioprocess_data <- read_excel("bioprocess_data.xlsx", sheet='Data')
#View(bioprocess_data)

## clean data for ANOVA media vs titer

bioprocess_clean <- na.omit(bioprocess_data[, c("media", "titer", "oxygen")])

# bioprocess[!is.na(bioprocess_data$titer) & is.na(as.numeric(bioprocess_data$titer)), ]

bioprocess_clean$titer <- as.numeric(as.character(bioprocess_data$titer))
bioprocess_clean$yield <- as.numeric(as.character(bioprocess_data$yield))
bioprocess_clean$titer <- as.numeric(as.character(bioprocess_data$titer))


bioprocess_clean <- bioprocess_clean[
    !is.na(bioprocess_data_clean$oxygen), 
]
bioprocess_clean$oxygen <- as.factor(as.character(bioprocess_data$oxygen))

# bioprocess_clean_media <- as.factor(bioprocess_clean$media)

# run anova

bioprocess_anova_titer_media <- aov(titer ~ media, data = bioprocess_clean)

summary(bioprocess_anova_titer_media)

bioprocess_anova_yield_oxygen <- aov(yield ~ oxygen, data = bioprocess_clean)

summary(bioprocess_anova_yield_oxygen)


## clean data for plotting
bioprocess_data_clean <- bioprocess_data

bioprocess_data_clean$yield <- as.numeric(as.character(bioprocess_data_clean$yield))
bioprocess_data_clean$titer <- as.numeric(as.character(bioprocess_data_clean$titer))
bioprocess_data_clean$oxygen <- as.numeric(as.character(bioprocess_data_clean$oxygen))

# remove all n/a
bioprocess_data_clean <- bioprocess_data_clean[
  !is.na(bioprocess_data_clean$yield) & !is.na(bioprocess_data_clean$titer) & 
    !is.na(bioprocess_data_clean$oxygen), 
]



# for loop to replace 1,2,3, with oxygenation level
# initialize temp variable
oxygenation <- 0

for (i in 1:nrow(bioprocess_data_clean)){
  oxygenation <- bioprocess_data_clean$oxygen[i]
  if (oxygenation == 1){
    temp_name <- 'Aerobic'
  } else if (oxygenation == 2) {
    temp_name <- 'Anaerobic'
  } else if (oxygenation == 3) {
    temp_name <- 'Microaerobic'
  }
  bioprocess_data_clean$oxygen[i] <- temp_name
  
}

bioprocess_data_clean$oxygen <- as.factor(bioprocess_data_clean$oxygen)
## plot data
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


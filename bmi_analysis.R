# Read BMI data
bmi_male <- read.csv('indicator_BMI-male-ASM.csv')

# Rename column labels
colnames(bmi_male) <- c("Country", 1980:2008)

# Install reshape2 package to reshape the bmi data frame
library(reshape2)
reshaped_bmi <- melt(bmi_male)
colnames(reshaped_bmi) <- c("Country", "Year", "BMI")

# Create line graph of year versus bmi
ggplot(aes(x = Year, y = BMI, group = Country, color = Country), data = reshaped_bmi) +
  geom_point() + geom_line()
ggsave('year_vs_bmi.png')

# Provide country labels adjacent to each line to identify the top 3 countries
# with the consistently high rate of BMI
ggplot(aes(x = Year, y = BMI, group = Country), data = reshaped_bmi) +
  geom_point() + geom_line() +
  geom_text(data = reshaped_bmi, aes(Year, BMI, label = Country))
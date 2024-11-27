# Load the tidyverse package (if not already loaded)
library(tidyverse)

# Import the CSV file as a dataframe called 'native'
native <- read_csv("102年度新北市總預算歲入來源別預算比較總表（經資併計）_export.csv")

# View the first few rows of the 'native' dataframe
glimpse(native)

# Rename the columns in the 'native' dataframe
native <- native %>%
  rename(
    field1 = 稅別,
    field2 = 金額一,
    field3 = 金額二,
    field4 = 金額三,
    field5 = 金額四,
  )

# Check the renamed dataframe
glimpse(native)

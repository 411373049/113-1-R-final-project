### Configure git with Rstudio ############################################

username <- rstudioapi::showPrompt("Github setup", "What is your Github username?")
email <- rstudioapi::showPrompt("Github setup", "What is your GitHub email? ")

username <- username |> stringr::str_remove_all(" ") 
email <- email |> stringr::str_remove_all(" ")

## set your user name and email:
usethis::use_git_config(user.name = username, user.email = email)

## create a personal access token for authentication:
usethis::create_github_token() 
## in case usethis version < 2.0.0: usethis::browse_github_token() (or even better: update usethis!)

## Note for Linux users:
## credentials::set_github_pat() (in line 34) might store your PAT in a memory cache that
## expires after 15 minutes or when the computer is rebooted. You thus may wish to do 
## extend the cache timeout to match the PAT validity period:
usethis::use_git_config(credential.helper="cache --timeout=2600000")

pat <- rstudioapi::showPrompt("Github setup","What is your personal access token? ")

## set personal access token:
credentials::set_github_pat(pat)

## or store it manually in '.Renviron':
# usethis::edit_r_environ()
## store your personal access token in the file that opens in your editor with:
## GITHUB_PAT=xxxyyyzzz
## and make sure '.Renviron' ends with a newline

# ----------------------------------------------------------------------------

#### 4. Restart R! ###########################################################

# ----------------------------------------------------------------------------

#### 5. Verify settings ######################################################

usethis::git_sitrep()

## Your username and email should be stated correctly in the output. 
## Also, the report shoud cotain something like:
## 'Personal access token: '<found in env var>''

## If you are still having troubles, read the output carefully.
## It might be that the PAT is still not updated in your `.Renviron` file.
## Call `usethis::edit_r_environ()` to update that file manually.


## THAT'S IT!



# Load the tidyverse package (if not already loaded)
library(tidyverse)

# Import the CSV file as a dataframe called 'native'
native <- read_csv("102年度新北市總預算歲入來源別預算比較總表（經資併計）_export.csv")

# View the first few rows of the 'native' dataframe
glimpse(native)

# Rename the columns in the 'native' dataframe
tidy_native <- native %>%
  rename(
    稅收項目 = field1,
    本年度預算數 = field2,
    上年度預算數 = field3,
    前年度決算數 = field4,
    本年度與上年度比較 = field5,
  )

# Check the renamed dataframe
glimpse(native)

# 建立一個數字向量，包含所有收入項目的數字
revenue_data <- c(
  稅課收入_房屋稅 = 9700000,
  稅課收入_契稅 = 2200000,
  稅課收入_娛樂稅 = 300000,
  稅課收入_使用牌照稅 = 7900000,
  稅課收入_印花稅 = 1080000,
  稅課收入_菸酒稅 = 1503694,
  稅課收入_土地稅 = 26460000,
  稅課收入_遺產及贈與稅 = 1725000,
  稅課收入_統籌分配稅 = 25914067,
  工程受益費收入_工程受益費收入 = 0,
  罰款及賠償收入_罰金罰鍰及怠金 = 3830373,
  罰款及賠償收入_沒入及沒收財物 = 19000,
  罰款及賠償收入_賠償收入 = 123048,
  規費收入_行政規費收入 = 1587396,
  規費收入_使用規費收入 = 1610510,
  財產收入_財產孳息 = 924910,
  財產收入_財產售價 = 2122985,
  財產收入_財產作價 = 10089337,
  財產收入_投資收回 = 0,
  財產收入_廢舊物資售價 = 26465,
  營業盈餘及事業收入_非營業特種基金賸餘繳庫 = 4433569,
  營業盈餘及事業收入_投資收益 = 19947,
  補助及協助收入_上級政府補助收入 = 26964475,
  捐獻及贈與收入_捐獻收入 = 1401237,
  自治稅捐收入_臨時稅課 = 0,
  其他收入_學雜費收入 = 0,
  其他收入_雜項收入 = 3730350
)

# 計算總收入
total_revenue <- sum(revenue_data)

# 打印總收入
total_revenue

# 計算每個收入項目佔總收入的比例
percentage <- (revenue_data / total_revenue) * 100

# 打印每個項目的比例
percentage

# 加載必要的庫
library(ggplot2)

# 將數據轉換為數據框
revenue_df <- data.frame(
  Category = names(revenue_data),
  Revenue = revenue_data
)

# 繪製條形圖
ggplot(revenue_df, aes(x = reorder(Category, -Revenue), y = Revenue)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "各項收入分佈", x = "收入類別", y = "金額") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 查找最大收入項目
max_revenue_category <- names(revenue_data)[which.max(revenue_data)]
max_revenue_value <- max(revenue_data)

# 查找最小收入項目
min_revenue_category <- names(revenue_data)[which.min(revenue_data)]
min_revenue_value <- min(revenue_data)

# 打印結果
max_revenue_category
max_revenue_value
min_revenue_category
min_revenue_value

# 建立收入數據向量
revenue_data <- c(
  稅課收入_房屋稅 = 8843000,
  稅課收入_契稅 = 3057000,
  稅課收入_娛樂稅 = 244843,
  稅課收入_使用牌照稅 = 7914000,
  稅課收入_印花稅 = 1059629,
  稅課收入_菸酒稅 = 1500003,
  稅課收入_土地稅 = 25521438,
  稅課收入_遺產及贈與稅 = 1512000,
  稅課收入_統籌分配稅 = 24590568,
  工程受益費收入_工程受益費收入 = 0,
  罰款及賠償收入_罰金罰鍰及怠金 = 3787097,
  罰款及賠償收入_沒入及沒收財物 = 20000,
  罰款及賠償收入_賠償收入 = 130500,
  規費收入_行政規費收入 = 1640502,
  規費收入_使用規費收入 = 1682581,
  財產收入_財產孳息 = 624209,
  財產收入_財產售價 = 2520325,
  財產收入_財產作價 = 220705,
  財產收入_投資收回 = 0,
  財產收入_廢舊物資售價 = 22518,
  營業盈餘及事業收入_非營業特種基金賸餘繳庫 = 9365330,
  營業盈餘及事業收入_投資收益 = 0,
  補助及協助收入_上級政府補助收入 = 38110139,
  捐獻及贈與收入_捐獻收入 = 1157092,
  自治稅捐收入_臨時稅課 = 0,
  其他收入_學雜費收入 = 127271,
  其他收入_雜項收入 = 5071799
)

# 計算總收入
total_revenue <- sum(revenue_data)

# 打印總收入
total_revenue

# 計算每個收入項目佔總收入的比例
percentage <- (revenue_data / total_revenue) * 100

# 打印每個項目的比例
percentage

# 加載 ggplot2 库
library(ggplot2)

# 將數據轉換為數據框
revenue_df <- data.frame(
  Category = names(revenue_data),
  Revenue = revenue_data
)

# 繪製條形圖
ggplot(revenue_df, aes(x = reorder(Category, -Revenue), y = Revenue)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "各項收入分佈", x = "收入類別", y = "金額") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 查找最大收入項目
max_revenue_category <- names(revenue_data)[which.max(revenue_data)]
max_revenue_value <- max(revenue_data)

# 查找最小收入項目
min_revenue_category <- names(revenue_data)[which.min(revenue_data)]
min_revenue_value <- min(revenue_data)

# 打印結果
max_revenue_category
max_revenue_value
min_revenue_category
min_revenue_value

# 建立收入數據向量
revenue_data <- c(
  稅課收入_房屋稅 = 857000,
  稅課收入_契稅 = -857000,
  稅課收入_娛樂稅 = 55157,
  稅課收入_使用牌照稅 = -14000,
  稅課收入_印花稅 = 20371,
  稅課收入_菸酒稅 = 3691,
  稅課收入_土地稅 = 938562,
  稅課收入_遺產及贈與稅 = 213000,
  稅課收入_統籌分配稅 = 1323499,
  工程受益費收入_工程受益費收入 = 0,
  罰款及賠償收入_罰金罰鍰及怠金 = 43276,
  罰款及賠償收入_沒入及沒收財物 = -1000,
  罰款及賠償收入_賠償收入 = -7452,
  規費收入_行政規費收入 = -53106,
  規費收入_使用規費收入 = -72071,
  財產收入_財產孳息 = 300701,
  財產收入_財產售價 = -397340,
  財產收入_財產作價 = 9868632,
  財產收入_投資收回 = 0,
  財產收入_廢舊物資售價 = 3947,
  營業盈餘及事業收入_非營業特種基金賸餘繳庫 = -4931761,
  營業盈餘及事業收入_投資收益 = 19947,
  補助及協助收入_上級政府補助收入 = -11145664,
  捐獻及贈與收入_捐獻收入 = 244145,
  自治稅捐收入_臨時稅課 = 0,
  其他收入_學雜費收入 = -127271,
  其他收入_雜項收入 = -1341449
)

# 計算總收入
total_revenue <- sum(revenue_data)

# 打印總收入
total_revenue

# 計算每個收入項目佔總收入的比例
percentage <- (revenue_data / total_revenue) * 100

# 打印每個項目的比例
percentage

# 加載 ggplot2 库
library(ggplot2)

# 將數據轉換為數據框
revenue_df <- data.frame(
  Category = names(revenue_data),
  Revenue = revenue_data
)

# 繪製條形圖
ggplot(revenue_df, aes(x = reorder(Category, -Revenue), y = Revenue)) +
  geom_bar(stat = "identity", fill = ifelse(revenue_df$Revenue >= 0, "skyblue", "salmon")) +
  labs(title = "各項收入與支出分佈", x = "收入類別", y = "金額") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 查找最大收入（或支出）項目
max_revenue_category <- names(revenue_data)[which.max(revenue_data)]
max_revenue_value <- max(revenue_data)

# 查找最小收入（或支出）項目
min_revenue_category <- names(revenue_data)[which.min(revenue_data)]
min_revenue_value <- min(revenue_data)

# 打印結果
max_revenue_category
max_revenue_value
min_revenue_category
min_revenue_value

# 查找所有負收入項目
negative_revenue <- revenue_data[revenue_data < 0]

# 打印所有負收入項目及其金額
negative_revenue

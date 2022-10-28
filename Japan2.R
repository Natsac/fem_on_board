library(tidyverse)
library(readxl)
library(dplyr)
library(modelsummary)
library("estimatr")

# 1. 必要なデータの読み込み

#必要なデータを選択

Japan0929 <- read_excel("Japan0929.xls", sheet = 2)

Japan0929a <- Japan0929 %>%
  drop_na("Company name")

Japan0929a <- Japan0929a %>%
  select(-"Market capitalization\nth USD\n2021", -"Market capitalization\nth USD\n2020", -"Market capitalization\nth USD\n2019", -"Market capitalization\nth USD\n2018", -"Market capitalization\nth USD\n2017", -"Market capitalization\nth USD\n2016", -"Market capitalization\nth USD\n2015", -"Number of\nemployees\nLast avail. yr")

Japan0825 <- read_excel("Japan0825.xls", sheet = 2)
Japan0825a <- Japan0825 %>%
  select("Market Cap.\nth USD\n2021", "Market Cap.\nth USD\n2020", "Market Cap.\nth USD\n2019", "Market Cap.\nth USD\n2018", "Market Cap.\nth USD\n2017", "Market Cap.\nth USD\n2016", "Market Cap.\nth USD\n2015", "Company name", "Number of\nemployees\n2021", "Number of\nemployees\n2020", 
         "Number of\nemployees\n2019", "Number of\nemployees\n2018", "Number of\nemployees\n2017", "Number of\nemployees\n2016", "Number of\nemployees\n2015")

JP1 <- Japan0929a %>%
  full_join(Japan0825a, by = c("Company name" = "Company name"))

JP1 <- distinct(JP1, .keep_all = FALSE) 


#役員情報（gender・appointment date）のデータを読み込む

JapanDMC1 <- read_excel("JapanDMC-1.xls", sheet = 2)
JapanDMC2 <- read_excel("JapanDMC-2.xls", sheet = 2)
JapanDMC3 <- read_excel("JapanDMC-3.xls", sheet = 2)
JapanDMC4 <- read_excel("JapanDMC-4.xls", sheet = 2)

JapanDMC = rbind(JapanDMC1, JapanDMC2, JapanDMC3, JapanDMC4)

summary(JapanDMC)

# 2. 女性役員の平均任期を作成

for(i in 1:30000){
  JapanDMC[i, 2] <- ifelse(is.na(JapanDMC[i, 2]), JapanDMC[i-1, 2], JapanDMC[i, 2])
}

for(i in 30001:82233){
  JapanDMC[i, 2] <- ifelse(is.na(JapanDMC[i, 2]), JapanDMC[i-1, 2], JapanDMC[i, 2])
}

summary(JapanDMC)

DMCdate <- JapanDMC %>%
  select("Company name","DMC\nFull name","DMC\nAppointment date", "DMC\nGender")


#日付情報のある人物のデータだけを残す
DMCdate <- DMCdate %>%
  drop_na("DMC\nAppointment date")  

#重複行の削除

DMCdate2 <- DMCdate

DMCdate3 <- DMCdate2 %>%
  mutate(num = row_number())

summary(DMCdate2)

DMCdate4 <- DMCdate3 %>%
  select("num",  "DMC\nFull name")

for (i in 2:4121){
  DMCdate4[i, 2] <- ifelse(DMCdate4[i, 2] == DMCdate4[i-1, 2], print("TRUE"), DMCdate4[i,2])
}

DMCdate5 <- DMCdate4 %>%
  filter(DMCdate4[2] != 'TRUE')

for (i in 4:3352){
  DMCdate5[i, 2] <- ifelse(DMCdate5[i, 2] == DMCdate5[i-1, 2],print("TRUE"),
                           ifelse(DMCdate5[i, 2] == DMCdate5[i-2, 2],print("TRUE"),
                                  ifelse(DMCdate5[i, 2] == DMCdate5[i-3, 2], print("TRUE"), DMCdate5[i,2])))
}

DMCdate6 <- DMCdate5 %>%
  filter(DMCdate5[2] != 'TRUE')

for (i in 3158){
  DMCdate6[i, 2] <- ifelse(DMCdate6[i, 2] == DMCdate6[i-1, 2], print("TRUE"), DMCdate6[i,2])
}

DMCdate7 <- DMCdate6 %>%
  left_join(DMCdate3, by = c("num" = "num"))


DMCdate7 <- DMCdate7 %>%
  rename("name" = "DMC\nFull name.x",
         "company" = "Company name",
         "date" = "DMC\nAppointment date", 
         "gender" = "DMC\nGender")

DMCdate7 <- DMCdate7 %>%
  select("num", "company", "name", "gender", "date")

write.csv(DMCdate7, "DMCdate7.csv")

#「女性」を取り出す
DMCdateF <- DMCdate7 %>%
  filter(`gender` == "F")

summary(DMCdateF)

#末尾4文字だけを抽出して年次データにする
DMCdateF_1 <- DMCdateF 
DMCdateF_1$`date` <- str_sub(DMCdateF$`date`, start = -4)

DMCdateF_1$`date` <- as.numeric(unlist(DMCdateF_1$`date`))
DMCdateF_1 <- DMCdateF_1[!(DMCdateF_1$`date` == 2022),]

#各女性役員の現在までの就任年数を入れる
DMCdateF_2 <- DMCdateF_1 %>%
  mutate(2021 - `date`)

DMCdateF_2 <- DMCdateF_2 %>%
  rename("years"="2021 - date")


#女性役員の平均任期を産出
DMCdateF_3 <- DMCdateF_2
DMCdateF_3 <- DMCdateF_3 %>% 
  group_by(`company`) %>% 
  summarise("avg_years" =mean(years))

summary(DMCdateF_3)
stargazer(as.data.frame(DMCdateF_3), type="text")

# 3. 女性役員割合の作成

#2022年に就任した役員の排除
for(i in 1:82233){
  JapanDMC[i, 2] <- ifelse(is.na(JapanDMC[i, 2]), JapanDMC[i-1, 2], JapanDMC[i, 2])
}

summary(JapanDMC)

JapanDMCgen <- JapanDMC %>%
  select("Company name", "DMC\nFull name", "DMC\nGender", `DMC\nAppointment date`)

JapanDMCgen$`DMC\nAppointment date` <- str_sub(JapanDMCgen$`DMC\nAppointment date`, start = -4)
JapanDMCgen1 <- JapanDMCgen
JapanDMCgen1$`DMC\nAppointment date`[is.na(JapanDMCgen1$`DMC\nAppointment date`)] <- 0
JapanDMCgen1 <- JapanDMCgen1[!(JapanDMCgen1$`DMC\nAppointment date` == 2022),]

JapanDMCgen1 <- JapanDMCgen1 %>%
  select("Company name", "DMC\nFull name", "DMC\nGender")

JapanDMCgen1 <- distinct(JapanDMCgen1, .keep_all = FALSE) 

summary(JapanDMCgen)


#割合にする
JapanDMCgen1 <- JapanDMCgen1 %>%
  drop_na()

JPgen1 <- JapanDMCgen1 %>%
  drop_na('DMC\nGender')

JPgen2 <- JPgen1 %>%
  group_by(`Company name`) %>%
  summarize('DMC\nGender' = n())

JPgen3 <- JPgen1 %>%
  filter(`DMC\nGender` == 'F')

JPgen4 <- JPgen3 %>%
  group_by(`Company name`) %>%
  summarize('DMC\nGender' = n())

JPgen4 <- JPgen4 %>%
  rename("Female" = 'DMC\nGender')

JPgen2 <- JPgen2 %>%
  rename("All" = 'DMC\nGender')

JPgen <- JPgen4 %>%
  full_join(JPgen2, by = c("Company name" = "Company name"))

JPgen <- JPgen %>%
  mutate_if(is.numeric, ~ replace_na(.x,0))

JPgenname <- JPgen1 %>%
  select("Company name")

JPgen <- JPgen %>%
  full_join(JPgenname, by = c("Company name" = "Company name"))

JPgen <- JPgen %>%
  drop_na("Company name")

JPgen <- JPgen %>%
  mutate(Female/All) #割合作る

JPgen <- distinct(JPgen, .keep_all = FALSE) 

JP2 <- JPgen %>%
  full_join(DMCdateF_3, by = c("Company name" = "company"))

JP3 <- JP1 %>%
  full_join(JP2, by = c("Company.name" = "Company name"))


for (i in 1:5736){
  JP3[i, 52] <- ifelse(JP3[i, 49] == 0, 0, JP3[i, 52])
}

JP4 <- JP3 %>%
  select("Company.name", "Female/All", "All", "avg_years", "Total.Assets.th.USD.2021","Market.capitalisation...Total.assets..Tobin.s.Q..2021", "Number.of.current.advisors", "FT.Industry.Class..M.")


# 4. 役員の平均年齢の作成

Japan0825age <- read_excel("Japan.0825Age.xlsx", sheet = 2)　#82028

for(i in 1:82233){
  Japan0825age[i, 2] <- ifelse(is.na(Japan0825age[i, 2]), Japan0825age[i-1, 2], Japan0825age[i, 2])
}

Japan0825age <- Japan0825age %>%
  drop_na("DMC\nAge")

JPage <- Japan0825age %>%
  select("Company name", "DMC\nAge")

JPage$`DMC\nAge` <- as.numeric(unlist(JPage$`DMC\nAge`))

JPage <- JPage %>%
  na.omit("DMC\nAge")

JPage <- aggregate(JPage$`DMC\nAge`,list(JPage$`Company name`), FUN=mean)

JPage <- JPage %>%
  rename(
    "Company name" = "Group.1",
    "avage" = "x"
  )

#軸データに、女性役員割合と役員平均年齢の情報を結合

JP5 <- JP4 %>%
  full_join(JPage,by = c("Company.name" = "Company name"))

stargazer(JP5, type="text")

JP5 <- JP5 %>%
  rename("TQ" = "Market.capitalisation...Total.assets..Tobin.s.Q..2021",
         "Ind" = "FT.Industry.Class..M.",
         "advi" = "Number.of.current.advisors", 
         "Fem" = "Female/All",
         "name" = "Company.name")

write.csv(JP5, "JP5.csv")

#　5．4産業に分類（python）と交差項作成

JP5ind <- read.csv("JP5ind.csv")

#最後の仕上げ

JP5ind <- JP5ind %>%
  rename("TA"="Total.Assets.th.USD.2021")

JP5ind$`TQ` <- as.numeric(unlist(JP5ind$`TQ`))
JP5ind$`TA` <- as.numeric(unlist(JP5ind$`TA`))

JP5ind <- JP5ind %>%
  drop_na("TQ")

JP5ind <- JP5ind %>%
  mutate(avg_years/All)

JP5ind <- JP5ind %>%
  rename("avg_years2" = "avg_years/All")

#総資産の対数をとる

JP5ind <- JP5ind %>%
  mutate(log(TA))

# 6. 記述統計表の作成
JPname <- JP5ind %>%
  select("Fem", "All", "TQ","advi","avage", "log(TA)", "avg_years", "avg_years2")

JPname <- JPname %>%
  rename("% of female on board"="Fem" ,
         "number of board members"="All",
         "Tobin's Q"="TQ",
         "logarithm of Total Assets"="log(TA)",
         "average age of board members"="avage",
         "average tenure of female on board"="avg_years",
         "average tenure of female on board / number of board members"="avg_years2",
  )

JPname2 <- JPname %>%
  drop_na()

library(stargazer)
stargazer(JPname2, type="text", title="Table2: Descriptive statistics of Japan")

library(ggplot2)
p <- ggplot(data = JP5ind, mapping = aes(x = Fem, y = TQ))
p + geom_point()

JP6 <- JP5ind

JP6 <- JP6 %>%
  filter(`Ind` != '')

JP7 <- JP6 %>%
  select("Unnamed..0", "Ind")

JP7 <- dummy_cols(JP7, remove_first_dummy = F, remove_most_frequent_dummy = F)

JP8 <- JP7 %>%
  full_join(JP6, by = c("Unnamed..0" = "Unnamed..0"))

JP8 <- JP8 %>%
  rename("Ind" = "Ind.x",
         "primary" = "Ind_primary",
         "secondary" = "Ind_secondary",
         "tertiary" = "Ind_tertiary",
         "quaternary" = "Ind_quaternary")

JP8_2 <- JP8

JP8_2 <- JP8_2 %>%
  mutate(Fem*primary, Fem*secondary, Fem*tertiary, Fem*quaternary, avg_years*primary, avg_years*secondary, avg_years*tertiary, avg_years*quaternary)

stargazer(as.data.frame(JP6), type = "text")

JP8_2 <- JP8_2 %>%
  rename("Femprimary"="Fem * primary",
         "Femsecondary"="Fem * secondary",
         "Femtertiary"="Fem * tertiary",
         "Femquaternary"="Fem * quaternary",
         "avg_yearsprimary"="avg_years * primary",
         "avg_yearssecondary"="avg_years * secondary",
         "avg_yearstertiary"="avg_years * tertiary",
         "avg_yearsquaternary"="avg_years * quaternary")

write.csv(JP8_2, "JP8_2.csv")

JP8_2 <- read.csv("JP8_2.csv")

table(JP8$Ind)

# 7. 女性役員存在ダミーの作成

library("fastDummies")

#ダミー作成

FemDum <- ifelse(JP8_3$Fem == 0, 0, 1) 

FemDum <- as.data.frame(FemDum)

summarize(FemDum)

JP8_4 <- bind_cols(JP8_3, FemDum)

# 8. 3産業に分類と交差項作成

terqua <- JP8 %>%
  select("name", "tertiary","quaternary")

for (i in 1:3720){
  terqua[i, 2] <- ifelse(terqua[i, 3] == 1, 1, terqua[i, 2])}

terqua <- terqua %>%
  rename("terqua" = "tertiary")

terqua <- terqua %>%
  select("name", "terqua")

JP8_2 <- JP8_2 %>%
  full_join(terqua, "name"="name")

JP8_2 <- JP8_2 %>%
  mutate(Fem*terqua, avg_years*terqua, avg_years2*terqua)

summary(JP8_2ind)

JP8_3 <- JP8_2 %>%
  mutate(avg_years2*primary, avg_years2*secondary, avg_years2*tertiary, avg_years2*quaternary)


JP8_3 <- JP8_3 %>%
  rename("avg_years2primary"="avg_years2 * primary",
         "avg_years2secondary"="avg_years2 * secondary",
         "avg_years2tertiary"="avg_years2 * tertiary",
         "avg_years2quaternary"="avg_years2 * quaternary",
         "avg_years2terqua"="avg_years2 * terqua",
         "avg_yearsterqua"="avg_years * terqua",
         "Femterqua" = "Fem * terqua")

# 9. 推計

#通常

ols2 <- lm_robust(data = JP8_3,
           formula = TQ ~ Fem + All + advi + avage + log(TA) + Ind)

ols2_2 <- lm_robust(data = JP8_3,
             formula = TQ ~ avg_years2 + All + advi + avage + log(TA) + Ind)

iv2 <- iv_robust(data = JP8_3,
                 formula = TQ ~ Fem + All + advi + avage + log(TA) + Ind | avg_years2 + All + advi + avage + TA + Ind, 
                 se_type = "HC1",
                 diagnostics = TRUE)

first2 <- lm_robust(data = JP8_3,
                    formula = Fem ~ avg_years2 + All + advi + avage + log(TA) + Ind,
                    se_type = "HC1")


regs2 <- list()
regs2[["(1) OLS"]] <- ols2
regs2[["(2) OLS"]] <- ols2_2
regs2[["(3) 2SLS"]] <- iv2
regs2[["(4) first"]] <- first2

library(stargazer)

rows <- tibble::tribble(~term, ~Bivariate, ~Multivariate, ~term, ~term,
                        'Dep.Var.', "Tobin's Q", "Tobin's Q", "Tobin's Q", "% of females on board")
attr(rows, 'position') <- c(0, 5)



modelsummary(regs2, stars = TRUE,
             gof_omit = "AIC|BIC|Log.Lik.|F|se_type",
             title = "OLS Regression Estimates of the Relationship Between Firm Performance and the % of Women Directors on the Board in Japan",
             coef_map = c("Fem"="% of female on board", "avg_years2"="avg female tenure / board members"),
             gof_map = c("nobs", "r.squared"),
             add_rows=rows)

summary(first2)$fstatistic

#交差項（avg_years2）3分類

ols4 <- lm_robust(data = JP8_3,
                  formula = TQ ~ All + advi + avage + log(TA) + Femprimary + Femsecondary + Femterqua)
iv4 <- iv_robust(data = JP8_3,
                 formula = TQ ~ All + advi + avage + log(TA) + Femprimary + Femsecondary + Femterqua | avg_years2:primary + avg_years2secondary + avg_years2terqua + All + advi + avage + log(TA) , 
                 se_type = "HC1",
                 diagnostics = TRUE)
first4_1 <- lm_robust(data = JP8_3,
                      formula = Femprimary ~ avg_years2primary + avg_years2secondary + avg_years2terqua + All + advi + avage + log(TA),
                      se_type = "HC1")

first4_2 <- lm_robust(data = JP8_3,
                      formula = Femsecondary ~ avg_years2primary + avg_years2secondary + avg_years2terqua + All + advi + avage + log(TA),
                      se_type = "HC1")
first4_3 <- lm_robust(data = JP8_3,
                      formula = Femterqua ~ avg_years2primary + avg_years2secondary + avg_years2terqua + All + advi + avage + log(TA) + Ind,
                      se_type = "HC1")

regs4 <- list()
regs4[["(1)OLS"]] <- ols4
regs4[["(2)2SLS"]] <- iv4
regs4[["(3)first"]] <- first4_1
regs4[["(4)first"]] <- first4_2
regs4[["(5)first"]] <- first4_3

rows2 <- tibble::tribble(~term, ~Bivariate, ~Multivariate, ~term, ~term, ~term,
                        'Dep.Var.', "Tobin's Q", "Tobin's Q", "% of female directors*primary", "% of female directors*secondary", "% of female directors*tertiary")
attr(rows2, 'position') <- c(1, 5)

modelsummary(regs4, stars = TRUE, 
             gof_omit = "AIC|BIC|Log.Lik.|F|se_type",gof_map = c("nobs", "r.squared"),
             add_rows=rows2,
             coef_map = c("Femprimary"= "% of female on board * primary",
                          "Femsecondary"= "% of female on board * secondary",
                          "Femterqua"= "% of female on board * tertiary", 
                          "avg_years2primary"= "(female tenure/boardmembers)*primary", 
                          "avg_years2secondary"="(female tenure/boardmembers)*secondary", 
                          "avg_years2terqua"="(female tenure/boardmembers)*tertiary"))
             
summary(first4_1)$fstatistic
summary(first4_2)$fstatistic
summary(first4_3)$fstatistic
summary(first4_4)$fstatistic



#Appendix########################################################

#通常avg_years

JP8_2 <- read.csv("JP8_2.csv")

ols1 <- lm_robust(data = JP8_3,
                  formula = TQ ~ Fem + All + advi + avage + log(TA) + Ind)

ols1_2 <- lm_robust(data = JP8_3,
                    formula = TQ ~ avg_years + All + advi + avage + log(TA) + Ind)

iv1 <- iv_robust(data = JP8_3,
                 formula = TQ ~ Fem + All + advi + avage + log(TA) + Ind | avg_years + All + advi + avage + TA + Ind, 
                 se_type = "HC1",
                 diagnostics = TRUE)

first1 <- lm_robust(data = JP8_3,
                    formula = Fem ~ avg_years + All + advi + avage + log(TA) + Ind,
                    se_type = "HC1")


regs1 <- list()
regs1[["(1) OLS"]] <- ols1
regs1[["(2) OLS"]] <- ols1_2
regs1[["(3) 2SLS"]] <- iv1
regs1[["(4) first"]] <- first1

modelsummary(regs1, stars = TRUE, 
             gof_omit = "AIC|BIC|Log.Lik.|F|se_type",
             title = "Table2: OLS Regression Estimates of the Relationship Between Firm Performance and the Number of Women Directors on the Board in Japan",
             gof_map = c("nobs", "r.squared"),
             add_rows=rows,
             coef_map = c("Fem" = "% of female on board", "avg_years"= "avg female tenure"))


#交差項avg_years
ols3 <- lm_robust(data = JP8_3,
                  formula = TQ ~ All + advi + avage + log(TA) + Femprimary + Femsecondary + Femterqua)

iv3 <- iv_robust(data = JP8_3,
                 formula = TQ ~ All + advi + avage + TA + Femprimary + Femsecondary +  Femterqua | avg_yearsprimary + avg_yearssecondary + avg_yearsterqua + All + advi + avage + log(TA) , 
                 se_type = "HC1",
                 diagnostics = TRUE)

first3_1 <- lm_robust(data = JP8_3,
                      formula = Femprimary ~ avg_yearsprimary+ avg_yearssecondary +  avg_yearsterqua + All + advi + avage + log(TA),
                      se_type = "HC1")

first3_2 <- lm_robust(data = JP8_3,
                      formula = Femsecondary ~ avg_yearsprimary+ avg_yearssecondary + avg_yearsterqua + All + advi + avage + log(TA),
                      se_type = "HC1")

first3_3 <- lm_robust(data = JP8_3,
                      formula = Femterqua ~ avg_yearsprimary + avg_yearssecondary + avg_yearsterqua + All + advi + avage + log(TA) + Ind,
                      se_type = "HC1")


regs3 <- list()
regs3[["(1)OLS"]] <- ols3
regs3[["(2)2SLS"]] <- iv3
regs3[["(3)first"]] <- first3_1
regs3[["(4)first"]] <- first3_2
regs3[["(5)first"]] <- first3_3

modelsummary(regs3, stars = TRUE, 
             gof_omit = "AIC|BIC|Log.Lik.|F|se_type",
             gof_map = c("nobs", "r.squared"),
             add_rows=rows2,
             coef_map = c("Femprimary"= "fem*primary",
                          "Femsecondary"= "fem*secondary",
                          "Femterqua"= "fem*tertiary", 
                          "avg_yearsprimary"= "(avg female tenure)*primary", 
                          "avg_yearssecondary"="(avg female tenure)*secondary", 
                          "avg_yearsterqua"="(avg female tenure)*tertiary"))

summary(first3_1)$fstatistic
summary(first3_2)$fstatistic
summary(first3_3)$fstatistic



#ダミー推計（avg_years）



library(estimatr)

ols5 <- lm_robust(data = JP8_4,
                  formula = TQ ~ FemDum + All + advi + avage + log(TA) + Ind)

ols5_2 <- lm_robust(data = JP8_4,
                    formula = TQ ~ avg_years + All + advi + avage + log(TA) + Ind)

iv5 <- iv_robust(data = JP8_4,
                 formula = TQ ~ FemDum + All + advi + avage + log(TA) + Ind | avg_years + All + advi + avage + log(TA) + Ind, 
                 se_type = "HC1",
                 diagnostics = TRUE)

first5 <- lm_robust(data = JP8_4,
                    formula = FemDum ~ avg_years + All + advi + avage + log(TA) + Ind,
                    se_type = "HC1")
regs5 <- list()
regs5[["(1) OLS"]] <- ols5
regs5[["(2) OLS"]] <- ols5_2
regs5[["(3) 2SLS"]] <- iv5
regs5[["(4) first"]] <- first5

rows3 <- tibble::tribble(~term, ~Bivariate, ~Multivariate, ~term, ~term,
                        'Dep.Var.', "Tobin's Q", "Tobin's Q", "Tobin's Q", "females on board dummy")
attr(rows3, 'position') <- c(0, 5)

modelsummary(regs5, stars = TRUE, 
             gof_omit = "AIC|BIC|Log.Lik.|F|se_type",
             gof_map = c("nobs", "r.squared"),
             add_rows=rows3,
             coef_map = c("FemDum"= "Female on board dummy",
                          "avg_years"="avg female tenure"))

summary(first5)$fstatistic

#ダミー推計（avg_years2）

ols6 <- lm_robust(data = JP8_4,
                  formula = TQ ~ FemDum + All + advi + avage + log(TA) + Ind)
ols6_2 <- lm_robust(data = JP8,
                    formula = TQ ~ avg_years2 + All + advi + avage + log(TA) + Ind)
iv6 <- iv_robust(data = JP8_4,
                 formula = TQ ~ FemDum + All + advi + avage + log(TA) + Ind | avg_years2 + All + advi + avage + TA + Ind, 
                 se_type = "HC1",
                 diagnostics = TRUE)
first6 <- lm_robust(data = JP8_4,
                    formula = FemDum ~ avg_years2 + All + advi + avage + log(TA) + Ind,
                    se_type = "HC1")
regs6 <- list()
regs6[["(1) OLS"]] <- ols6
regs6[["(2) OLS"]] <- ols6_2
regs6[["(3) 2SLS"]] <- iv6
regs6[["(4) first"]] <- first6

modelsummary(regs6, stars = TRUE, 
             gof_omit = "AIC|BIC|Log.Lik.|F|se_type",
             gof_map = c("nobs", "r.squared"),
             add_rows=rows3,
             coef_map = c("FemDum"= "Females on board dummy",
                          "avg_years2"="avg female tenure / board members"))

summary(first6)$fstatistic

summary(JP8_4)


#ダミー交差項

JP8_5 <- JP8_4 %>%
  mutate(FemDum*primary, FemDum*secondary, FemDum*terqua)

stargazer(as.data.frame(JP6), type = "text")

JP8_5 <- JP8_5 %>%
  rename("FemDumprimary"="FemDum * primary",
         "FemDumsecondary"="FemDum * secondary",
         "FemDumterqua"="FemDum * terqua")


ols7 <- lm_robust(data = JP8_5,
                  formula = TQ ~ All + advi + avage + log(TA) + FemDumprimary + FemDumsecondary + FemDumterqua)

iv7 <- iv_robust(data = JP8_5,
                 formula = TQ ~ All + advi + avage + log(TA) + FemDumprimary + FemDumsecondary + FemDumterqua | avg_years2primary + avg_years2secondary + avg_years2terqua + All + advi + avage + log(TA) , 
                 se_type = "HC1",
                 diagnostics = TRUE)

first7_1 <- lm_robust(data = JP8_5,
                      formula = FemDumprimary ~ avg_years2primary + avg_years2secondary + avg_years2terqua + All + advi + avage + log(TA),
                      se_type = "HC1")

first7_2 <- lm_robust(data = JP8_5,
                      formula = FemDumsecondary ~ avg_years2primary + avg_years2secondary + avg_years2terqua + All + advi + avage + log(TA),
                      se_type = "HC1")
first7_3 <- lm_robust(data = JP8_5,
                      formula = FemDumterqua ~ avg_years2primary + avg_years2secondary + avg_years2terqua + All + advi + avage + log(TA) + Ind,
                      se_type = "HC1")

regs7 <- list()
regs7[["(1)OLS"]] <- ols7
regs7[["(2)iv"]] <- iv7
regs7[["(3)first"]] <- first7_1
regs7[["(4)first"]] <- first7_2
regs7[["(5)first"]] <- first7_3

rows4 <- tibble::tribble(~term, ~Bivariate, ~Multivariate, ~term, ~term,~term,
                         'Dep.Var.', "Tobin's Q", "Tobin's Q", "females on board dummy*primary", "females on board dummy*secondary", "females on board dummy*tertiary")
attr(rows4, 'position') <- c(0, 5)

modelsummary(regs7, stars = TRUE, 
             gof_omit = "AIC|BIC|Log.Lik.|F|se_type",
             gof_map = c("nobs", "r.squared"),
             add_rows=rows4,
             coef_map = c("FemDumprimary"= "female on board dummy*primary",
                          "FemDumsecondary"= "female on board dummy*secondary",
                          "FemDumterqua"= "female on board dummy*tertiary", 
                          "avg_years2primary"= "(female tenure/boardmembers)*primary", 
                          "avg_years2secondary"="(female tenure/boardmembers)*secondary", 
                          "avg_years2terqua"="(female tenure/boardmembers)*tertiary"))

summary(first7_1)$fstatistic
summary(first7_2)$fstatistic
summary(first7_3)$fstatistic

#4産業分類（avg_years2）

ols8 <- lm_robust(data = JP8_3,
                  formula = TQ ~ All + advi + avage + log(TA) + Femprimary + Femsecondary + Femtertiary + Femquaternary)
iv8 <- iv_robust(data = JP8_3,
                 formula = TQ ~ All + advi + avage + log(TA) + Femprimary + Femsecondary + Femtertiary + Femquaternary | avg_years2primary + avg_years2secondary + avg_years2tertiary + avg_years2quaternary + All + advi + avage + log(TA) , 
                 se_type = "HC1",
                 diagnostics = TRUE)
first8_1 <- lm_robust(data = JP8_3,
                      formula = Femprimary ~ avg_years2primary + avg_years2secondary + avg_years2tertiary + avg_years2quaternary + All + advi + avage + log(TA),
                      se_type = "HC1")

first8_2 <- lm_robust(data = JP8_3,
                      formula = Femsecondary ~ avg_years2primary + avg_years2secondary + avg_years2tertiary + avg_years2quaternary + All + advi + avage + log(TA),
                      se_type = "HC1")
first8_3 <- lm_robust(data = JP8_3,
                      formula = Femtertiary ~ avg_years2primary + avg_years2secondary + avg_years2tertiary + avg_years2quaternary + All + advi + avage + log(TA) + Ind,
                      se_type = "HC1")

first8_4 <- lm_robust(data = JP8_3,
                      formula = Femquaternary ~ avg_years2primary + avg_years2secondary + avg_years2tertiary + avg_years2quaternary + All + advi + avage + log(TA) + Ind,
                      se_type = "HC1")

regs8 <- list()
regs8[["(1)OLS"]] <- ols8
regs8[["(2)2SLS"]] <- iv8
regs8[["(3)first"]] <- first8_1
regs8[["(4)first"]] <- first8_2
regs8[["(5)first"]] <- first8_3
regs8[["(6)first"]] <- first8_4

rows5 <- tibble::tribble(~term, ~Bivariate, ~Multivariate, ~term, ~term,~term,~term,
                         'Dep.Var.', "Tobin's Q", "Tobin's Q", "% of females on board*primary", "% of females on board*secondary", "% of females on board*tertiary", "% of females on board*quaternary")
attr(rows5, 'position') <- c(0, 5)

modelsummary(regs8, stars = TRUE, 
             gof_omit = "AIC|BIC|Log.Lik.|F|se_type",
             gof_map = c("nobs", "r.squared"),
             add_rows=rows5,
             coef_map = c("Femprimary"= "% of females on board * primary",
                          "Femsecondary"= "% of females on board * secondary",
                          "Femtertiary"= "% of females on board * tertiary", 
                          "Femquaternary"= "% of females on board * quaternary",
                          "avg_years2primary"= "(females tenure/boardmembers)*primary", 
                          "avg_years2secondary"="(females tenure/boardmembers)*secondary", 
                          "avg_years2tertiary"="(females tenure/boardmembers)*tertiary",
                          "avg_years2quaternary"="(females tenure/boardmembers)*quaternary"))

summary(first8_1)$fstatistic
summary(first8_2)$fstatistic
summary(first8_3)$fstatistic
summary(first8_4)$fstatistic


#R econ library

library(tidyverse)
library(haven)

# Load an example dataset ---------------
AS7_2 <- read.csv("AS7_2.csv")
JP8 <- read.csv("JP8.csv")

econJ <- JP8 %>%
  select("Fem")

econJ <- econJ %>%
  mutate("Japan")

econJ <- econJ %>%
  rename("category"='"Japan"')

econA <- AS7_2 %>%
  select("Fem")

econA <- econA %>%
  mutate("ASEAN")

econA <- econA %>%
  rename("category"='"ASEAN"')

econ = rbind(econA, econJ)

econgraph <- econ %>%
  mutate(category = as.factor(.$category)) %>%
  group_by(category) %>%
  mutate(mean_Fem = mean(Fem)) %>%
  ungroup()

ggplot(econgraph, aes(x = Fem, colour = category)) +
  geom_density(alpha = 0.1) +
  geom_vline(
    aes(xintercept = mean_Fem, colour = category), 
    linetype = "dashed", size = 0.3
  ) +
  theme_classic() +
  xlab("% of female on board") +
  ylab("Density") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  )



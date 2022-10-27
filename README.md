# fem_on_board
「女性役員割合と業績の関係」の日本コードです。

# Data
OSIRISの企業データを使用。
元のサンプル数はは日本の企業5735社であったが、欠損地の関係で1780社となった。
 
# Variables
変数は以下のとおりである。
### Dependent Variable
Tobin's Q：利益/総資産で表される
### Independent Variables
% of females on board：2021年に存在した女性役員割合を計算
females on board dummy：2021年に女性役員が存在するかのダミー変数
### Instrumental Variables
average tenure of females on the board / number of directors：女性役員の平均任期を役員数で割ったもの
average tenure of females on the board：女性役員の平均任期を計算
### Control Variables
Total assets：総資産の対数
Number of current advisors：経営コンサルティングなどの取引数
Average age of board members：役員の平均年齢
Number of board members：2021年に存在した役員数
Industry dummies：FT industry classをもとに、業界を1次、2次、3次産業に分類したダミー

# Empirical Methodology

OLSと2SLSを採択。
 
# Flow

データ加工と分析のフローは以下のとおりである
1．データの読み込み
2．女性役員の平均任期を作成
3．女性役員割合の作成
4．役員の平均年齢の作成
5．4産業に分類（python）と交差項作成
6．記述統計表の作成
7. 女性役員存在ダミーの作成
8. 3産業に分類と交差項作成
9. 推計と結果の表示

# Requirements

library(tidyverse)
library(readxl)
library(dplyr)
library(estimatr)
library(stargazer)
library(modelsummary)

# Results
 
# Note
推計結果は数が多いため、一部省略している。
 

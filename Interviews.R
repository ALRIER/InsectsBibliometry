pkg <- function(pkg){
   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
   if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
   sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse","raster","sf","ggspatial","cluster","factoextra",
              "NbClust","tidyr","forecast","semTools","corrplot",
              "corrr","haven","psych","dplyr","lavaan","readr","cvms","tm",
              "NLP","SnowballC","RColorBrewer","wordcloud","wordcloud2",
              "RefManageR","bibliometrix","GGally","quanteda","ggplot2",
              "ggpubr","Factoshiny","syuzhet","RColorBrewer","tokenizers",
              "stringr","sentimentr","stringi","stopwords","twitteR",
              "mscstexta4r","plyr","psych","corrr","latticeExtra",
              "semPlot","lavaan","readr","lme4","sjPlot","gvlma","Rcmdr",
              "tidymodels","caret","lmtest","gapminder","png","rtweet","knitr")
pkg(packages)


setwd("/home/alrier/Documentos/Insectos/InsectsBibliometry")
library(readxl)
nocons <- read_excel("/home/alrier/Documentos/Insectos/InsectsBibliometry/NO_CONSUMIDORES.xlsx")
con=read_excel("/home/alrier/Documentos/Insectos/InsectsBibliometry/CONSUMIDORES.xlsx")
#PREGUNTA 1 CONSUMIDORES ---------
P1cons= con$`PREGUNTA 1-PATRON ALIMENTACION`
#Token
tokensP1cons<-tokens(P1cons,what = "word",
                           remove_punct = TRUE,
                           remove_symbols =TRUE,
                           remove_numbers =TRUE,
                           remove_separators =TRUE,
                           split_hyphens =TRUE)
P1consvecotr <- tolower(tokensP1cons)
sent_P1cons = get_nrc_sentiment(P1consvecotr, lang="spanish")
#graphic emotions
barplot(
   colSums(prop.table(sent_P1cons[, 1:8])),
   space = 0.2,
   horiz = F,
   las = 1,
   cex.names = 0.7,
   col = brewer.pal(n = 8, name = "Set3"),
   main = "8 different emotions expresed in the interview",
   xlab="Emotions", ylab = "Frequency")

#graphic sentiments
barplot(
   colSums(prop.table(sent_P1cons[, 9:10])),
   space = 0.2,
   horiz = T,
   las = 1,
   cex.names = 0.7,
   col = brewer.pal(n = 2, name = "Set3"),
   main = "Sentiments that express the interview 1",
   sub = "There are two different sentiments: Positive and Negative",
   xlab="Frequency", ylab = "Sentiments")
# PREGUNTA 1 NO CONSUMIDORES -------

P1nocons= nocons$`PREGUNTA 1-PATRON ALIMENTACION`
#Token
tokensP1nocons<-tokens(P1nocons,what = "word",
                     remove_punct = TRUE,
                     remove_symbols =TRUE,
                     remove_numbers =TRUE,
                     remove_separators =TRUE,
                     split_hyphens =TRUE)
P1noconsvecotr <- tolower(tokensP1nocons)
sent_P1nocons = get_nrc_sentiment(P1noconsvecotr, lang="spanish")
#graphic emotions
barplot(
   colSums(prop.table(sent_P1nocons[, 1:8])),
   space = 0.2,
   horiz = F,
   las = 1,
   cex.names = 0.7,
   col = brewer.pal(n = 8, name = "Set3"),
   main = "8 different emotions expresed in the interview",
   xlab="Emotions", ylab = "Frequency")

#graphic sentiments
barplot(
   colSums(prop.table(sent_P1nocons[, 9:10])),
   space = 0.2,
   horiz = T,
   las = 1,
   cex.names = 0.7,
   col = brewer.pal(n = 2, name = "Set3"),
   main = "Sentiments that express the interview 1",
   sub = "There are two different sentiments: Positive and Negative",
   xlab="Frequency", ylab = "Sentiments")
## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(rainbow)

## ----example------------------------------------------------------------------


data(sbdata)

res = spring_summary(sbdata, spring_out=TRUE )
head(res)

allyear = res[[2]]
ggplot(allyear, aes(as.factor(year), tavg_mean, fill=precip_sum))+geom_col()



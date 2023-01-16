library(ggplot2)
library(tidyverse)
library(dplyr)
fev1
ggplot(FEV1~age, data=fev1)

set.seed(10)

fev1 <- fev1 %>% slice_sample(n=20)

ggplot(fev1, aes(x = age, y = FEV1)) + 
  geom_smooth(method=loess) + labs(x = 'Age', y = 'FEV1', title = 'Age vs FEV1', caption = 'Image by Catherine')

cor(fev1$age, fev1$FEV1)

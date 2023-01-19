library(ggplot2)
library(tidyverse)
library(dplyr)

fev1

set.seed(10)

fev1_sampled <- fev1 %>%
  count(id) %>%
  filter(n > 6) %>%
  slice_sample(n = 20) %>%
  select(id) %>%
  inner_join(fev1)

cor(fev1$age, fev1$FEV1)

fev1_plot <- ggplot(data = fev1, 
                    aes(x = age, y = FEV1)) +
  geom_point(alpha=0.5, color='blue') +
  labs(x = 'Age', y = 'FEV1', title = 'Age vs FEV1', caption = 'Image by Catherine') +
  theme_bw()

fev1_plot_2 <- ggplot(data = fev1, 
                    aes(x = age, y = FEV1)) +
  geom_smooth(lty=3,se=F) +
  labs(x = 'Age (years)', y = 'FEV1 (L)', title = 'Spirometry of 300 girls in Topeka, KS', caption = 'Image by Catherine')

fev1_plot_3 <- ggplot(data = fev1, 
                      aes(x = age, y = FEV1, group = id)) +
  geom_line() +
  labs(x = 'Age (years)', y = 'FEV1 (L)', title = 'Spirometry of 300 girls in Topeka, KS', caption = 'Image by Catherine')

library(mgcv)

model <- gamm(data = fev1, FEV1 ~ s(age), random = list(id = ~1))

pred <- data.frame(age = seq(min(fev1$age),
                             max(fev1$age),
                             length.out = 101)) %>%
  mutate(FEV1 = predict(model$gam, .))

ggplot(data = fev1, aes(x = age, y = FEV1)) +
  geom_line(alpha = 0.05, aes(group = id, color = "Data")) +
  geom_smooth(aes(color = "Loess"), se=FALSE, method = "loess") +
  theme_bw() +
  geom_line(data = pred, aes(color = "GAMM")) +
  xlab("Age (years)") +
  ylab("FEV1 (L)") +
  ggtitle("Average spirometry of 300 girls in Topeka, KS") +
  scale_color_manual(values = c("Data" = "black",
                                "Loess" = "red",
                                "GAMM" = "blue"),
                     name = "Source")

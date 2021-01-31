library(Lahman)
library(tidyverse)

data("Teams")
glimpse(Teams)

#Filter, Select and create run differential, and Win %.
Teams_2000 <- Teams %>%
  filter(yearID >= 2000) %>%
  select(teamID, yearID, lgID, G, W, L, R, RA) %>%
  mutate(RD = (R- RA), Wpg = (W/(W+L)))

glimpse(Teams_2000)

theme_chart <- function(){
  theme(legend.position = "bottom", legend.direction = "vertical", legend.box = "vertical",
        legend.key.size = unit(5, "mm"), plot.background = element_rect(fill = "gray95"),
        plot.margin = unit(c(5, 10, 5, 10), units = "mm"))
}

p <- ggplot(Teams_2000, aes(x = RD, y = Wpg)) +
  geom_point() +
  ggtitle("Run Differential vs Winning %") +
  theme_chart()

p

#Strong linear relationship between RD and W%
model <- lm(Wpg ~ RD, Teams_2000)
summary(model)

##Add Linear Regression
p +
  geom_smooth(method = 'lm') +
  geom_jitter() +
  stat_summary(fun.y = min, colour = "blue", geom = "point", size = 1) +
  stat_summary(fun.y = max, colour = "orange", geom = "point", size = 1)


which.min(Teams_2000$Wpg)
Teams_2000[55,]
#SEA 2001

which.max(Teams_2000$Wpg)
Teams_2000[101,]
#DET 2003


#Which Teams are more consistent
Teams_2000 %>%
  group_by(teamID) %>%
  summarise(avg = mean(Wpg)) %>%
  arrange(desc(by = avg))


p <- ggplot(Teams_2000, aes(x = RD, y = Wpg, color = factor(lgID)))+
  geom_point() +
  ggtitle("Run Differential vs Winning %") +
  theme_chart()

p + geom_smooth(method = 'lm') +
  geom_jitter()


#Pythagorean formula of Wpg
Teams_2000 <- Teams_2000 %>%
  mutate(PytWpg = (R^2)/(R^2+RA^2), PytResidual = Wpg - PytWpg)

sqrt(mean(Teams_2000$PytResidual ^ 2))

#Actual vs Proyected Wpg. Red Sox
RedSox <- Teams_2000 %>%
  filter(teamID == "BOS") %>%
  select(yearID, W, L, Wpg, PytWpg) %>%
  gather(-yearID, -W, -L, key = "var", value = "Value")


w <- ggplot(RedSox, aes(x = yearID, y = Value)) +
  geom_line() +
  xlab('Year') +
  ylab('Wpg and Projected Wpg') +
  theme_chart() +
  facet_wrap(~ var)

w + aes(color = var)


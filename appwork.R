

load("~/dynamic_bulldoze2/outputs/model31")

library(tidyverse)
library(ctsem)
library(plotly)
library(rstan)
library(kableExtra)
library(shinystan)


kalman.data = ctKalman(model, 
                       timestep = .25,
                       subjects = 1:length(model$setup$idmap$new)
                       )

g = kalman.data %>%
  filter(Element %in% c("etasmooth")) %>%
  ggplot(aes(x = Time, y = value)) +
  geom_line(aes(color = Subject)) +
  facet_wrap(~Row, ncol = 1) +
  theme_bw()


g = g +
  geom_ribbon(aes(ymin = value - sd, ymax = value + sd,
                  color = Subject), linetype = "dotted",
              alpha = .2) 


ggplotly(g)

getAnywhere(plot.ctKalmanDF)

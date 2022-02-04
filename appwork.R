

load("~/dynamic_bulldoze2/outputs/model31")

library(tidyverse)
library(ctsem)
library(plotly)
library(rstan)
library(kableExtra)
library(shinystan)


###################################
#################
##############
### Latent Plot

kalman.data = ctKalman(model, 
                       timestep = .25,
                       subjects = 1:length(model$setup$idmap$new)
                       )

g = kalman.data %>%
  filter(Element %in% c("etasmooth")) %>%
  ggplot(aes(x = Time, y = value)) +
  geom_line(aes(color = Subject)) +
  facet_wrap(~Row, ncol = 1) +
  theme_bw() +
  ggtitle("Latent Values") +
  ylab("") +
  xlab("Year")


g = g +
  geom_ribbon(aes(ymin = value - sd, ymax = value + sd,
                  color = Subject), linetype = "dotted",
              alpha = .2) 


ggplotly(g)



###########################################
###########################################
###########################################
# Manifest Plot

d1 = kalman.data %>%
  filter(Element %in% c("y"))

d2 = kalman.data %>%
  filter(Element %in% c("ysmooth"))

g = kalman.data %>%
  filter(Element %in% c("y", "ysmooth")) %>%
  ggplot(aes(x = Time, y = value)) +
  geom_point(aes(color = Subject, group = Element), data = d1) +
  geom_line(aes(color = Subject, group = Element), data = d2) +
  facet_wrap(~Row, ncol = 1) +
  theme_bw() +
  theme(strip.background =element_rect(fill="black")) +
  theme(strip.text = element_text(colour = 'white')) +
  theme(legend.position = "none") +
  ggtitle("Manifest Values") +
  ylab("") +
  xlab("Year")

ggplotly(g)


g = g +
  geom_ribbon(aes(ymin = value - sd, ymax = value + sd,
                  color = Subject), linetype = "dotted",
              alpha = .2) 

#####################################
#####################################
#####################################
# Summary  Output

check = summary(model)$parmatrice



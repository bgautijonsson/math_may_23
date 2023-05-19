library(tidyverse)
library(bggjphd)
library(metill)
library(gganimate)
library(sf)
library(ggtext)
library(glue)
library(scales)
library(evd)
theme_set(theme_bggj())

crossing(
    mu = 25,
    sigma = seq(1, 15, length.out = 200),
    xi = seq(-0.3, 0.3, length.out = 200),
    x = 30
) |> 
    rowwise() |> 
    mutate(
        p = 1 - pgev(x, loc = mu, scale = sigma, shape = xi)
    ) |> 
    ungroup() |> 
    filter(
        p <= 0.9999
    ) |> 
    ggplot(aes(sigma, xi)) +
    geom_contour_filled(aes(z = p)) +
    scale_x_log10() +
    facet_wrap("mu", scales = "free")


crossing(
    mu = 2,
    sigma = c(1, 2, 4),
    xi = 0.5,
    x = seq(0, 24, length.out = 2000)
) |> 
    rowwise() |> 
    mutate(
        p = dgev(x, loc = mu, scale = sigma, shape = xi)
    ) |> 
    ungroup() |> 
    filter(
        p <= 0.9999
    ) |> 
    ggplot(aes(x, p)) +
    geom_line(aes(col = factor(sigma))) 

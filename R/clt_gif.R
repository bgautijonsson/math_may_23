library(tidyverse)
library(bggjphd)
library(metill)
library(gganimate)
library(sf)
library(ggtext)
library(glue)
library(scales)
library(ggforce)
theme_set(theme_bggj())

bin_width <- 0.02

crossing(
    iter = 1:100,
    size = c(10, 100, 1000)
) |> 
    mutate(
        x = map(size, ~ rexp(n = .x))
    ) |> 
    unnest(x) |> 
    summarise(
        mean = mean(x) |> plyr::round_any(bin_width),
        .by = c(iter, size)
    ) |> 
    arrange(iter, mean) |> 
    mutate(
        y = seq_along(mean),
        .by = mean
    )  |> 
    ggplot() +
    geom_ellipse(
        aes(x0 = mean, y0 = y, group = iter, a = bin_width/2, b = 0.5, angle = 0),
        fill = "black"
    ) +
    facet_wrap("size", scales = "free") +
    transition_states(states = iter, transition_length = 100, state_length = 1) +
    coord_equal(bin_width) +
    shadow_mark()



map_dfr(
    1:100,
    function(i) {
        tibble(
            iter = i,
            x = rexp(n = 1000)
        )
    }
) |> 
    summarise(
        mean = mean(x) |> plyr::round_any(bin_width),
        .by = iter
    ) |> 
    arrange(iter, mean) |> 
    mutate(
        y = seq_along(mean),
        .by = mean
    )  |> 
    ggplot() +
    geom_ellipse(
        aes(x0 = mean, y0 = y, group = iter, a = bin_width/2, b = 0.5, angle = 0),
        fill = "black"
        ) +
    transition_states(states = iter, transition_length = 100, state_length = 1) +
    coord_equal(bin_width) +
    shadow_mark()


n_frame <- 40

crossing(
    samp = seq_len(n_frame),
    size = exp(seq(log(1e1), log(1e6), length.out = 5))
) |> 
    mutate(
        mean = map_dbl(
            size, ~ mean(rexp(n = size))
        )
    ) |> 
    ggplot(aes(mean)) +
    geom_density(aes(group = size, fill = size)) +
    scale_fill_distiller(
        trans = "log10"
    ) +
    transition_states(size) +
    ease_aes("cubic-in-out") +
    shadow_wake(0.1)


p_anim <- animate(
    plot = p,
    nframes = n_frame * 8,
    fps = 10,
    width = 8, height = 0.621 * 8, unit = "in",
    res = 200
)


anim_save(
    animation = p_anim,
    filename = "images/clt.gif"
)

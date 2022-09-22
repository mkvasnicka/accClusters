library(tidyverse)
library(sf)
library(tmap)
library(manipulateWidget)

tmap_mode("view")

oo <- read_rds("guts_data/output/clusters/clusters_CZ0642_default_2019-01-01_2021-12-31.rds")

f <- function(q, al, var) {
    zz <- oo |>
        filter(quantile == q, additional_clusters == al) |>
        pull(clusters) |>
        pluck(1) |>
        mutate(stat = case_when(
            var == "cost" ~ cost,
            var == "density" ~ total_density,
            var == "mean density" ~ total_density / total_length))
    p <- tm_shape(zz |> filter(!is.na(cost))) +
        tm_lines("stat", lwd = 3,
                 palette = rev(hcl.colors(7, "Greens2")),
                 midpoint = 0.00002)
    if (nrow(zz |> filter(is.na(cost))) > 0)
        p <- p + tm_shape(zz |> filter(is.na(cost))) +
            tm_lines("stat", lwd = 3,
                     palette = rev(hcl.colors(7, "OrRd")),
                     midpoint = 0.00002)
    print(p)
}

manipulateWidget(f(quantile, additional_lixels, var = v),
           quantile = mwSlider(0.95, 0.999, step = 0.001, value = 0.95),
           additional_lixels = mwSlider(1, 10, value = 1),
           v = mwSelect(c("cost", "density", "mean density"), value = "cost"))

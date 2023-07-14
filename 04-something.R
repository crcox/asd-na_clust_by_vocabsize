library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

decision_boundary <- function(m, crit_p = .5) {
    L <- log(crit_p / (1 - crit_p))
    g <- contrasts(model.frame(m)$group)[, 1]
    b <- coef(m)
    return(-(b["(Intercept)"] + (b["groupASD"] * g) - L) / (b["nproduced"] + (b["nproduced:groupASD"] * g)))
}

models <- map(1:680, ~{
    p <- file.path("models", "glm-item_level", sprintf("%03d.rds", .))
    readRDS(p)
})

vid_lookup <- readRDS("samples/vid_lookup.rds")
vid_lookup$mod <- models[vid_lookup$num_item_id]
vid_lookup <- bind_cols(
    vid_lookup,
    map(vid_lookup$mod, ~{
        x <- decision_boundary(.)
        tibble(
            not_autistic = as.integer(round(x["NA"])),
            autistic = as.integer(round(x["ASD"]))
        )
    }) %>% list_rbind()
)

clust_df <- readRDS("result/vocab_scoped_local_clust.rds")

df <- vid_lookup %>%
    left_join(
        select(clust_df, not_autistic = vocab_size, num_item_id, clust_na = clust, clust_rand_na = clust_rand)
    ) %>%
    left_join(
        select(clust_df, autistic = vocab_size, num_item_id, clust_asd = clust, clust_rand_asd = clust_rand)
    ) %>%
    na.omit()

plot(df$not_autistic, df$clust_na - df$clust_rand_na, col = 'red')
points(df$autistic, df$clust_asd - df$clust_rand_asd, col = 'blue')

df_plot <- tibble(
    group = gl(2, nrow(df), labels = c("non-autistic", "autistic")),
    vocab_size = c(df$not_autistic, df$autistic),
    clust = c(df$clust_na, df$clust_asd),
    clust_norm = c(df$clust_na - df$clust_rand_na, df$clust_asd - df$clust_rand_asd)
)

ggplot(df_plot, aes(x = vocab_size, y = clust, color = group)) +
    geom_point() +
    geom_smooth()


# maybe? ----
models <- readRDS("models/base_models.rds")

d <- expand_grid(
    group = factor(1:2, labels = levels(models$aspl$model$group)),
    vocab_size = 35:567
)
orth_poly <- as.matrix(select(models$aspl$model, linear, quadradic, cubic))
attributes(orth_poly) <- attributes(models$aspl$model$linear)
class(orth_poly) <- c("poly", "matrix")
orth_poly <- predict(orth_poly, d$vocab_size)
d$linear <- orth_poly[, 1]
d$quadradic <- orth_poly[, 2]
d$cubic <- orth_poly[, 3]

x <- map(models, function(m, df) {
   predict(m, df)
}, df = d)
x

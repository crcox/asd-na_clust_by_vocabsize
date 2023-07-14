library(dplyr)
library(purrr)
library(tidyr)

vid_lookup <- readRDS("samples/vid_lookup.rds")

samples_vid <- readRDS("samples/samples_vid-ppt.rds")
clust <- readRDS("samples/clust-ppt.rds")
clust_full <- map2(samples_vid, clust, function(ix_lst, x_lst) {
    map2(ix_lst, x_lst, function(ix, x) {
        x[is.nan(x)] <- 0
        y <- numeric(675)
        y[ix[seq_along(x)]] <- c(x, rep(0, length(ix) - length(x)))
        return(y)
    })
})
clust_avg <- map(clust_full, ~{
    rowMeans(matrix(list_c(.), nrow = 675))
})

d <- expand_grid(vocab_size = 35:567, vid = 1:675) %>%
    left_join(vid_lookup)
d$clust <- list_c(clust_avg)

samples_vid <- readRDS("samples/samples_vid-rand.rds")
clust <- readRDS("samples/clust-rand.rds")
clust_full <- map2(samples_vid, clust, function(ix_lst, x_lst) {
    map2(ix_lst, x_lst, function(ix, x) {
        x[is.nan(x)] <- 0
        y <- numeric(675)
        y[ix[seq_along(x)]] <- c(x, rep(0, length(ix) - length(x)))
        return(y)
    })
})
clust_avg <- map(clust_full, ~{
    rowMeans(matrix(list_c(.), nrow = 675))
})
d$clust_rand <- list_c(clust_avg)

saveRDS(d, "result/vocab_scoped_local_clust.rds")

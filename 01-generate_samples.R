library(dplyr)
library(purrr)

d <- readRDS("data/asd_na-osg-2023_06_30.rds")
meta <- readRDS("data/cdi-metadata.rds")

tmp <- map(35:567, function(i, window_size) {
    q <- (window_size - 1) / 2
    a <- i - q
    b <- i + q
    d %>%
        filter(nproduced >= a, nproduced <= b, produced == TRUE) %>%
        count(num_item_id) %>%
        mutate(p = n / sum(n))

}, window_size = 31)

samples <- map2(tmp, 35:567, function(df, n) {
    map(1:1000, function(i, df) {
        sample(df$num_item_id, size = n, replace = FALSE, prob = df$p)
    }, df = df)
})

samples_rand <- map(35:567, function(n) {
    map(1:1000, function(i) {
        sample(675, size = n, replace = FALSE)
    })
})

saveRDS(samples, "samples/samples-ppt.rds")
saveRDS(samples_rand, "samples/samples-rand.rds")

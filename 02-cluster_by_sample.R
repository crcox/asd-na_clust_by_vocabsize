library(dplyr)
library(purrr)
library(igraph)
library(progressr)

m <- readRDS("data/cdi-metadata.rds")
g <- upgrade_graph(readRDS("data/child_net_graph.rds"))

vid_lookup <- tibble(
    vid = 1:vcount(g),
    word = names(V(g))
) %>% left_join(select(m, word, num_item_id))
saveRDS(vid_lookup, file = "samples/vid_lookup.rds")

samples_vid <- readRDS("samples/samples-ppt.rds") %>%
    map_depth(2, function(ix, x) {
        x$vid[x$num_item_id %in% ix]
    }, x = vid_lookup)
saveRDS(samples_vid, file = "samples/samples_vid-ppt.rds")

clust_list <- with_progress({
    p <- progressor(length(samples_vid) * length(samples_vid[[1]]))
    map_depth(samples_vid, .depth = 2, ~{
        p()
        transitivity(induced_subgraph(g, .), type = "local")
    })
})
saveRDS(clust_list, file = "samples/clust-ppt")


samples_vid <- readRDS("samples/samples-rand.rds") %>%
    map_depth(2, function(ix, x) {
        x$vid[x$num_item_id %in% ix]
    }, x = vid_lookup)
saveRDS(samples_vid, file = "samples/samples_vid-rand.rds")

clust_list <- with_progress({
    p <- progressor(length(samples_vid) * length(samples_vid[[1]]))
    map_depth(samples_vid, .depth = 2, ~{
        p()
        transitivity(induced_subgraph(g, .), type = "local")
    })
})
saveRDS(clust_list, file = "samples/clust-rand")

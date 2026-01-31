remotes::install_github("YukunJiao/threadleR")
library(threadleR)

# basics
th_start_threadle("~/Documents/Threadle/Threadle.CLIconsole/bin/Debug/net8.0/threadle")
ex_dir <- th_stage_examples_to_wd("./inst/examples")
th_set_workdir(ex_dir)

mynet_nodeset <- th_load_file("mynet_nodeset", "mynet_nodesetfile.tsv", type = "nodeset")
mynet <- th_load_file("mynet", "mynet.tsv", type = "network")

lazeganet_nodeset <- th_load_file("lazega_nodeset", "lazega_nodes.tsv", type = "nodeset")
lazeganet <- th_load_file("lazega","lazega.tsv", type = "network")

th_info(mynet)
th_info(mynet_nodeset)

th_density(mynet, "trade")
th_density(mynet, "work")

# attribute walk
th_attr_walk <- function(network,
                         start = NULL,
                         steps = 30,
                         layernames = NULL,
                         direction = "both",
                         attrname = "gender",
                         keep = c("f"),
                         unique = TRUE,
                         fallback = c("any", "stop")
) {
  fallback <- match.arg(fallback)

  if (is.null(start)) {
    cur <- th_get_random_node(network)
  } else {
    cur <- start
  }

  path <- integer(steps + 1L)
  path[1L] <- cur

  for (t in seq_len(steps)) {
    alters <- th_get_node_alters(
      network = network,
      nodeid = cur,
      layernames = layernames,
      direction = direction,
      unique = unique
    )

    if (length(alters) == 0L) {
      if (fallback == "stop") break
      path[(t + 1L):(steps + 1L)] <- NA_integer_
      break
    }

    avals <- vapply(
      alters,
      function(nid) {
        v <- th_get_attr(network, nodeid = nid, attrname = attrname)
        as.character(v)
      },
      FUN.VALUE = character(1)
    )

    ok <- alters[avals %in% keep]

    if (length(ok) == 0L) {
      if (fallback == "stop") break
      nxt <- sample(alters, 1L)
    } else {
      nxt <- sample(ok, 1L)
    }

    cur <- nxt
    path[t + 1L] <- cur
  }

  path_attr <- vapply(
    path,
    function(nid) if (is.na(nid)) NA_character_ else as.character(th_get_attr(network, nid, attrname)),
    FUN.VALUE = character(1)
  )

  data.frame(step = seq_along(path) - 1L, nodeid = path, attr = path_attr, stringsAsFactors = FALSE)
}

walk1 <- th_attr_walk(
  network   = lazeganet,
  steps     = 20,
  layernames = c("friends"),
  direction = "both",
  attrname  = "Gender",
  keep      = c("M"),
  fallback  = "any"
)

walk1

# ego network
ego <- 42
alters <- th_get_node_alters(lazeganet, ego, layernames = "friends", unique = TRUE)
ego_nodes <- unique(c(ego, alters))

# 1. create an empty nodeset
ego_nodeset <- th_create_nodeset("ego_nodeset")

# 2. add ego + alters
for (id in ego_nodes) {
  th_add_node(ego_nodeset, id)
}

# 3. subnet
ego_net <- th_subnet("ego_net", lazeganet, ego_nodeset)

th_info(ego_net)

th_preview(ego_net)

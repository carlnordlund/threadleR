# Install and load
remotes::install_github("YukunJiao/threadleR")
library(threadleR)


# Start Threadle
# If 'threadle' is on your PATH, you can call th_start_threadle() with no args.
# Otherwise, provide the full path to the Threadle executable.
th_start_threadle()

# Locate example data shipped with the package
ext <- system.file("extdata", package = "threadleR")

# Set Threadle's working directory
th_set_workdir(ext)
# Alternatively, to keep both in sync:
# th_set_workdir(getwd()) or th_sync_wd()

#----

# Load lazega.tsv from Threadle's workdir
# This creates both a network and its associated nodeset in the Threadle
lazeganet <- th_load_file("lazeganet", "lazega.tsv", type = "network")

# Inspect structure metadata (e.g., number of nodes/edges, layers, attributes)
th_info(lazeganet)

# Preview a small sample of nodes and ties
th_preview(lazeganet)


# It is recommended to assign the returned handle when loading objects,
# and to keep the R variable name consistent with the Threadle backend object name.

# Load a nodeset
mynet_nodeset <- th_load_file(
  name = "mynet_nodeset",
  file = "mynet_nodesetfile.tsv",
  type = "nodeset"
)

# Load a network
mynet <- th_load_file(
  name = "mynet",
  file = "mynet.tsv",
  type = "network"
)


# Loading multiple nodesets
# If you need multiple nodesets at the same time,
# use distinct backend names to avoid overwriting existing objects:
mynet_ns1 <- th_load_file("mynet_ns1", "mynet_nodesetfile.tsv", type = "nodeset")
mynet_ns2 <- th_load_file("mynet_ns2", "mynet_nodesetfile.tsv", type = "nodeset")


# Object handles
# You can assign the returned handle to any R variable name:
ns <- th_load_file("mynet_ns", "mynet_nodesetfile.tsv", type = "nodeset")

# This does not duplicate data (handles only store the backend name),
# but for clarity we recommend matching the R variable name to the backend name:
mynet_ns <- th_load_file("mynet_ns", "mynet_nodesetfile.tsv", type = "nodeset")

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

th_stop_threadle()


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
                         layernames = NULL,        # NULL = 所有层；或 c("trade","kinship") 等
                         direction = "both",        # "both" / "in" / "out"
                         attrname = "gender",
                         keep = c("f"),             # 允许的属性值集合（字符）
                         unique = TRUE,
                         fallback = c("any", "stop") # 没有符合条件邻居时的策略
) {
  fallback <- match.arg(fallback)

  # 起点
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

    # 没邻居：直接结束（或 NA）
    if (length(alters) == 0L) {
      if (fallback == "stop") break
      path[(t + 1L):(steps + 1L)] <- NA_integer_
      break
    }

    # 取属性（Threadle 后端可能返回 numeric 或 character；这里统一转成 character 做筛选）
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

  # 附带把每一步的属性也取出来，方便展示
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
  layernames = c("friends"),   # 只用 trade 层；也可以 NULL 表示所有层
  direction = "both",        # trade 是有向层时可以改成 "out"/"in"
  attrname  = "Gender",
  keep      = c("f"),
  fallback  = "any"
)

walk1

# ego network
ego <- 42
alters <- th_get_node_alters(lazeganet, ego, layernames = "friends", unique = TRUE)
ego_nodes <- c(ego, alters)
ego_nodeset <- th_filter(
  name = "ego_nodeset",
  nodeset = lazeganet_nodeset,
  attrname = "Gender",
  cond = "eq",
  attrvalue = "M"
)

th_info(ego_nodeset)

ego_net <- th_subnet("ego_net", lazeganet, ego_nodeset)
th_info(ego_net)

ego <- 42
alters <- th_get_node_alters(lazeganet, ego, layernames = "friends", unique = TRUE)
ego_nodes <- unique(c(ego, alters))

# 1) create an empty nodeset
ego_nodeset <- th_create_nodeset("ego_nodeset")

# 2) add ego + alters
for (id in ego_nodes) {
  th_add_node(ego_nodeset, id)
}
th_add_node(ego_nodeset, 65)
# 3) subnet
ego_net <- th_subnet("ego_net", lazeganet, ego_nodeset)

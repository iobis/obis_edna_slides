#!/usr/bin/env Rscript

# Refresh the cached OBIS statistics that the slides display.
#
#   Rscript scripts/fetch-obis-stats.R           fetch and rewrite the cache
#   Rscript scripts/fetch-obis-stats.R --check   report what would change, write nothing
#
# This is deliberately NOT part of the render. Nothing in _quarto.yml calls it,
# and `quarto render` never reaches the network: the slides read the committed
# _variables.yml through {{< var >}}, which is plain text substitution. Adding a
# pre-render hook to _quarto.yml would break that guarantee.
#
# Refreshing is a decision, and it shows up as a reviewable diff in git.

suppressPackageStartupMessages({
  library(httr2)
  library(yaml)
})

STATS_URL <- "https://api.obis.org/statistics"
NODE_URL  <- "https://api.obis.org/node"
OUT       <- "_variables.yml"

# Two of the 39 entries the endpoint returns are not counted as nodes, which is
# why the published figure is 37 rather than 39. Matched on id: node names get
# edited, ids do not.
#
#   OBIS Secretariat  coordinates OBIS rather than publishing to it.
#   OBIS Senegal      excluded for now, per the OBIS team (2026-09-10).
#
# OBIS Panama is NOT excluded: it is a real node, newly joined, and simply has
# not published yet. Recheck this list when the node roster changes.
EXCLUDED_NODE_IDS <- c(
  "OBIS Secretariat" = "310922b4-9d0c-4de1-92d7-9b442d34765b",
  "OBIS Senegal"     = "a31658c6-e934-4e48-bc5b-8da1dccabba6"
)

# Figures OBIS publishes that no endpoint exposes. Maintained by hand; update
# these when OBIS does, and move the date with them.
MANUAL <- list(
  scientists = "6K",
  countries  = "99"
)
MANUAL_AS_OF <- "2026-09-10"

check_only <- "--check" %in% commandArgs(trailingOnly = TRUE)

die <- function(...) {
  message("fetch-obis-stats: ", ...)
  message("Nothing written; ", OUT, " left as it was.")
  quit(status = 1)
}

get_json <- function(url) {
  resp <- tryCatch(
    request(url) |>
      req_user_agent("obis_edna_slides stats refresh") |>
      req_timeout(30) |>
      req_perform(),
    error = function(e) die("could not reach ", url, " (", conditionMessage(e), ")")
  )
  resp_body_json(resp, simplifyVector = TRUE)
}

# A stat that came back missing, non-numeric or nonsensical means the API
# changed shape. Better to stop than to quietly publish a wrong number.
need_count <- function(x, field) {
  if (is.null(x[[field]])) die("'", field, "' missing from the API response")
  v <- suppressWarnings(as.numeric(x[[field]]))
  if (is.na(v) || v <= 0) die("'", field, "' is not a positive number: ", x[[field]])
  v
}

comma <- function(x) formatC(x, format = "d", big.mark = ",")

# Big totals read better spoken than digit by digit: 229,141,362 -> "229.1 million".
short <- function(x) {
  trim <- function(v) sub("\\.0$", "", formatC(v, format = "f", digits = 1))
  if (x >= 1e9)      paste(trim(x / 1e9), "billion")
  else if (x >= 1e6) paste(trim(x / 1e6), "million")
  else               comma(x)
}

# The tile form used on the "OBIS in numbers" slide, matching how OBIS writes
# these itself: 229,141,362 -> "229M", 168,352 -> "168K". Counts only; applying
# this to a year would turn 1103 into "1K".
rounded <- function(x) {
  if (x >= 1e6)      paste0(round(x / 1e6), "M")
  else if (x >= 1e3) paste0(round(x / 1e3), "K")
  else               formatC(x, format = "d")
}

stats <- get_json(STATS_URL)
nodes <- get_json(NODE_URL)

records      <- need_count(stats, "records")
species      <- need_count(stats, "species")
taxa         <- need_count(stats, "taxa")
datasets     <- need_count(stats, "datasets")
specieslevel <- need_count(stats, "specieslevel")

years <- stats$yearrange
if (length(years) != 2 || anyNA(suppressWarnings(as.integer(years)))) {
  die("'yearrange' is not a pair of years: ", paste(years, collapse = ", "))
}
years <- as.integer(years)

if (is.null(nodes$results) || !"id" %in% names(nodes$results)) {
  die("the node list came back without an 'id' column")
}
# Subtracting a fixed 2 would quietly go wrong the day one of these is renamed,
# merged or removed, so require every excluded id to still be present.
missing <- names(EXCLUDED_NODE_IDS)[!EXCLUDED_NODE_IDS %in% nodes$results$id]
if (length(missing) > 0) {
  die("no longer in the node list: ", paste(missing, collapse = ", "),
      ". Check https://api.obis.org/node and update EXCLUDED_NODE_IDS before rerunning.")
}
node_count <- nrow(nodes$results) - length(EXCLUDED_NODE_IDS)

now <- Sys.time()
attr(now, "tzone") <- "UTC"

# Emitted by hand rather than via as.yaml so that key order, integer formatting
# and quoting stay stable, which keeps the git diff between refreshes readable.
vals <- list(
  retrieved            = format(now, "%Y-%m-%d"),
  retrieved_utc        = format(now, "%Y-%m-%dT%H:%M:%SZ"),
  source_statistics    = STATS_URL,
  source_nodes         = NODE_URL,
  records              = records,
  records_fmt          = comma(records),
  records_short        = short(records),
  records_round        = rounded(records),
  specieslevel         = specieslevel,
  specieslevel_fmt     = comma(specieslevel),
  specieslevel_short   = short(specieslevel),
  specieslevel_round   = rounded(specieslevel),
  species              = species,
  species_fmt          = comma(species),
  species_round        = rounded(species),
  taxa                 = taxa,
  taxa_fmt             = comma(taxa),
  taxa_round           = rounded(taxa),
  datasets             = datasets,
  datasets_fmt         = comma(datasets),
  datasets_round       = rounded(datasets),
  nodes                = node_count,
  nodes_fmt            = comma(node_count),
  nodes_round          = rounded(node_count),
  year_min             = years[1],
  year_max             = years[2]
)

emit <- function(v) if (is.numeric(v)) formatC(v, format = "d") else paste0('"', v, '"')

yaml_line <- function(k, v) sprintf("  %s: %s", k, emit(v))

lines <- c(
  paste0("# Generated by scripts/fetch-obis-stats.R on ", vals$retrieved, ". Do not edit by hand."),
  "# Refresh with: Rscript scripts/fetch-obis-stats.R",
  "",
  "obis:",
  vapply(names(vals), function(k) yaml_line(k, vals[[k]]), character(1)),
  "",
  paste0("  # Not from the API. Maintained by hand in the script, as of ", MANUAL_AS_OF, "."),
  vapply(names(MANUAL), function(k) yaml_line(k, MANUAL[[k]]), character(1)),
  ""
)

previous <- if (file.exists(OUT)) read_yaml(OUT)$obis else NULL

reported <- c(vals, MANUAL)

changed <- Filter(Negate(is.null), lapply(names(reported), function(k) {
  if (grepl("^retrieved", k)) return(NULL)          # always moves; not news
  old <- previous[[k]]
  if (is.null(old)) return(sprintf("  %-20s -> %s (new)", k, reported[[k]]))
  if (as.character(old) != as.character(reported[[k]]))
    sprintf("  %-20s %s -> %s", k, old, reported[[k]])
  else NULL
}))

if (length(changed) == 0 && !is.null(previous)) {
  message("No change since ", previous$retrieved, ".")
} else {
  message(if (is.null(previous)) "New cache:" else "Changes:")
  message(paste(unlist(changed), collapse = "\n"))
}

if (check_only) {
  message("\n--check: nothing written.")
  quit(status = 0)
}

# Write to a temp file in the same directory and move it into place, so an
# interrupted run cannot leave a half-written cache behind.
tmp <- tempfile("obis-stats-", tmpdir = dirname(normalizePath(OUT, mustWork = FALSE)))
writeLines(lines, tmp)
if (!file.rename(tmp, OUT)) {
  unlink(tmp)
  die("could not move the new cache into place at ", OUT)
}

message("\nWrote ", OUT, " (retrieved ", vals$retrieved_utc, ").")

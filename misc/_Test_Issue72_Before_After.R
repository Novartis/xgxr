# Before/after demonstration for GitHub issue #72
# https://github.com/Novartis/xgxr/issues/72
#
#   xgx_stat_ci(breaks = ...) binned incorrectly when combined with a log10 x
#   scale, because the user supplies `breaks` on the scale of the original data
#   while the data reaching the stat has already been transformed by the scale.
#
# HOW THIS SCRIPT STAYS WORKING
# -----------------------------
# It does not contain a copy of the old code.  Instead it checks the pinned
# pre-fix commit out into a temporary git worktree, and compares it against
# whatever is in your working tree right now.  So "before" is frozen at the
# commit below, and "after" tracks the package as it continues to be developed.
# Nothing here needs updating when xgx_stat_ci changes.
#
# Each side is run in its own R subprocess, because two different versions of
# the same package cannot be loaded into one session.
#
# USAGE
#   Rscript misc/_Test_Issue72_Before_After.R
#   Rscript misc/_Test_Issue72_Before_After.R <ref>   # compare against any ref
#
# Requires: git, devtools, ggplot2.

# The last commit before the issue #72 fix.  Do not change this.
BEFORE_REF_DEFAULT <- "2f8f6872142c159071308b60cb5794834336c406"

args <- commandArgs(trailingOnly = TRUE)
before_ref <- if (length(args) >= 1) args[[1]] else BEFORE_REF_DEFAULT

repo <- tryCatch(
  system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE,
          stderr = FALSE),
  warning = function(w) stop("this script must be run from inside the xgxr git repo"))
repo <- normalizePath(repo)

# ---------------------------------------------------------------------------
# The probe: run against one version of the package, return a tidy summary.
# ---------------------------------------------------------------------------
# This is written to a file and sourced by each subprocess.  It deliberately
# uses only long-standing public API so that it runs against old and new code
# alike.
probe_src <- '
suppressMessages(library(ggplot2))
args <- commandArgs(trailingOnly = TRUE)
pkg_dir <- args[[1]]
out_rds <- args[[2]]

suppressMessages(devtools::load_all(pkg_dir, quiet = TRUE))

set.seed(1234)
df <- data.frame(
  x = c(runif(300, 0.4, 2.8), runif(142, 2.8, 5.2),
        runif(25, 5.2, 7.6), runif(3, 7.6, 10)),
  y = rbinom(470, 1, 0.3))
breaks <- quantile(df$x)

set.seed(5678)
ord <- data.frame(
  x = 10^runif(400, -0.5, 1),
  response = factor(sample(c("Mild", "Moderate", "Severe"), 400, TRUE)))
ord_breaks <- quantile(ord$x)

grab <- function(p) {
  suppressMessages(suppressWarnings(
    tryCatch(layer_data(p), error = function(e) conditionMessage(e))))
}

base <- ggplot(df, aes(x = x, y = y)) +
  xgx_stat_ci(breaks = breaks, geom = "point")
ord_base <- ggplot(ord, aes(x = x, response = response, colour = response)) +
  xgx_stat_ci(distribution = "ordinal", breaks = ord_breaks, geom = "point")

saveRDS(list(
  ggplot2   = as.character(packageVersion("ggplot2")),
  breaks    = breaks,
  linear    = grab(base),
  logged    = grab(base + xgx_scale_x_log10()),
  ord_breaks = ord_breaks,
  ord_linear = grab(ord_base),
  ord_logged = grab(ord_base + xgx_scale_x_log10())
), out_rds)
'

tmp <- file.path(tempdir(), paste0("xgxr-issue72-", Sys.getpid()))
dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
probe_file <- file.path(tmp, "probe.R")
writeLines(probe_src, probe_file)

worktree <- file.path(tmp, "before")

cleanup <- function() {
  if (dir.exists(worktree)) {
    system2("git", c("-C", shQuote(repo), "worktree", "remove", "--force",
                     shQuote(worktree)), stdout = FALSE, stderr = FALSE)
  }
  unlink(tmp, recursive = TRUE)
}
on.exit(cleanup(), add = TRUE)

run_probe <- function(pkg_dir, label) {
  out_rds <- file.path(tmp, paste0(label, ".rds"))
  status <- system2("Rscript", c(shQuote(probe_file), shQuote(pkg_dir),
                                 shQuote(out_rds)),
                    stdout = FALSE, stderr = FALSE)
  if (status != 0 || !file.exists(out_rds)) {
    stop("probe failed for '", label, "' (", pkg_dir, ")")
  }
  readRDS(out_rds)
}

message("Checking out ", substr(before_ref, 1, 10), " into a temporary worktree ...")
status <- system2("git", c("-C", shQuote(repo), "worktree", "add", "--detach",
                           shQuote(worktree), before_ref),
                  stdout = FALSE, stderr = FALSE)
if (status != 0) stop("could not create git worktree for ref '", before_ref, "'")

message("Running probe against BEFORE ...")
before <- run_probe(worktree, "before")
message("Running probe against AFTER (your working tree) ...")
after <- run_probe(repo, "after")

# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------
rule <- function(txt) cat("\n", txt, "\n", strrep("-", nchar(txt)), "\n", sep = "")

fmt <- function(d, expected_rows) {
  if (is.character(d)) return(paste("ERROR:", d))
  paste0(nrow(d), "/", expected_rows, " rows   y = ",
         paste(sprintf("%.4f", d$y), collapse = ", "))
}

cat("\nxgxr issue #72: before/after\n")
cat("  before : ", substr(before_ref, 1, 10), "\n", sep = "")
cat("  after  : working tree at ", repo, "\n", sep = "")
cat("  ggplot2: ", after$ggplot2, "\n", sep = "")

n_bin <- length(after$breaks) - 1L
n_ord <- n_bin * 3L

rule("Continuous, breaks = quantile(x), NO log scale (control)")
cat("  before: ", fmt(before$linear, n_bin), "\n", sep = "")
cat("  after : ", fmt(after$linear,  n_bin), "\n", sep = "")

rule("Continuous, breaks = quantile(x), WITH xgx_scale_x_log10()")
cat("  before: ", fmt(before$logged, n_bin), "   <- bins collapse\n", sep = "")
cat("  after : ", fmt(after$logged,  n_bin), "   <- matches control\n", sep = "")

rule("Ordinal, breaks = quantile(x), NO log scale (control)")
cat("  before: ", fmt(before$ord_linear, n_ord),
    "   <- extra NA bin\n", sep = "")
cat("  after : ", fmt(after$ord_linear,  n_ord), "\n", sep = "")

rule("Ordinal, breaks = quantile(x), WITH xgx_scale_x_log10()")
cat("  before: ", fmt(before$ord_logged, n_ord), "\n", sep = "")
cat("  after : ", fmt(after$ord_logged,  n_ord), "   <- matches control\n",
    sep = "")

# The invariant the fix restores: binning is unchanged by the presence of a
# log10 scale, so the summary statistics must be identical.
holds <- function(v) {
  if (is.character(v$linear) || is.character(v$logged)) return(FALSE)
  isTRUE(all.equal(v$logged$y, v$linear$y))
}
rule("Invariant: log10 scale must not change the binned estimates")
cat("  before: ", if (holds(before)) "HOLDS" else "VIOLATED", "\n", sep = "")
cat("  after : ", if (holds(after))  "HOLDS" else "VIOLATED", "\n", sep = "")
cat("\n")

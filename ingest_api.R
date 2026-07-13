#!/usr/bin/env Rscript

suppressWarnings(suppressMessages({
  library(plumber)
  library(jsonlite)
}))

cache_path <- function() {
  Sys.getenv("XROAD_CACHE_PATH", unset = "/data/xroad/xroad_cache.rds")
}

json_error <- function(res, status, message) {
  res$status <- status
  list(ok = FALSE, error = message)
}

authorized <- function(req) {
  expected <- Sys.getenv("OPENFN_INGEST_TOKEN", unset = "")
  supplied <- req$HTTP_AUTHORIZATION %||% ""
  nzchar(expected) && identical(supplied, paste("Bearer", expected))
}

`%||%` <- function(value, fallback) {
  if (is.null(value) || length(value) == 0) fallback else value
}

health_handler <- function() {
  list(ok = TRUE, service = "itafaray-xroad-ingest")
}

ingest_handler <- function(req, res) {
  if (!nzchar(Sys.getenv("OPENFN_INGEST_TOKEN", unset = ""))) {
    return(json_error(res, 503, "OPENFN_INGEST_TOKEN is not configured on the server."))
  }
  if (!authorized(req)) {
    return(json_error(res, 401, "Invalid bearer token."))
  }

  cache <- cache_path()
  lock <- paste0(cache, ".ingest.lock")

  if (dir.exists(lock)) {
    lock_age <- as.numeric(difftime(Sys.time(), file.info(lock)$mtime, units = "secs"))
    if (is.finite(lock_age) && lock_age > 3600) {
      unlink(lock, recursive = TRUE, force = TRUE)
    }
  }
  if (!dir.create(lock, showWarnings = FALSE, recursive = FALSE)) {
    return(json_error(res, 409, "An X-Road ingestion is already running."))
  }
  on.exit(unlink(lock, recursive = TRUE, force = TRUE), add = TRUE)

  started_at <- Sys.time()
  output <- system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = "ingest_xroad.R",
    stdout = TRUE,
    stderr = TRUE
  )
  exit_code <- attr(output, "status") %||% 0L

  if (!identical(as.integer(exit_code), 0L)) {
    res$status <- 502
    return(list(
      ok = FALSE,
      error = "ingest_xroad.R failed; the previous cache was preserved.",
      exit_code = as.integer(exit_code),
      output = unname(output)
    ))
  }

  info <- file.info(cache)
  list(
    ok = TRUE,
    cache_path = cache,
    cache_size_bytes = unname(info$size),
    cache_modified_at = format(info$mtime, "%Y-%m-%dT%H:%M:%S%z"),
    duration_seconds = round(as.numeric(difftime(Sys.time(), started_at, units = "secs")), 3),
    output = unname(output)
  )
}

port <- as.integer(Sys.getenv("INGEST_API_PORT", unset = "8000"))
pr() |>
  pr_get("/health", health_handler, serializer = serializer_unboxed_json()) |>
  pr_post("/ingest", ingest_handler, serializer = serializer_unboxed_json()) |>
  pr_run(host = "0.0.0.0", port = port)

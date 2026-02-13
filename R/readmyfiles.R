#' Read My Files
#'
#' Reads all readable data files in a directory (and optionally subdirectories),
#' filters by file size, attempts to load them into R, and returns them as a list.
#' Optionally produces plots of file sizes and memory usage.
#'
#' @param path Character string. Path to the directory to scan for files.
#' @param max_kb Numeric. Maximum file size (in kilobytes) to attempt to read.
#' @param include_subdirs Logical. If TRUE, also search subdirectories.
#' @param make_plots Logical. If TRUE, generate plots of file sizes and memory usage.
#'
#' @returns A named list of R objects successfully read from files.
#' @export
#'
#' @examples
#' #' \dontrun{
#' files <- readmyfiles(path = "data", max_kb = 1000)
#' names(files)
#' }
readmyfiles <- function(
    path = ".",
    max_kb = 500,
    include_subdirs = FALSE,
    make_plots = TRUE
) {
  ###########################################################################
  # 1. DEFINE SUPPORTED FILE EXTENSIONS THAT R CAN READ
  ###########################################################################
  # These are common formats that R can read via base or imported packages.
  readable_ext <- c(
    "txt", "csv", "tsv", "dat", "fwf",
    "json", "xml", "yaml", "yml",
    "xls", "xlsx",
    "rds", "rda", "RData",
    "sav", "dta", "sas7bdat", "xpt",
    "feather", "parquet", "fst",
    "html", "md",
    "db", "sqlite"
  )

  ###########################################################################
  # 2. LIST SUBDIRECTORIES AND THEIR FILES
  ###########################################################################
  # List immediate subdirectories and what files they contain (for reporting).
  subdirs <- list.dirs(path, full.names = TRUE, recursive = FALSE)
  if (length(subdirs) > 0) {
    message("Subdirectories found under '", path, "':")
    for (sd in subdirs) {
      sd_files <- list.files(sd, full.names = FALSE)
      message("  - ", sd, ":")
      if (length(sd_files) == 0) {
        message("      (no files)")
      } else {
        for (sf in sd_files) {
          message("      ", sf)
        }
      }
    }
  } else {
    message("No subdirectories found under '", path, "'.")
  }

  ###########################################################################
  # 3. LIST FILES (OPTIONALLY INCLUDING SUBDIRECTORIES)
  ###########################################################################
  # If include_subdirs = TRUE, recursively scan subdirectories.
  if (include_subdirs) {
    files <- list.files(path, full.names = TRUE, recursive = TRUE)
  } else {
    files <- list.files(path, full.names = TRUE, recursive = FALSE)
  }

  # Extract file extensions and keep only those we know how to read.
  ext <- tools::file_ext(files)
  keep <- ext %in% readable_ext
  files <- files[keep]
  ext <- ext[keep]

  ###########################################################################
  # 4. FILE METADATA (ALL AVAILABLE INFO)
  ###########################################################################
  # file.info() returns a data frame with full metadata: size, isdir, mode,
  # mtime, ctime, atime, uid, gid, uname, grname (where available).
  info <- file.info(files)
  info$size_kb <- info$size / 1024
  info$ext <- ext
  info$filename <- basename(files)
  info$fullpath <- files

  ###########################################################################
  # 5. OPTIONAL GRAPHIC OF FILE SIZES (SEPARATE WINDOW)
  ###########################################################################
  if (make_plots && nrow(info) > 0) {
    grDevices::dev.new()  # open a new graphics device/window
    p <- ggplot2::ggplot(
      info,
      ggplot2::aes(
        x = stats::reorder(filename, size_kb),
        y = size_kb,
        fill = ext
      )
    ) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Readable Files in Directory",
        x = "File",
        y = "Size (KB)"
      ) +
      ggplot2::theme_minimal()
    print(p)
  }

  ###########################################################################
  # 6. READ FILES SMALLER THAN max_kb WITH PROGRESS BAR
  ###########################################################################
  # Identify files below the size threshold.
  small_files <- info$size_kb < max_kb
  files_to_read <- info$fullpath[small_files]
  exts_to_read <- info$ext[small_files]

  # Prepare output list and container for unread files.
  out_list <- list()
  unread <- data.frame(
    filename = character(),
    size_kb = numeric(),
    stringsAsFactors = FALSE
  )

  # Progress bar to show reading progress.
  pb <- progress::progress_bar$new(
    format = "Reading files [:bar] :percent :eta",
    total = length(files_to_read),
    clear = FALSE,
    width = 60
  )

  # Loop over files and attempt to read based on extension.
  for (i in seq_along(files_to_read)) {
    pb$tick()

    f <- files_to_read[i]
    e <- exts_to_read[i]
    name <- tools::file_path_sans_ext(basename(f))

    # Attempt to read file; on error, return NULL.
    obj <- tryCatch({
      switch(e,
             csv      = utils::read.csv(f),
             txt      = utils::read.table(f, header = TRUE),
             tsv      = utils::read.delim(f),
             dat      = utils::read.table(f, header = TRUE),
             json     = jsonlite::fromJSON(f),
             xml      = xml2::read_xml(f),
             yaml     = yaml::read_yaml(f),
             yml      = yaml::read_yaml(f),
             xls      = readxl::read_excel(f),
             xlsx     = readxl::read_excel(f),
             rds      = base::readRDS(f),
             rda      = { base::load(f); get(ls()[1]) },
             RData    = { base::load(f); get(ls()[1]) },
             sav      = haven::read_sav(f),
             dta      = haven::read_dta(f),
             sas7bdat = haven::read_sas(f),
             xpt      = haven::read_xpt(f),
             feather  = arrow::read_feather(f),
             parquet  = arrow::read_parquet(f),
             fst      = fst::read_fst(f),
             html     = rvest::read_html(f),
             md       = base::readLines(f),
             db       = DBI::dbConnect(RSQLite::SQLite(), f),
             sqlite   = DBI::dbConnect(RSQLite::SQLite(), f),
             NULL)
    }, error = function(e) NULL)

    # If unreadable, record in 'unread'; otherwise, store in output list.
    if (is.null(obj)) {
      unread <- rbind(
        unread,
        data.frame(
          filename = basename(f),
          size_kb = file.info(f)$size / 1024,
          stringsAsFactors = FALSE
        )
      )
    } else {
      out_list[[name]] <- obj
    }
  }

  ###########################################################################
  # 7. WARNING FOR UNREAD FILES
  ###########################################################################
  if (nrow(unread) > 0) {
    warning(
      paste0(
        "The following files could not be read:\n",
        paste0(
          unread$filename,
          " (",
          round(unread$size_kb, 2),
          " KB)",
          collapse = "\n"
        )
      )
    )
  }

  ###########################################################################
  # 8. MEMORY USAGE PIE CHART (SEPARATE WINDOW)
  ###########################################################################
  # Compute memory used by each element of the list and total memory.
  mem_used <- sapply(out_list, function(x) as.numeric(object.size(x)))
  total_mem <- sum(mem_used)

  if (make_plots && length(mem_used) > 0) {
    mem_df <- data.frame(
      object = names(mem_used),
      kb = mem_used / 1024,
      stringsAsFactors = FALSE
    )

    grDevices::dev.new()  # open a new graphics device/window
    pie_plot <- ggplot2::ggplot(
      mem_df,
      ggplot2::aes(x = "", y = kb, fill = object)
    ) +
      ggplot2::geom_col(width = 1) +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::labs(
        title = paste0(
          "Memory Usage of Returned List (Total: ",
          round(total_mem / 1024, 2), " KB)"
        )
      ) +
      ggplot2::theme_void()

    print(pie_plot)
  }

  ###########################################################################
  # 9. RETURN THE LIST OF SUCCESSFULLY READ OBJECTS
  ###########################################################################
  return(out_list)
}

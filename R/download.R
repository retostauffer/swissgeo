


#' @param url character, URL of the data set/asset to download.
#' @param checksum `NULL` or character. Only used if `dir` is
#'        specified.
#' @param dir character, name/path to a directory to cache the
#'        data sets (see 'Details' for more information).
#'
#' @details Assets (data sets) provided via geo.admin.ch come
#' with a file checksum which can be used to check if a file
#' changed. We use this to cache files.
#'
#' If a directory is specified (`dir`) a `checksum` must be
#' provided. In this case the file downloaded will be stored
#' in the directory specified. If one tries to access the
#' same file again, and its content has not changed since
#' the previous time (i.e., the checksum did not change)
#' we will load the data from the directory if it already
#' exists rather than downloading it again. If not existing,
#' the file will be stored in `dir` after downloading to
#' be (potentially) re-used later.
#'
#' Please note that the the size of the directory can grow
#' fast, especially if this mechanic is used when downloading/accessing
#' data sets that rapidly change (i.e., most recent data) as
#' a new file will be created every time the checksum changes.
#'
#' @author Reto
download_asset <- function(url, checksum = NULL, dir = NULL) {
    stopifnot(
        "argument 'url' must be a valid URL" =
            is.character(url) && length(url) == 1L && grepl("^https\\:\\/\\/", url),
        "argument 'checksum' must be NULL or single character" =
            is.null(checksum) || (is.character(checksum) && length(checksum) == 1L),
        "argument 'dir' must be NULL or single character" =
            is.null(dir) || (is.character(dir) && length(dir) == 1L)
    )

    # If 'dir' is set, that directory must exist
    if (!is.null(dir) && !dir.exists(dir)) stop("directory \"", dir, "\" not found.")
    # IF 'dir' is set checksum must be set as well
    if (!is.null(dir) && is.null(checksum))
        stop("if argument 'dir' is specified, a 'checksum' must be provided.")

    # Specify local file
    file <- if (is.null(dir)) NULL else file.path(dir, sprintf("%s-%s", checksum, basename(url)))
    print(file)


}







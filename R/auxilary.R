
#' Generate Geoportal API URLs
#'
#' @param ... additional arguments to extend the base API URL.
#'        All arguments (after coercion to character) must represent
#'        valid non-empty characters.
#'
#' @return Character of length one with the URL of the API end point.
#'
#' @details On loading the package sets the option `"swissgeo.apiurl"`
#' with the main entry point to the API. This is used inside this
#' function (i.e., by the entire package) to talk to the API.
#'
#' This would allow users to change the end-point (or version)
#' without updating the package, e.g., for testing.
#'
#' @examples
#' ## Base URL
#' sg_api_url()
#'
#' ## Extending the base URL to point to a specific API endpoint
#' sg_api_url("ch.meteoschweiz.ogd-smn")
#' sg_api_url("ch.meteoschweiz.ogd-smn", "items")
#'
#' ## Changing default API URL. Possible, but not a standard use-case.
#' hold_apiurl <- getOption("swissgeo.apiurl") # kept for resetting
#' options(swissgeo.apiurl = "https://some.new.domain/api/stac/v5")
#' sg_api_url()
#'
#' ## Setting back to defaults
#' options(swissgeo.apiurl = hold_apiurl)
#' sg_api_url()
#'
#' @author Reto
#' @export
sg_api_url <- function(...) {
    args <- list(...)

    # No arguments specified? Set NULL. Else we check
    # that all elements are valid characters.
    if (length(args) == 0L) {
        args <- NULL
    } else {
        args <- lapply(args, as.character)
        stopifnot(
            "arguments must all be non-empty characters" =
                all(sapply(args, is.character)) &&
                all(sapply(args, length) == 1L) &&
                all(sapply(args, nchar) > 0L)
        )
    }

    apiurl <- getOption("swissgeo.apiurl")
    if (is.null(args)) return(apiurl)

    # Else extending the URL
    return(paste0(apiurl, "/", paste(args, collapse = "/")))
}

#' Show HTTP Error Status and Terminate
#'
#' This function is called whenever \code{httr::GET} returns an
#' http status code out of the \code{200} range (success).
#' Shows \code{\link[httr]{http_status}} code information alongside
#' with additional messages returned by the API (if any).
#'
#' @param scode numeric, http status code.
#' @param xtra \code{NULL} or named list with additional information.
#'
#' @return No return, will terminate R.
#'
#' @author Reto Stauffer
#' @importFrom httr http_status
# Show http_status message if possible.
show_http_status_and_terminate <- function(scode, xtra = NULL) {

    stopifnot("argument 'scode' must be numeric of length 1L" =
              is.numeric(scode), length(scode) == 1)
    if (scode %/% 100 == 2) return(NULL)

    # Given we have to deal with the return code, ensure
    # 'xtra' is a named list or NULL
    stopifnot("argument 'xtra' must be NULL or a named list of length > 0L" =
        is.null(xtra) || (is.list(xtra) && !is.null(names(xtra)) && length(xtra) > 0L))

    cat('---\n')

    info <- tryCatch(http_status(scode),
                     error = function(x) NULL)

    # Depending on the status code these are somewhat redundant
    if (!is.null(xtra))
        xtra <- paste(c("  status returned by API:",
                      sprintf("    %-20s %s", sprintf("%s:", names(xtra)), xtra)),
                      collapse = "\n")
    if (!is.null(info))
        info <- paste(c("  http_status description:",
                      sprintf("    %-20s %s", sprintf("%s:", names(info)), info)),
                      collapse = "\n")

    if (is.null(info) & is.null(xtra)) {
        stop("HTTP request error: server returned status code ", scode)
    } else {
        stop(paste("HTTP request error", xtra, info, sep = "\n"))
    }
}


#' Converting Datetime Columns
#'
#' Takes a data frame as input and tries to identify columns
#' containing datetime information.
#'
#' @param x data frame.
#'
#' @return Data frame with POSIXct variables if any variable
#' containing datetime information was detected.
#'
#' @details Data received by the API or read via the files
#' provided regularey contain date or datetime information
#' as character strings.
#'
#' This function takes a data frame and tries to identify
#' variables/columns containing date and time information
#' to coerce the information into Date or POSIXct objects.
#' Currently, the following formats are checked currently:
#'
#' * `2026-01-16T17:34:34(...)Z`: converted to POSIXct
#' * `16.01.2026`: converted to Date
#' * `2026-01-16`: converted to Date
#' * `16.01.2026 12:03`: converted to POSIXct w/ time zone Europe/Zuerich
#' * `16.01.2026 00:00`: converted to Date
#'
#' @examples
#' \dontrun{
#' autoconvert_datetime(data.frame(a = 1, b = "2026-01-16T12:34:56.23435Z"))
#' autoconvert_datetime(data.frame(a = 1, b = "2026-01-16T12:34:56Z"))
#' autoconvert_datetime(data.frame(a = 1, b = "16.01.2026 12:34"))
#' autoconvert_datetime(data.frame(a = 1, b = "16.01.2026 00:00"))
#' autoconvert_datetime(data.frame(a = 1, b = "16.01.2026"))
#' autoconvert_datetime(data.frame(a = 1, b = "2026-01-16"))
#' }
#'
#' @author Reto
#' @importFrom parsedate parse_iso_8601
autoconvert_datetime <- function(x) {
    stopifnot(is.data.frame(x))
    if (nrow(x) == 0L) return(x)

    get_idx <- function(x, pattern) {
        tmp <- sapply(x, function(x) grepl(pattern, x) | is.na(x))
        which(if (is.matrix(tmp)) colSums(tmp) == nrow(x) else tmp)
    }

    # Checking for ISO8601 format (Y-m-dTH:M:S..) and convert, if found
    idx <- get_idx(x, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}")
    for (i in idx) x[[i]] <- parse_iso_8601(x[[i]])

    # Checking for 'german date' and convert, if found.
    idx <- get_idx(x, "^[0-9]{2}.[0-9]{2}.[0-9]{4}$")
    for (i in idx) x[[i]] <- as.Date(x[[i]], format = "%d.%m.%Y")

    # Checking for 'date' and convert, if found.
    idx <- get_idx(x, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
    for (i in idx) x[[i]] <- as.Date(x[[i]], format = "%Y-%m-%d")

    # Checking for 'german date and time' and convert, if found.
    idx <- get_idx(x, "^[0-9]{2}.[0-9]{2}.[0-9]{4}\\s+[0-9]{2}:[0-9]{2}")
    for (i in idx) x[[i]] <- as.POSIXct(x[[i]], format = "%d.%m.%Y %H:%M", tz = "Europe/Zurich")

    return(x)
}

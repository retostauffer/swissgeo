
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

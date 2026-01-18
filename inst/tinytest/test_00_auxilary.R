# -------------------------------------------------------
# Checking auxilary functions/helper functions
# -------------------------------------------------------

if (interactive()) { library("tinytest"); library("swissgeo") }


# -------------------------------------------------------
# HTTP error handler function
# -------------------------------------------------------

# First at all, check that the function (not exported) exists
expect_true(is.function(swissgeo:::show_http_status_and_terminate),
            info = "Hidden function swissgeo:::show_http_status_and_terminate exists")

# Incorrect use/sanity checks
expect_error(swissgeo:::show_http_status_and_terminate("foo"),
             info = "Argument 'scode' expected to be numeric")
expect_error(swissgeo:::show_http_status_and_terminate(1:3),
             info = "Argument 'scode' expected to be of length 1L")
expect_error(swissgeo:::show_http_status_and_terminate(404, xtra = "foo"),
             info = "Argument 'xtra' expected to be NULL or a named list > 1L")
expect_error(swissgeo:::show_http_status_and_terminate(404, xtra = list(1, 2, 3)),
             info = "Argument 'xtra' expected to be NULL or a named list > 1L")
expect_error(swissgeo:::show_http_status_and_terminate(404, xtra = list()),
             info = "Argument 'xtra' expected to be NULL or a named list > 1L")


# If the status code is in the 200 range, the function
# simply returns NULL (no problem found)
expect_null(swissgeo:::show_http_status_and_terminate(200),
            info = "Returning NULL, successful request (200).")
expect_null(swissgeo:::show_http_status_and_terminate(200.0),
            info = "Returning NULL, successful request (200.0; numeric).")
expect_null(swissgeo:::show_http_status_and_terminate(202),
            info = "Returning NULL, successful request (202).")
expect_null(swissgeo:::show_http_status_and_terminate(299),
            info = "Returning NULL, successful request (299).")

# Testing some common errors
expect_error(swissgeo:::show_http_status_and_terminate(404),
             info = "HTTP response error 404")
expect_error(swissgeo:::show_http_status_and_terminate(503),
             info = "HTTP response error 503")

# Custom error for status codes not catched by the httr package
expect_error(swissgeo:::show_http_status_and_terminate(6020),
             pattern = "HTTP request error\\: server returned status code 6020",
             info = "HTTP response error (unknown/non-standard; 6020)")

# Custom message (xtra)
expect_null(swissgeo:::show_http_status_and_terminate(200, xtra = "foo"),
            info = "Returning NULL, successful request (200); argument 'xtra' has no effect.")


# Custom additional information via xtra (named list)
expect_error(swissgeo:::show_http_status_and_terminate(404, xtra = list(foo = "bar")),
             pattern = "foo\\:\\s+bar",
             info = "Error with custom message expected.")
expect_error(swissgeo:::show_http_status_and_terminate(404, xtra = list(foo = "bar", test = 12345)),
             pattern = "foo\\:\\s+bar.*test\\:\\s+12345",
             info = "Error with custom message expected.")


# -------------------------------------------------------
# Generating API URLs/URIs
# -------------------------------------------------------

# Getting API base URL set by .onAttach function
apiurl <- getOption("swissgeo.apiurl")
expect_true(is.character(apiurl) && length(apiurl) == 1L &&
            grepl("^https\\:\\/\\/", apiurl),
            info = "Option 'swissgeo.apiurl' is set and valid character.")

expect_true(is.function(sg_api_url),
            info = "Function to generate API URLs exist.")

# Sanity checks
expect_error(sg_api_url("foo", ""),
            info = "Error thrown when arguments contain empty strings.")
expect_error(sg_api_url(list(1, 2, 3)),
            info = "Error thrown when arguments can't be coerced to valid character.")
expect_error(sg_api_url(mean),
            info = "Error thrown when arguments can't be coerced to valid character.")

# Default usage: getting base url
expect_identical(sg_api_url(), apiurl,
            info = "Getting base API url")
expect_identical(sg_api_url(1234), paste0(apiurl, "/1234"),
            info = "API URL correctly extended by /1234.")
expect_identical(sg_api_url("foo"), paste0(apiurl, "/foo"),
            info = "API URL correctly extended by /foo.")
expect_identical(sg_api_url("foo/bar"), paste0(apiurl, "/foo/bar"),
            info = "API URL correctly extended by /foo/bar.")
expect_identical(sg_api_url("foo", "bar"), paste0(apiurl, "/foo/bar"),
            info = "API URL correctly extended by /foo/bar.")


# -------------------------------------------------------
# Automatically converting datetime variables/columns
# -------------------------------------------------------

# Getting API base URL set by .onAttach function
expect_true(is.function(swissgeo:::autoconvert_datetime),
            info = "Non-exported function autoconvert_datetime exists.")
expect_error(swissgeo:::autoconvert_datetime(3),
            info = "Error if input is not a data.frame.")
emptydf <- subset(data.frame(a = 1, b = 2, c = "foo"), a < 0)
expect_identical(swissgeo:::autoconvert_datetime(emptydf), emptydf,
            info = "If the input is an empty df, the return is identical to the input.")

d <- data.frame(a = 1, b = "test", c = "2026-01-16T12:34:56.23435Z")
expect_silent(r <- swissgeo:::autoconvert_datetime(d), info = "Function is silent.")
expect_identical(dim(d), dim(r), info = "Return of same dimension as the input.")
expect_identical(names(d), names(r), info = "Names of return identical to input.")
expect_inherits(r$c, "POSIXct", info = "Column $c converted to POSIXct.")
rm(d, r)

d <- data.frame(a = 1, b = "test", c = "2026-01-16T12:34:56Z")
expect_silent(r <- swissgeo:::autoconvert_datetime(d), info = "Function is silent.")
expect_identical(dim(d), dim(r), info = "Return of same dimension as the input.")
expect_identical(names(d), names(r), info = "Names of return identical to input.")
expect_inherits(r$c, "POSIXct", info = "Column $c converted to POSIXct.")
rm(d, r)

d <- data.frame(a = 1, b = "test", c = "16.01.2026 12:34")
expect_silent(r <- swissgeo:::autoconvert_datetime(d), info = "Function is silent.")
expect_identical(dim(d), dim(r), info = "Return of same dimension as the input.")
expect_identical(names(d), names(r), info = "Names of return identical to input.")
expect_inherits(r$c, "POSIXct", info = "Column $c converted to POSIXct.")
rm(d, r)

d <- data.frame(a = 1, b = "test", c = "16.01.2026 00:00")
expect_silent(r <- swissgeo:::autoconvert_datetime(d), info = "Function is silent.")
expect_identical(dim(d), dim(r), info = "Return of same dimension as the input.")
expect_identical(names(d), names(r), info = "Names of return identical to input.")
expect_inherits(r$c, "POSIXct", info = "Column $c converted to POSIXct.")
rm(d, r)

d <- data.frame(a = 1, b = "test", c = "16.01.2026")
expect_silent(r <- swissgeo:::autoconvert_datetime(d), info = "Function is silent.")
expect_identical(dim(d), dim(r), info = "Return of same dimension as the input.")
expect_identical(names(d), names(r), info = "Names of return identical to input.")
expect_inherits(r$c, "Date", info = "Column $c converted to Date.")
rm(d, r)

d <- data.frame(a = 1, b = "test", c = "2026-01-16")
expect_silent(r <- swissgeo:::autoconvert_datetime(d), info = "Function is silent.")
expect_identical(dim(d), dim(r), info = "Return of same dimension as the input.")
expect_identical(names(d), names(r), info = "Names of return identical to input.")
expect_inherits(r$c, "Date", info = "Column $c converted to Date.")
rm(d, r)




# -------------------------------------------------------
# Checking auxilary functions/helper functions
# -------------------------------------------------------

if (interactive()) { library("tinytest"); library("swissgeo") }


# -------------------------------------------------------
# HTTP error handler function
# -------------------------------------------------------

formals(sg_collections)

# First at all, check that the function
expect_true(is.function(sg_collections),
            info = "Function swissgeo:::show_http_status_and_terminate exists")
# Checking default arguments
expect_identical(as.list(formals(sg_collections)),
                 list(pattern = NULL, verbose = FALSE, raw = FALSE),
            info = "Function arguments and defaults as expected.")

# Incorrect use/sanity checks
expect_error(sg_collections(pattern = 1423),
             info = "Argument 'pattern' expected to be valid character if specified.")
expect_error(sg_collections(pattern = ""),
             info = "Argument 'pattern' expected to be valid character if specified.")
expect_error(sg_collections(verbose = c(TRUE, TRUE)),
             info = "Argument 'verbose' expected to be a single logical.")
expect_error(sg_collections(raw = c(TRUE, TRUE)),
             info = "Argument 'raw' expected to be a single logical.")


# Retrieving all collections, raw format
expect_silent(raw <- sg_collections(raw = TRUE),
             info = "Function should be silent by default.")
expect_true(is.list(raw),
             info = "Given raw = TRUE, the return is expected to be a list.")

# Testing raw = FALSE in combination with a pattern
expect_silent(col <- sg_collections(pattern = "meteoschweiz"),
             info = "Function should be silent.")
expect_inherits(col, c("tbl"),
             info = "Collections returned as a tbl data.frame.")
expect_true(all(dim(col) > 0L),
             info = "Returned collection object expected to have positive dimensions.")
expect_true(all(grepl("meteoschweiz", col$id)),
             info = "All collection IDs must match the pattern specified.")
rm(col)

# Warning if the pattern does not match anything, and returns NULL
expect_warning(col <- sg_collections(pattern = "this_pattern_does_not_exist"),
               pattern = "No collection ids matching.*returning NULL",
             info = "No collections matching pattern, expecting warning.")
expect_null(col,
             info = "Returning NULL if no collections could be found.")
rm(col)


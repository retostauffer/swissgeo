# -------------------------------------------------------
# Checking auxilary functions/helper functions
# -------------------------------------------------------

if (interactive()) { library("tinytest"); library("swissgeo") }


# The collection ID we use for testing
CID <- "ch.meteoschweiz.ogd-smn"

# -------------------------------------------------------
# HTTP error handler function
# -------------------------------------------------------


# First at all, check that the function
expect_true(is.function(sg_items),
            info = "Function sg_items exists.")
# Checking default arguments
expect_identical(formals(sg_items),
                 as.pairlist(alist(id =, verbose = FALSE, raw = FALSE)),
            info = "Function arguments and defaults as expected.")

# Incorrect use/sanity checks
expect_error(sg_items(id = 1423),
             info = "Error if id is not a character of length 1.")
expect_error(sg_items(id = c("foo", "bar")),
             info = "Error if id is not a character of length 1.")
expect_error(sg_items(id = ""),
             info = "Error if id is an empty character.")

expect_error(sg_items(verbose = c(TRUE, TRUE)),
             info = "Argument 'verbose' expected to be a single logical.")
expect_error(sg_items(raw = c(TRUE, TRUE)),
             info = "Argument 'raw' expected to be a single logical.")

#######################

# Retrieving all collections, raw format
expect_silent(raw <- sg_items(CID, raw = TRUE),
             info = "Function should be silent by default.")
expect_true(is.list(raw),
             info = "Given raw = TRUE, the return is expected to be a list.")

# Testing raw = FALSE in combination with a pattern
expect_silent(col <- sg_items(CID),
             info = "Function should be silent.")
expect_inherits(col, "sf",
             info = "Items object returned as a simple feature data frame.")
expect_inherits(st_geometry(col), "sfc_POINT",
             info = "Geometry of the sf object is expected to be POINT geometries.")
expect_true(all(dim(col) > 0L),
             info = "Returned items object expected to have positive dimensions.")
rm(col)



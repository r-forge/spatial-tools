# For tauboa:
package_dir <- "/Users/jgrn307/Documents/workspace/spatial-tools/pkg/spatial.tools"
package_dir <- "D:\\Users\\jgrn\\workspace\\spatial-tools\\pkg\\spatial.tools"

setwd(package_dir)

require("roxygen2")
roxygenize(package.dir=package_dir,
		roxygen.dir=package_dir,
		copy.package=FALSE,unlink.target=FALSE)
# Writing spatial_sync_raster.Rd

# Tricks for finding non-ASCII lines from http://r.789695.n4.nabble.com/Rd-file-error-non-ASCII-input-and-no-declared-encoding-td2403233.html
tools::showNonASCII( readLines(f)) # f is the path to a .Rd file
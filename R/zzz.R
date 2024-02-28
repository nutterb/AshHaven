.onLoad <- function(libname,pkgname)
{
  options(randomTickSpeed = 3, 
          subchunkSize = 16^3)
}

.onUnload <- function(libPath)
{
  options(randomTickSpeed = NULL,
          subchunkSize = NULL)
}

.onLoad <- function(libname,pkgname)
{
  options(randomTickSpeed = 3)
}

.onUnload <- function(libPath)
{
  options(randomTickSpeed = NULL)
}

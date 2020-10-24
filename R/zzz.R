.onAttach <- function(libname, pkgname)
{
  Ver <- as.character(packageVersion("Snake")[1])
  Msg <- paste("This is Snake, version ", Ver, ". Type 'PlaySnake()' to play a game of snake.", sep = "")

  packageStartupMessage(Msg)
}

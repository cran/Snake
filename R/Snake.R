utils::globalVariables(c("SnakeBG", "SunsetBG"))

PlaySnake <- function(FIELDSIZE = 20, Speed = 0.8, Image = "Snake", Interpolate = FALSE)
{
  Blue <- rgb(red = 0, green = 0, blue = 1)
  Black <- rgb(red = 0, green = 0, blue = 0)
  Red <- rgb(red = 1, green = 0, blue = 0)

  InitSnake <- function()
  {
    Snake <- matrix(ncol = 2, nrow = 5)
    colnames(Snake) <- c("x", "y")

    Snake[1, 1] <- 7
    Snake[1, 2] <- 10
    Snake[2, 1] <- 8
    Snake[2, 2] <- 10
    Snake[3, 1] <- 9
    Snake[3, 2] <- 10
    Snake[4, 1] <- 10
    Snake[4, 2] <- 10
    Snake[5, 1] <- 11
    Snake[5, 2] <- 10

    return(Snake)
  }

  SetPixel <- function(Field, x, y, Col)
  {
    Step <- ncol(Field) / FIELDSIZE

    for (i in (((x - 1) * Step) + 1):(x * Step))
    {
      for (j in (((y - 1) * Step) + 1):(y * Step))
      {
        Field[i, j] <- Col
      }
    }

    return(Field)
  }

  DrawSnake <- function(Snake, Food, FIELDSIZE = 20)
  {
    Field <- FieldImage

    Field <- SetPixel(Field, Food[1, 1], Food[1, 2], Blue) #Food
    Field <- SetPixel(Field, Snake[1, 1], Snake[1, 2], Red) #Snake's head
    for (i in 2:nrow(Snake)) Field <- SetPixel(Field, Snake[i, 1], Snake[i, 2], Black) #Snake's body

    grid::grid.newpage()
    grid::grid.raster(Field, interpolate = Interpolate)
  }

  MoveSnake <- function(Snake, Direction)
  {
    for (i in seq(from = nrow(Snake), to = 2, by = -1)) Snake[i, ] = Snake[(i - 1), ]

    if (Direction == 1) Snake[1, 1] <- Snake[1, 1] + 1
    if (Direction == 2) Snake[1, 2] <- Snake[1, 2] - 1
    if (Direction == 3) Snake[1, 1] <- Snake[1, 1] - 1
    if (Direction == 4) Snake[1, 2] <- Snake[1, 2] + 1

    return(Snake)
  }

  ThrowFood <- function(Snake, FIELDSIZE = 20)
  {
    MakePos <- function()
    {
      FoodPos <- matrix(ncol = 2, nrow = 1)

      FoodPos[1, 1] <- trunc(runif(n = 1, min = 1, max = FIELDSIZE + 0.99999))
      FoodPos[1, 2] <- trunc(runif(n = 1, min = 1, max = FIELDSIZE + 0.99999))

      return(FoodPos)
    }

    FoodPos <- MakePos()

    CollusionFree <- FALSE
    while (!CollusionFree)
    {
      for (i in 1:nrow(Snake))
      {
        if (FoodPos[1, 1] == Snake[i, 1] & FoodPos[1, 2] == Snake[i, 2])
        {
          i <- -1
          break
        }
      }
      if (i == nrow(Snake)) CollusionFree <- TRUE else FoodPos <- MakePos()
    }

    return(FoodPos)
  }

  CheckEat <- function(Snake, Food)
  {
    if (Snake[1, 1] == Food[1, 1] & Snake[1, 2] == Food[1, 2]) return(TRUE) else return(FALSE)
  }

  SnakeCrash <- function(Snake, FIELDSIZE = 20)
  {
    if (Snake[1, 1] < 1 | Snake[1, 1] > FIELDSIZE | Snake[1, 2] < 1 | Snake[1, 2] > FIELDSIZE) return(TRUE) #Border crash

    for (i in 2:nrow(Snake))
    {
      if (Snake[1, 1] == Snake[i, 1] & Snake[1, 2] == Snake[i, 2]) return(TRUE) #Self bite
    }

    return(FALSE)
  }

  Shutdown <- function(FIELDSIZE = 150)
  {
    for (i in 1:(FIELDSIZE / 2))
    {
      Field <- matrix(ncol = FIELDSIZE, nrow = FIELDSIZE, data = rgb(red = 1, green = 1, blue = 1))

      Field[1:(i - 1), ] <- rgb(red = 0, green = 0, blue = 0)
      Field[FIELDSIZE:(FIELDSIZE - i + 1), ] <- rgb(red = 0, green = 0, blue = 0)

      grid::grid.newpage()
      grid::grid.raster(Field, interpolate = FALSE)

      Sys.sleep(0.001)
    }

    for (i in 1:(FIELDSIZE / 2))
    {
      Field[(FIELDSIZE / 2), 1:(i - 1)] <- rgb(red = 0, green = 0, blue = 0)
      Field[(FIELDSIZE / 2), FIELDSIZE:(FIELDSIZE - i + 1)] <- rgb(red = 0, green = 0, blue = 0)

      grid::grid.newpage()
      grid::grid.raster(Field, interpolate = FALSE)

      Sys.sleep(0.001)
    }

    for (i in seq(from = 1, to = 0, by = -0.1))
    {
      Field[(FIELDSIZE / 2), (FIELDSIZE / 2)] <- rgb(red = i, green = i, blue = i)

      grid::grid.newpage()
      grid::grid.raster(Field, interpolate = FALSE)

      Sys.sleep(0.05)
    }
  }

  SetFocus <- function(Window)
  {
    info_sys <- Sys.info() #https://stackoverflow.com/questions/9622287/how-do-i-bring-an-r-tk-window-to-the-front-after-launching-via-rscript-from-anot
    if (info_sys["sysname"] == "Windows")
    {
      Dir <- getwd()
      on.exit(setwd(Dir))
      setwd("C:/")
      shell("powershell -command [void] [System.Reflection.Assembly]::LoadWithPartialName('Microsoft.VisualBasic') ; [Microsoft.VisualBasic.Interaction]::AppActivate('Snake control')")
    } else
    {
      invisible(tcltk::tkwm.deiconify(Window)) #At least, try this.
    }
  }

  if (FIELDSIZE < 12)
  {
    message("Field size must be at least 12.")
    return(invisible(1))
  }

  if (Speed < 0 | Speed > 1)
  {
    message("Speed must be between 0 and 1.")
    return(invisible(1))
  }

  message("Controls: Arrow keys.")
  message("Hit Esc to end game.")
  message("Hit Pause to pause.")
  flush.console()

  if (Image == "Snake") FieldImage <- SnakeBG
  if (Image == "Sunset") FieldImage <- SunsetBG
  if (Image != "Snake" & Image != "Sunset") FieldImage <- matrix(ncol = 200, nrow = 200, data = rgb(red = 1, green = 1, blue = 1))

  Snake <- InitSnake()
  Points <- 0
  Food <- ThrowFood(Snake, FIELDSIZE)

  if (.Device == "null device") dev.new()
  DrawSnake(Snake, Food, FIELDSIZE)

  invisible(readline(prompt = "Press Return to start snake.\n"))
  message("Game started.")
  flush.console()

  SnakeEnvir <- new.env()
  SnakeEnvir$KeyCode <- NA
  tt <- tcltk::tktoplevel()
  invisible(tcltk::tkwm.title(tt, "Snake control"))
  invisible(tcltk::tkwm.geometry(tt, "300x100+9000+500")) #https://stackoverflow.com/questions/14910858/how-to-specify-where-a-tkinter-window-opens
  invisible(tcltk::tkbind(tt, "<Key>", function(K) {SnakeEnvir$KeyCode <- K})) #https://stackoverflow.com/questions/52370937/access-keyboard-buffer-in-r
  SetFocus(tt)

  Dir <- 3
  Pause <- FALSE
  while (TRUE)
  {
    LastBlock <- Snake[nrow(Snake), ]

    if (!is.na(SnakeEnvir$KeyCode))
    {
      if (SnakeEnvir$KeyCode == "Pause")
      {
        if (Pause) message("Pause released.") else message("Game paused.")
        flush.console()
        Pause <- !Pause
      }
      if (SnakeEnvir$KeyCode == "Escape")
      {
        message("Game terminated.")
        break
      }
      if (SnakeEnvir$KeyCode == "Down"  | SnakeEnvir$KeyCode == "s")   if (Dir != 3) Dir <- 1
      if (SnakeEnvir$KeyCode == "Left"  | SnakeEnvir$KeyCode == "a")   if (Dir != 4) Dir <- 2
      if (SnakeEnvir$KeyCode == "Up"    | SnakeEnvir$KeyCode == "w")   if (Dir != 1) Dir <- 3
      if (SnakeEnvir$KeyCode == "Right" | SnakeEnvir$KeyCode == "d")   if (Dir != 2) Dir <- 4

      SnakeEnvir$KeyCode <- NA
    }

    if (!Pause)
    {
      Snake <- MoveSnake(Snake, Dir)

      if (SnakeCrash(Snake, FIELDSIZE))
      {
        message("Game over.")
        break
      }

      DrawSnake(Snake, Food, FIELDSIZE)

      if (CheckEat(Snake, Food))
      {
        Snake <- rbind(Snake, LastBlock)
        Food <- ThrowFood(Snake, FIELDSIZE)

        Points <- Points + 1
        message(paste(Points, " point", ifelse(Points == 1, "", "s"), " scored!", sep = ""))
        flush.console()
      }

      Sys.sleep(1 - Speed)
    }
  }
  tcltk::tkdestroy(tt)

  Sys.sleep(1)
  Shutdown(20)

  return(invisible(0))
}

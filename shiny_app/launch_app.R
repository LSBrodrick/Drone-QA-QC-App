# launch_app.R
install.packages("shiny", dependencies = TRUE)
library(shiny)
runApp("app.R", launch.browser = TRUE)    
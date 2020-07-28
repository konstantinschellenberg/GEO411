#superfun message
message("hi robin, please concentrate")
# change Prompt
options(prompt="R> ")


if(interactive()){
  try(fortunes::fortune(), silent = TRUE)
}

.Last = function() {
  cond = suppressWarnings(!require(fortunes, quietly = TRUE))
  if(cond){
    try(install.packages("fortunes"), silent = TRUE)
    message("Goodbye at ", date(), "\n")
  }
}


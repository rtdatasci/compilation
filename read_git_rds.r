# read rds file from git commit or older git commit


url <- "https://github......rds?raw=true"
download.file(url, "temp2.rds", extra = "-L") # saves in current wd

data <- readRDS("temp2.rds")
as_tibble(data)
str(data)

unlink(paste0(getwd(),"/temp2.rds"))



# install.R
# Install dependencies listed in requirements.txt

# Read requirements
reqs <- readLines("requirements.txt")

# Function to parse package and version
parse_req <- function(line) {
  # Remove comments and trim
  line <- trimws(strsplit(line, "#")[[1]][1])
  if (line == "") return(NULL)
  
  # Handle version specification
  if (grepl("\\(", line)) {
    pkg <- sub("\\s*\\(.*\\)", "", line)
    ver <- sub(".*>=", "", line)
    ver <- sub("\\)", "", ver)
    return(list(pkg = trimws(pkg), ver = trimws(ver)))
  } else {
    return(list(pkg = line, ver = NULL))
  }
}

parsed <- lapply(reqs, parse_req)
parsed <- parsed[!sapply(parsed, is.null)]

# Install or update packages
for (p in parsed) {
  # Install if missing
  if (!requireNamespace(p$pkg, quietly = TRUE)) {
    message("Installing missing package: ", p$pkg)
    install.packages(p$pkg, dependencies = TRUE)
  }
  
  # Update if version requirement exists and installed version is too low
  if (!is.null(p$ver)) {
    installed_ver <- as.character(utils::packageVersion(p$pkg))
    if (utils::compareVersion(installed_ver, p$ver) < 0) {
      message("Updating package: ", p$pkg, " from version ", installed_ver, " to >= ", p$ver)
      install.packages(p$pkg, dependencies = TRUE)
    }
  }
  
  # Detach old namespace if loaded
  if (paste0("package:", p$pkg) %in% search()) {
    detach(paste0("package:", p$pkg), unload = TRUE, character.only = TRUE)
  }
}


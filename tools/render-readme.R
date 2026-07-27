# Render README
quarto::quarto_render(input = "tools/readme.qmd")

# Delete my-results
unlink(
  x = "tools/my-results",
  recursive = TRUE,
  force = TRUE
)

# Delete any summary files
unlink(
  x = list.files(path = "tools", pattern = "summary-", full.names = TRUE),
  force = TRUE
)

# Get agd-run-* directories at root
dirs <- list.dirs(path = "tools", recursive = FALSE)
dirs <- dirs[stringr::str_detect(string = dirs, pattern = "agd-run-")]

# Delete any dirs found
if(length(dirs)) {
  unlink(
    x = dirs,
    recursive = TRUE
  )
}

# Import README
readme <- readLines("tools/README.md")

# Replace windows username
readme <- gsub(
  pattern = config::get("windows_username"),
  replacement = "username",
  x = readme
)

# Replace repo path username
readme <- gsub(
  pattern = config::get("repo_path"),
  replacement = "chms",
  x = readme
)

# Find img tags
img <- stringr::str_detect(
  string = readme,
  pattern = "<img"
) |> which()

# Get first image path
img_path <- readme[img[1]]

# Remove everything before readme_files
img_path <- sub(
  pattern = "^.*?(readme_files)",
  replacement = "\\1",
  x = img_path
)

# Remove everything after .png
img_path <- sub(
  pattern = "(\\.png).*",
  replacement = "\\1",
  x = img_path
)

# Rename image
rename <- file.rename(
  from = paste0("tools/", img_path),
  to = "man/figures/agd-plot.png"
)

# Remove img tags
readme <- c(
  readme[1:(img[1] - 1)],
  paste0("<img src='man/figures/agd-plot.png' style='width: 75%;'>"),
  readme[(img[length(img)] + 2):length(readme)]
)

# Remove readme_files
unlink(
  x = "tools/readme_files",
  recursive = TRUE
)

# Export README
writeLines(text = readme, con = "README.md")

# Delete original README
unlink(x = "tools/README.md", force = TRUE)

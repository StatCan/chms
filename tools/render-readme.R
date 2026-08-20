# Render README
quarto::quarto_render(input = "tools/readme.qmd")

# Delete results
unlink(x = "tools/my-results", recursive = TRUE)

# Get agd-run-* directories at root
dirs <- list.dirs(path = tempdir(), recursive = FALSE)
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
  replacement = "Clippy",
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

# Find img duplicates
img_dup <- img[seq(from = 2, to = length(img), by = 2)]
img_dup <- c(img_dup, img_dup + 1)
img_dup <- img_dup[order(img_dup)]

# Remove img duplicates
readme <- readme[-img_dup]

# Find img tags
img <- stringr::str_detect(
  string = readme,
  pattern = "<img"
) |> which()

# Get image paths
img_path <- stringr::str_extract(readme[img], '(?<=src=")[^"]+')

# Remove man/figures
unlink(x = "man/figures", recursive = TRUE, force = TRUE)

# Create man/figures
dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

# Move image paths to man/figures
move_files <- file.rename(
  from = paste0("tools/", img_path),
  to = paste0("man/figures/", basename(img_path))
)

# Remove readme_files directory
unlink(
  x = "tools/readme_files",
  recursive = TRUE
)

# Replace readme_files/figure-commonmark with man/figures
readme <- gsub(
  pattern = "readme_files/figure-commonmark",
  replacement = "man/figures",
  x = readme
)

# Update img width
readme <- gsub(
  pattern = "width:100.0%",
  replacement = "width: 75%;",
  x = readme
)

# Export README
writeLines(text = readme, con = "README.md")

# Delete original README
unlink(x = "tools/README.md", force = TRUE)

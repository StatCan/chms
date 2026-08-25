# Set paths where README.md will be rendered and stored
render_path <- paste0(tempdir(), "/readme.qmd")
storage_path <- config::get("readme_storage_path")

# Copy tools/readme.qmd to dir
copy <- file.copy(from = "tools/readme.qmd", to = render_path, overwrite = TRUE)

# Load readme.qmd from dir
readme_qmd <- readLines(render_path)

# Add chms local package path to load_all()
readme_qmd <- gsub(
  pattern = "load_all[(][)]",
  replacement = paste0('load_all("', config::get("chms_local_package_path"), '")'),
  x = readme_qmd
)

# Save updated readme.qmd back to dir
writeLines(text = readme_qmd, con = render_path)

# Render README
quarto::quarto_render(input = render_path)

# Import README
readme <- readLines(paste0(dirname(render_path), "/readme.md"))

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

# Remove man/figures from storage_path
unlink(x = paste0(storage_path, "/man/figures"), recursive = TRUE, force = TRUE)

# Create man/figures in storage_path
dir.create(paste0(storage_path, "/man/figures"), recursive = TRUE, showWarnings = FALSE)

# Move image paths to storage_path
move_files <- file.rename(
  from = paste0(dirname(render_path), "/", img_path),
  to = paste0(storage_path, "/man/figures/", basename(img_path))
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
writeLines(text = readme, con = paste0(storage_path, "/README.md"))

# Clean up render_path
unlink(
  x = paste0(dirname(render_path), c("/readme_files", "/readme.qmd", "/README.md")),
  recursive = TRUE
)

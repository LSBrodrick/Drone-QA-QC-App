# ==============================================================================
# BUILD STANDALONE DISTRIBUTION
# Creates a self-contained folder with embedded R Portable + all packages
# No R installation required on the target machine
# ==============================================================================
#
# PREREQUISITES:
#   1. R 4.4+ installed on THIS (build) machine
#   2. Internet connection (downloads R Portable ~90MB + packages)
#   3. All app packages already installed on this machine
#
# USAGE:
#   cd <path-to-shiny_app>
#   Rscript build/build_standalone.R
#
# OUTPUT:
#   build/output/DroneQAQC/          <- self-contained folder
#   build/output/DroneQAQC.zip       <- distributable ZIP
#   Field techs unzip, double-click DroneQAQC.bat, pin to taskbar.
#
# ==============================================================================

cat("============================================================\n")
cat("  Drone QA/QC - Standalone Build\n")
cat("============================================================\n\n")

# --- Configuration ---
app_name     <- "DroneQAQC"
app_version  <- "1.0.0"
r_version    <- paste0(R.version$major, ".", R.version$minor)  # e.g. "4.4.2"
r_major_minor <- paste0(R.version$major, ".", sub("\\..*", "", R.version$minor))  # e.g. "4.4"

# Directories
app_dir    <- normalizePath(file.path(getwd()), winslash = "/")
build_dir  <- normalizePath(file.path(app_dir, "build"), winslash = "/")
output_dir <- file.path(build_dir, "output", app_name)
r_portable_dir <- file.path(output_dir, "R-Portable")

# Packages the app needs
required_pkgs <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyFiles",
  "DT", "plotly", "leaflet",
  "dplyr", "tidyr", "zoo", "purrr",
  "viridis", "RColorBrewer", "scales",
  "exifr", "htmlwidgets", "tiff",
  "rmarkdown", "tinytex", "kableExtra"
)

cat(sprintf("App:          %s v%s\n", app_name, app_version))
cat(sprintf("R version:    %s\n", r_version))
cat(sprintf("App dir:      %s\n", app_dir))
cat(sprintf("Output dir:   %s\n", output_dir))
cat(sprintf("Packages:     %d\n\n", length(required_pkgs)))

# ==============================================================================
# STEP 1: Download R Portable for Windows
# ==============================================================================
cat("--- Step 1: Downloading R for Windows ---\n")

r_installer_filename <- sprintf("R-%s-win.exe", r_version)
r_installer_path <- file.path(build_dir, r_installer_filename)

# CRAN moves older R versions to the old/ archive
r_installer_urls <- c(
  sprintf("https://cloud.r-project.org/bin/windows/base/%s", r_installer_filename),
  sprintf("https://cloud.r-project.org/bin/windows/base/old/%s/%s", r_version, r_installer_filename)
)

if (!file.exists(r_installer_path)) {
  cat(sprintf("Downloading R %s installer...\n", r_version))
  downloaded <- FALSE
  for (url in r_installer_urls) {
    cat(sprintf("Trying: %s\n", url))
    result <- tryCatch({
      download.file(url, r_installer_path, mode = "wb", quiet = FALSE)
      TRUE
    }, error = function(e) FALSE)
    if (result && file.exists(r_installer_path)) {
      downloaded <- TRUE
      break
    }
  }
  if (!downloaded) stop("Could not download R installer. Check your internet connection.")
  cat("Download complete.\n\n")
} else {
  cat(sprintf("R installer already cached: %s\n\n", basename(r_installer_path)))
}

# ==============================================================================
# STEP 2: Extract R to a portable directory (silent install to local folder)
# ==============================================================================
cat("--- Step 2: Installing R Portable ---\n")

if (dir.exists(r_portable_dir)) {
  cat("Removing old R Portable directory...\n")
  unlink(r_portable_dir, recursive = TRUE)
}

# Create output structure
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Use Inno Setup's silent extraction to install R into our folder
# /VERYSILENT = no UI, /DIR= = install location, /COMPONENTS= minimal
cat("Extracting R (this takes 1-2 minutes)...\n")
install_cmd <- sprintf(
  '"%s" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /DIR="%s" /COMPONENTS="main,x64" /NOICONS',
  normalizePath(r_installer_path, winslash = "\\"),
  normalizePath(r_portable_dir, winslash = "\\", mustWork = FALSE)
)

result <- system(install_cmd, wait = TRUE, invisible = TRUE)
if (result != 0) {
  # Fallback: try without /COMPONENTS flag
  cat("Retrying extraction without component selection...\n")
  install_cmd <- sprintf(
    '"%s" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /DIR="%s" /NOICONS',
    normalizePath(r_installer_path, winslash = "\\"),
    normalizePath(r_portable_dir, winslash = "\\", mustWork = FALSE)
  )
  result <- system(install_cmd, wait = TRUE, invisible = TRUE)
}

# Verify R was installed
rscript_path <- file.path(r_portable_dir, "bin", "Rscript.exe")
if (!file.exists(rscript_path)) {
  # Check x64 subfolder
  rscript_path <- file.path(r_portable_dir, "bin", "x64", "Rscript.exe")
}
if (!file.exists(rscript_path)) {
  stop("R installation failed. Rscript.exe not found in: ", r_portable_dir,
       "\nCheck if the R installer download was corrupted.")
}
cat(sprintf("R Portable installed: %s\n\n", rscript_path))

# ==============================================================================
# STEP 3: Install packages into the portable R library
# ==============================================================================
cat("--- Step 3: Installing packages into R Portable ---\n")

# Create a library directory inside portable R
pkg_lib <- file.path(r_portable_dir, "library")
dir.create(pkg_lib, showWarnings = FALSE, recursive = TRUE)

# Build a one-line R command to install all packages (force install to ensure completeness)
pkgs_str <- paste0('"', required_pkgs, '"', collapse = ", ")
install_script <- sprintf(
  'install.packages(c(%s), lib="%s", repos="https://cloud.r-project.org", type="binary", dependencies=TRUE); cat("Package install complete.\\n")',
  pkgs_str,
  gsub("\\\\", "/", pkg_lib)
)

cat(sprintf("Installing %d packages (binary)...\n", length(required_pkgs)))
cat("This may take 5-10 minutes on first run.\n")

pkg_result <- system2(
  normalizePath(rscript_path, winslash = "\\"),
  args = c("-e", shQuote(install_script)),
  stdout = TRUE, stderr = TRUE
)
cat(paste(tail(pkg_result, 5), collapse = "\n"), "\n\n")

# ==============================================================================
# STEP 4: Copy app files into the output directory
# ==============================================================================
cat("--- Step 4: Copying app files ---\n")

app_dest <- file.path(output_dir, "app")
dir.create(app_dest, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(app_dest, "R"), showWarnings = FALSE)
dir.create(file.path(app_dest, "www"), showWarnings = FALSE)

# Copy main files
files_to_copy <- c("app.R", "launch_app.R", "report_template.Rmd")
for (f in files_to_copy) {
  src <- file.path(app_dir, f)
  if (file.exists(src)) {
    file.copy(src, file.path(app_dest, f), overwrite = TRUE)
    cat(sprintf("  Copied: %s\n", f))
  }
}

# Copy R/ subdirectory
r_files <- list.files(file.path(app_dir, "R"), full.names = TRUE)
for (f in r_files) {
  file.copy(f, file.path(app_dest, "R", basename(f)), overwrite = TRUE)
  cat(sprintf("  Copied: R/%s\n", basename(f)))
}

# Copy www/ subdirectory
www_files <- list.files(file.path(app_dir, "www"), full.names = TRUE, recursive = TRUE)
for (f in www_files) {
  rel <- sub(paste0(normalizePath(file.path(app_dir, "www"), winslash = "/"), "/"), "", normalizePath(f, winslash = "/"))
  dest_file <- file.path(app_dest, "www", rel)
  dir.create(dirname(dest_file), showWarnings = FALSE, recursive = TRUE)
  file.copy(f, dest_file, overwrite = TRUE)
  cat(sprintf("  Copied: www/%s\n", rel))
}

# Copy bundled ExifTool (must be at shiny_app/exiftool/)
exiftool_dir <- file.path(app_dir, "exiftool")
if (!dir.exists(exiftool_dir)) {
  stop("ExifTool not found at: ", exiftool_dir,
       "\nPlace your ExifTool + Perl folder at shiny_app/exiftool/")
}
{
  exiftool_dest <- file.path(output_dir, "exiftool")
  if (!dir.exists(exiftool_dest)) {
    cat("  Copying ExifTool...\n")
    dir.create(exiftool_dest, showWarnings = FALSE, recursive = TRUE)
    file.copy(list.files(exiftool_dir, full.names = TRUE, recursive = TRUE),
              exiftool_dest, recursive = TRUE, overwrite = TRUE)
    # Also copy subdirectories
    exiftool_subdirs <- list.dirs(exiftool_dir, full.names = TRUE, recursive = TRUE)
    for (d in exiftool_subdirs) {
      rel_d <- sub(paste0(normalizePath(exiftool_dir, winslash = "/"), "/?"), "", normalizePath(d, winslash = "/"))
      if (nchar(rel_d) > 0) {
        dest_d <- file.path(exiftool_dest, rel_d)
        dir.create(dest_d, showWarnings = FALSE, recursive = TRUE)
        sub_files <- list.files(d, full.names = TRUE, recursive = FALSE)
        sub_files <- sub_files[!file.info(sub_files)$isdir]
        if (length(sub_files) > 0) {
          file.copy(sub_files, dest_d, overwrite = TRUE)
        }
      }
    }
    cat("  ExifTool bundled.\n")
  }
}

# Bundle Pandoc (required for PDF reports)
pandoc_info <- rmarkdown::find_pandoc()
pandoc_src <- NULL
if (!is.null(pandoc_info$dir) && dir.exists(pandoc_info$dir)) {
  pandoc_src <- pandoc_info$dir
} else {
  # Search common locations (RStudio bundles Pandoc but find_pandoc() may miss it)
  pandoc_search <- c(
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "C:/Program Files/RStudio/bin/quarto/bin/tools",
    file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc"),
    "C:/Program Files/Pandoc"
  )
  for (p in pandoc_search) {
    if (file.exists(file.path(p, "pandoc.exe"))) { pandoc_src <- p; break }
  }
}
if (!is.null(pandoc_src)) {
  pandoc_dest <- file.path(output_dir, "pandoc")
  dir.create(pandoc_dest, showWarnings = FALSE)
  pandoc_files <- list.files(pandoc_src, full.names = TRUE)
  file.copy(pandoc_files, pandoc_dest, overwrite = TRUE)
  cat(sprintf("  Pandoc bundled from: %s\n", pandoc_src))
} else {
  warning("Pandoc not found on build machine. PDF reports may not work.")
}

# Bundle TinyTeX (required for offline PDF reports)
tinytex_root <- tryCatch(tinytex::tinytex_root(), error = function(e) "")
if (nchar(tinytex_root) > 0 && dir.exists(tinytex_root)) {
  tinytex_dest <- file.path(r_portable_dir, "TinyTeX")
  cat(sprintf("  Copying TinyTeX from: %s\n", tinytex_root))
  cat("  This may take a few minutes...\n")
  dir.create(dirname(tinytex_dest), showWarnings = FALSE, recursive = TRUE)
  file.copy(tinytex_root, dirname(tinytex_dest), recursive = TRUE)

  # Ensure required LaTeX packages are present
  tlmgr <- file.path(tinytex_dest, "bin", "windows", "tlmgr.exe")
  if (file.exists(tlmgr)) {
    cat("  Checking LaTeX packages...\n")
    system2(tlmgr, args = c("install", "tabu", "booktabs", "colortbl",
                             "array", "xcolor", "environ", "trimspaces"),
            stdout = TRUE, stderr = TRUE)
  }
  cat("  TinyTeX bundled.\n")
} else {
  warning("TinyTeX not found on build machine. PDF reports won't work offline.\n",
          "  Install with: tinytex::install_tinytex()")
}

cat("\n")

# ==============================================================================
# STEP 5: Create launcher scripts
# ==============================================================================
cat("--- Step 5: Creating launcher scripts ---\n")

# --- Main batch launcher ---
bat_content <- sprintf('@echo off
title %s v%s
cd /d "%%~dp0app"
set R_HOME=%%~dp0R-Portable
set R_LIBS_USER=%%~dp0R-Portable\\library
set RSTUDIO_PANDOC=%%~dp0pandoc
set TINYTEX_DIR=%%~dp0R-Portable\\TinyTeX
"%%~dp0R-Portable\\bin\\Rscript.exe" launch_app.R
pause
', app_name, app_version)

writeLines(bat_content, file.path(output_dir, paste0(app_name, ".bat")))
cat(sprintf("  Created: %s.bat\n", app_name))

# --- Silent VBS launcher (no console window) ---
vbs_content <- 'Set WshShell = CreateObject("WScript.Shell")
Set WshEnv = WshShell.Environment("Process")
Dim baseDir
baseDir = Left(WScript.ScriptFullName, InStrRev(WScript.ScriptFullName, "\\") - 1)
WshShell.CurrentDirectory = baseDir & "\\app"
WshEnv("RSTUDIO_PANDOC") = baseDir & "\\pandoc"
WshEnv("TINYTEX_DIR") = baseDir & "\\R-Portable\\TinyTeX"
WshShell.Run """" & baseDir & "\\R-Portable\\bin\\Rscript.exe"" launch_app.R", 0, False
'

writeLines(vbs_content, file.path(output_dir, paste0(app_name, ".vbs")))
cat(sprintf("  Created: %s.vbs (silent launcher)\n", app_name))

# --- Desktop shortcut creator ---
shortcut_ps <- sprintf('$WshShell = New-Object -ComObject WScript.Shell
$Shortcut = $WshShell.CreateShortcut("$env:USERPROFILE\\Desktop\\%s.lnk")
$Shortcut.TargetPath = "%s"
$Shortcut.WorkingDirectory = "%s"
$Shortcut.IconLocation = "%s,0"
$Shortcut.Description = "LandScan Drone QA/QC Analysis Tool"
$Shortcut.Save()

$StartMenu = "$env:APPDATA\\Microsoft\\Windows\\Start Menu\\Programs"
$Shortcut2 = $WshShell.CreateShortcut("$StartMenu\\%s.lnk")
$Shortcut2.TargetPath = "%s"
$Shortcut2.WorkingDirectory = "%s"
$Shortcut2.IconLocation = "%s,0"
$Shortcut2.Description = "LandScan Drone QA/QC Analysis Tool"
$Shortcut2.Save()

Write-Host "Shortcuts created on Desktop and Start Menu."
Write-Host "You can now pin the Start Menu entry to your Taskbar."
',
  app_name,
  gsub("/", "\\\\", file.path(output_dir, paste0(app_name, ".vbs"))),
  gsub("/", "\\\\", output_dir),
  gsub("/", "\\\\", file.path(r_portable_dir, "bin", "R.exe")),
  app_name,
  gsub("/", "\\\\", file.path(output_dir, paste0(app_name, ".vbs"))),
  gsub("/", "\\\\", output_dir),
  gsub("/", "\\\\", file.path(r_portable_dir, "bin", "R.exe"))
)

writeLines(shortcut_ps, file.path(output_dir, "create_shortcuts.ps1"))
cat("  Created: create_shortcuts.ps1\n")

# --- Also create a simple install.bat that runs the PowerShell shortcut creator ---
install_bat <- sprintf('@echo off
echo Creating shortcuts for %s...
powershell -ExecutionPolicy Bypass -File "%%~dp0create_shortcuts.ps1"
echo.
echo Done! You can now:
echo   1. Double-click the Desktop shortcut
echo   2. Find "%s" in the Start Menu
echo   3. Right-click the Start Menu entry and "Pin to Taskbar"
echo.
pause
', app_name, app_name)

writeLines(install_bat, file.path(output_dir, "install_shortcuts.bat"))
cat("  Created: install_shortcuts.bat\n")

# ==============================================================================
# STEP 6: Update config.R for portable paths
# ==============================================================================
cat("\n--- Step 6: Updating config for portable mode ---\n")

config_path <- file.path(app_dest, "R", "config.R")
if (file.exists(config_path)) {
  config_text <- readLines(config_path)
  # Update perl path to be relative to the portable distribution
  # Replace the entire perl_path line (handles file.path() expressions with internal commas)
  config_text <- gsub(
    'perl_path\\s*=\\s*.+?(,\\s*$)',
    'perl_path = file.path(getwd(), "..", "exiftool", "exiftool_files", "perl.exe"),',
    config_text,
    perl = TRUE
  )
  writeLines(config_text, config_path)
  cat("  Updated config.R with portable paths.\n")
}

# ==============================================================================
# STEP 7: Create ZIP for distribution
# ==============================================================================
cat("\n--- Step 7: Creating distributable ZIP ---\n")

zip_path <- file.path(build_dir, "output", paste0(app_name, ".zip"))
if (file.exists(zip_path)) file.remove(zip_path)

old_wd <- setwd(file.path(build_dir, "output"))
zip_result <- tryCatch({
  utils::zip(zip_path, files = app_name, extras = "-r")
  TRUE
}, error = function(e) {
  cat(sprintf("  ZIP creation failed: %s\n", e$message))
  cat("  You can manually ZIP the output folder instead.\n")
  FALSE
})
setwd(old_wd)

if (zip_result && file.exists(zip_path)) {
  zip_size_mb <- round(file.size(zip_path) / 1024 / 1024, 0)
  cat(sprintf("  Created: %s (%d MB)\n", basename(zip_path), zip_size_mb))
}

# ==============================================================================
# DONE
# ==============================================================================
cat("\n============================================================\n")
cat("  BUILD COMPLETE\n")
cat("============================================================\n\n")
cat(sprintf("Output folder: %s\n", output_dir))
if (zip_result && file.exists(zip_path)) {
  cat(sprintf("ZIP file:      %s\n", zip_path))
}
cat("\nTo deploy to a field technician:\n")
cat("  1. Copy the ZIP (or the DroneQAQC folder) to their machine\n")
cat("  2. They run install_shortcuts.bat once (creates Desktop + Start Menu shortcuts)\n")
cat("  3. Double-click the shortcut to launch the app\n")
cat("  4. Right-click Start Menu entry -> 'Pin to Taskbar' for quick access\n")
cat("\nNo R installation required on the target machine!\n")

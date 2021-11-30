# Set the name of your (!) new package
package <- "kwb.fcr"

# Set the path to your (!) local folder to which GitHub repositories are cloned
repo_dir <- "~/R/git"

# Set the path to the package directory
pkg_dir <- file.path(repo_dir, package)

# Create directory for R package
kwb.pkgbuild::create_pkg_dir(pkg_dir)
#> /var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T//RtmpE9Q727/kwb.newpackage is a valid 'root_dir' for pkg 'kwb.newpackage'
#> [1] "/var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T//RtmpE9Q727/kwb.newpackage"
# Create a default package structure
withr::with_dir(pkg_dir, {kwb.pkgbuild::use_pkg_skeleton(package)})
#> ✔ Setting active project to '/private/var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T/RtmpE9Q727/kwb.newpackage'
#> ✔ Writing 'kwb.newpackage.Rproj'
#> ✔ Adding '.Rproj.user' to '.gitignore'
#> NULL

author <- list(name = "Malte Zamzow",
               orcid = "0000-0002-8748-038X")

# author <- list(
#   name = "Michael Rustler",
#   orcid = "0000-0003-0647-7726",
#   url = "http://mrustl.de"
# )

description <- list(
  name = package,
  title = "Fertilizer chemical risk assessment",
  desc  = "This risk assessment is based on the TGD on risk assessment by the
  European Comission. Every variable can be entered as probability distribution
  to include uncertainties."
)

# setwd(pkg_dir)

kwb.pkgbuild::use_pkg(
  author,
  description,
  version = "0.0.0.9000",
  stage = "experimental"
)

usethis::use_vignette("documention")

# ---------------------------------------------------------------------
# in case package dependencies need to be added
pkg_dependencies <- c() # insert package names
sapply(pkg_dependencies, usethis::use_package)


### R functions
if(FALSE) {
  ## add your dependencies (-> updates: DESCRIPTION)
  pkg_dependencies <- c("dplyr")

  sapply(pkg_dependencies, usethis::use_package)

  desc::desc_add_remotes("kwb-r/kwb.utils",normalize = TRUE)

  usethis::use_pipe()
}

kwb.pkgbuild::use_ghactions()




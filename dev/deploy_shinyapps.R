## ---------------------------------------------------------------
##  dev/deploy_shinyapps.R
##
##  shinyapps.io installs dependencies from CRAN. rcef is not on CRAN, so
##  the deployment has to be told where to get it. rsconnect reads the
##  RemoteType/RemoteUrl fields that remotes::install_github() writes into
##  the installed DESCRIPTION -- so INSTALL FROM GITHUB FIRST, locally,
##  and deploy from that installed copy. Installing with
##  devtools::install() or R CMD INSTALL leaves no remote and the deploy
##  fails with "Unable to determine package source".
## ---------------------------------------------------------------
REPO <- "DVillagra26/rcef"

remotes::install_github(REPO, upgrade = "never")
stopifnot(!is.na(utils::packageDescription("rcef")$RemoteType))

rsconnect::deployApp(
  appDir      = system.file("app", package = "rcef"),
  appName     = "rcef-workbench",
  appFileManifest = NULL,
  forceUpdate = TRUE)

#emoji
devtools::install_github("hadley/emo")

#build vignette
devtools::build_vignettes()


install.packages("pkgdown")
usethis::use_pkgdown()
library("pkgdown")

build_home()
build_articles()
pkgdown::build_site()

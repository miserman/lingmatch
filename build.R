library(lingmatch)
library(splot)

# unload to rebuild
dyn.unload(system.file("libs/x64/lingmatch.dll", package = "lingmatch"))

# builds documentation and site
styler::style_pkg(filetype = c("R", "Rmd"))
spelling::spell_check_package()
devtools::document()
pkgdown::build_site(lazy = TRUE)
covr::report(covr::package_coverage(quiet = FALSE), "docs/coverage.html")

# makes sysdata.rda
lsm_profiles <- as.data.frame(matrix(c(
  9.95, 5.26, 6.51, 8.53, 5.27, 12.93, 5.90, 1.66, 2.02, 10.66, 5.53, 6, 8.75, 5.88, 12.60, 6.43,
  1.81, 2.27, 12.74, 5.28, 5.70, 9.25, 6.02, 14.27, 7.46, 1.69, 2.35, 10.35, 4.79, 8.35, 7.77,
  4.17, 14.27, 6.28, 1.68, 1.8, 13.37, 7.53, 4.34, 12.03, 7.67, 10.29, 6.21, 2.24, 1.93, 3.56, 3.84,
  9.08, 5.11, 2.76, 14.27, 4.85, .62, 1.94, 9.02, 4.60, 5.58, 8.27, 5.13, 11.88, 4.19, 1.74, 1.85
), 7, 9, byrow = T, dimnames = list(c(
  "overall", "blogs", "expressive", "novels", "natural", "nytimes", "twitter"
), c(
  "ppron", "ipron", "article", "auxverb", "adverb", "prep", "conj", "negate", "quant"
))))
lss_info <- read.csv("https://osf.io/download/9yzca", row.names = 1)
colnames(lss_info) <- c(
  "terms", "corpus", "model", "dimensions", "model_info", "original_max", "osf_dat", "osf_terms"
)
dict_info <- read.csv("https://osf.io/download/kjqb8", row.names = 1)
save(lsm_profiles, lss_info, dict_info, file = "r/sysdata.rda", compress = "bzip2")

# runs checks
devtools::check_win_devel()
devtools::check_rhub(interactive = FALSE)

# releases
devtools::release()

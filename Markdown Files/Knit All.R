# Knit every Rmd in Markdown Files/ to docs/ -------------------------------
# Run from anywhere; output goes to <repo>/docs. The tutorial's knitr cache
# (MNAR-Tutorial_cache/) is reused, so only edited chunks rerun.

fdir::set()                                   # cd to this script's folder
library(rmarkdown)

rmds <- c('MNAR-Tutorial.Rmd',
          'IRT Equivalence.Rmd',
          'Blimp Installation Guide.Rmd',
          'Blimp Scripting Quick Start.Rmd')

for (f in rmds) {
  if (!file.exists(f)) { message('skipping (not found): ', f); next }
  message('\n==== knitting ', f, ' ====')
  render(f, output_dir = '../docs', envir = new.env(), quiet = TRUE)
}

message('\nDone. HTML files in docs/:')
print(list.files('../docs', pattern = '\\.html$'))

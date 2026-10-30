# Knit every Rmd in Markdown Files/ and copy the HTML to docs/ -------------
# Each file is rendered IN PLACE (next to its Rmd), exactly as the RStudio
# Knit button does, so the tutorial's knitr cache (MNAR-Tutorial_cache/) is
# reused and only edited chunks rerun. Do NOT add output_dir to render():
# it changes the figure path, which is part of the cache hash, and every
# model would rerun. The HTML is copied to docs/ afterward.

fdir::set()                                   # cd to this script's folder
library(rmarkdown)

rmds <- c('MNAR-Tutorial.Rmd',
          # 'IRT Equivalence.Rmd',   # not ready yet; uncomment when the IRT models run
          'Blimp Installation Guide.Rmd',
          'Blimp Scripting Quick Start.Rmd')

for (f in rmds) {
  if (!file.exists(f)) { message('skipping (not found): ', f); next }
  message('\n==== knitting ', f, ' ====')
  html <- render(f, envir = new.env(), quiet = TRUE)
  file.copy(html, file.path('../docs', basename(html)), overwrite = TRUE)
  file.remove(html)                           # keep Markdown Files/ clean
}

message('\nDone. HTML files in docs/:')
print(list.files('../docs', pattern = '\\.html$'))

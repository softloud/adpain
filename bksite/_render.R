# render book in browser

# system("rm bksite/_main.md")

browseURL(
  xfun::in_dir("bksite", bookdown::render_book("index.Rmd"))
)


browseURL(
  xfun::in_dir("bksite", bookdown::preview_chapter("98-testing.Rmd"))
)

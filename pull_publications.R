library(scholar)
library(tidyverse)
library(glue)

# escape some special chars, german umlauts, ...
char2html <- function(x){
  dictionary <- data.frame(
    symbol = c("ä","ö","ü","Ä", "Ö", "Ü", "ß"),
    html = c("&auml;","&ouml;", "&uuml;","&Auml;",
             "&Ouml;", "&Uuml;","&szlig;"))
  for(i in 1:dim(dictionary)[1]){
    x <- gsub(dictionary$symbol[i],dictionary$html[i],x)
  }
  x
}

# my google scholar user id from my profile url
# https://scholar.google.com/citations?user=b8bWNkUAAAAJ&hl=en
thackl <- "73RvTUoAAAAJ"
html_1 <- get_publications(thackl)
html_2 <- html_1 %>%
  as_tibble %>% arrange(desc(year)) %>%
  mutate(
    #    author=str_replace_all(author, " (\\S) ", "\\1 "),
    author=str_replace_all(author, "([A-Z]) ([A-Z]) ", "\\1\\2 "),
    author=str_replace_all(author, ", \\.\\.\\.", " et al."),
    author=str_replace_all(author, "KL Hoffman", "<b>KL Hoffman</b>"),
    author=str_replace_all(author, "K Hoffman", "<b>K Hoffman</b>") # make my name fat
  ) %>% split(.$year) %>%
  map(function(x){
    x <- x %>%
      glue_data('<tr><td width="100%">{author} ({year}) <a href="https://scholar.google.com/scholar?oi=bibs&cluster={cid}&btnI=1&hl=en">{title}</a>, {journal}, {number}</td></tr>') %>%
      str_replace_all("(, )+</p>", "</p>") %>%
      char2html()
    x <- c('<table class="publication-table" border="10px solid blue" cellspacing="0" cellpadding="6" rules="", frame=""><tbody>', x, '</tbody></table>')
    return(x);
  }) %>% rev

html_3 <- map2(names(html_2) %>% paste0("<h3>", ., "</h3>"), html_2, c) %>% unlist


html_4 <- c(
  paste0('---\ntitle: "Publications"\neditor: source\n---'), 
  paste0('<p style="text-align: left; margin-top: 20px;"><small>Last updated <i>',
         format(Sys.Date(), format="%B %d, %Y"),
         '&ndash; Pulled automatically from my <a href="https://scholar.google.com/citations?hl=en&user=b8bWNkUAAAAJ">Google Scholar profile</a>. See <a href="https://thackl.github.io/automatically-update-publications-with-R-scholar">this post</a> for details.</small></p>'),
  html_3)
writeLines(html_4, "publications.qmd")

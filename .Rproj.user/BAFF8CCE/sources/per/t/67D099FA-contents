setwd("/home/agricolamz/work/other_projects/language_of_science_website_2")
library(tidyverse)
library(glue)
library(rvest)
library(readxl)

# cleaning ----------------------------------------------------------------

files <- list.files(".", pattern = "qmd") 

to_remove <- files[!files %in% c("index.qmd", 
                                 "word_lists.qmd",
                                 "tasks.qmd")]

file.remove(c(to_remove, list.files(".", pattern = ".html")))


# extract paradigms -------------------------------------------------------

# read_xlsx("data/word_profiles.xlsx") |> 
#   filter(!is.na(lemma)) |> 
#   distinct(lemma) |> 
#   pull(lemma) |> 
#   walk(function(i){
#     read_html(glue("https://ru.wiktionary.org/wiki/{i}")) |> 
#       html_element(".morfotable") |> 
#       write_lines(glue("data/{i}.html"))    
#     
#     read_lines(glue("data/{i}.html")) |> 
#       str_remove("float:right; ") |> 
#       str_remove('width="210" ') |> 
#       str_remove('<a href.*?>') |> 
#       str_remove('</a>') |> 
#       append("<details>", after = 0) |>
#       append("<summary>парадигма</summary>", after = 1) |>
#       append("</details>") |> 
#       write_lines(glue("data/{i}.html"))
#   })

# generate word profiles --------------------------------------------------

readxl::read_xlsx("data/word_profiles.xlsx") |> 
  filter(!is.na(lemma)) |> 
  distinct(lemma) |> 
  pull(lemma) |> 
  walk(function(i){
    options(ymlthis.rmd_body = glue::glue("

```{{r, child='data/{i}.html'}}
```

```{{r}}
#| echo: false

library(tidyverse)
readxl::read_xlsx('data/word_profiles.xlsx') |> 
  filter(lemma == '{i}') |> 
  select(phrase_for_site, example) |> 
  rename(выражение = phrase_for_site, 
         пример = example) ->
  result

if(sum(is.na(result$пример)) == nrow(result)){{
  result |> 
    select(выражение) ->
    result
}}

DT::datatable(result, 
              filter = 'top', 
              escape = FALSE, 
              rownames = FALSE,
              options = list(pageLength = 15, dom = 'tp'))
```

"))
    ymlthis::yml_empty() |> 
      ymlthis::yml_title(i) |> 
      ymlthis::use_rmarkdown(path = str_c(i, ".qmd"), 
                             open_doc = FALSE, 
                             quiet = TRUE,
                             include_body = FALSE,
                             body = NULL)
})

# render site -------------------------------------------------------------

quarto::

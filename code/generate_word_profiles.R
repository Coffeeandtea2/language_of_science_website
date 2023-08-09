setwd("/home/agricolamz/work/other_projects/language_of_science_website")
library(tidyverse)
library(glue)
library(rvest)
library(readxl)

# extract paradigms -------------------------------------------------------

read_xlsx("data/word_profiles.xlsx") |>
  filter(!is.na(lemma_for_site)) |>  
  distinct(lemma_for_site) |> 
  mutate(lemma_for_site = str_split(lemma_for_site, " - ")) |> 
  unnest_longer(lemma_for_site) |> 
  mutate(lemma_for_site = str_remove_all(lemma_for_site, "\\(.*?\\)")) |> 
  pull(lemma_for_site) |> 
  walk(function(i){
    read_html(glue("https://ru.wiktionary.org/wiki/{i}")) |>
      html_element(".morfotable") |>
      write_lines(glue("data/{i}.html"))

    read_lines(glue("data/{i}.html")) |>
      str_remove("float:right; ") |>
      str_remove_all('<a href.*?>') |>
      str_remove_all('</a>') |>
      append("<details>", after = 0) |>
      append("<summary>парадигма</summary>", after = 1) |>
      append("</details>") |>
      write_lines(glue("data/{i}.html"))
  })

# merge 2 paradimes of verbs with different aspects

read_xlsx("data/word_profiles.xlsx") |>
  filter(str_detect(lemma_for_site, "-")) |>  
  distinct(lemma, lemma_for_site) |> 
  mutate(lemma_for_site = str_split(lemma_for_site, " - ")) |> 
  unnest_longer(lemma_for_site) |> 
  group_by(lemma) |> 
  reframe(n = 1:n(),
          n = str_c("variant_", n),
          lemma_for_site = lemma_for_site) |> 
  mutate(lemma_for_site = str_remove_all(lemma_for_site, "[\\(\\)]")) |> 
  pivot_wider(names_from = n, values_from = lemma_for_site) ->
  merge_paradigms

walk(merge_paradigms$lemma, function(i){
  v1 <- merge_paradigms[merge_paradigms$lemma == i,]$variant_1
  v2 <- merge_paradigms[merge_paradigms$lemma == i,]$variant_2
  
  read_lines(str_c("data/", v2, ".html")) |> 
    str_remove_all("<details>") |> 
    str_remove_all("<summary>парадигма</summary>") ->
    v2_modified
  
  read_lines(str_c("data/", v1, ".html")) |> 
    str_remove_all("</details>") |> 
    append(v2_modified) ->
    result
  
  write_lines(result, str_c("data/", i, ".html"))
  
})


# cleaning ----------------------------------------------------------------

files <- list.files(".", pattern = "qmd") 

to_remove <- files[!files %in% c("index.qmd", 
                                 "word_lists.qmd",
                                 "tasks.qmd")]

file.remove(c(to_remove, list.files(".", pattern = ".html")))

# generate word profiles --------------------------------------------------

w_profiles <- readxl::read_xlsx("data/word_profiles.xlsx") 

w_profiles |> 
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
  mutate(example = ifelse(is.na(example), '', example)) |> 
  select(phrase_for_site, example) |> 
  group_by(phrase_for_site) |> 
  summarize(example = str_c(example, collapse = '<br>')) |> 
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
      ymlthis::yml_title(w_profiles |> 
                           filter(lemma == i) |> 
                           distinct(lemma_for_site) |> 
                           pull(lemma_for_site)) |> 
      ymlthis::use_rmarkdown(path = str_c(i, ".qmd"), 
                             open_doc = FALSE, 
                             quiet = TRUE,
                             include_body = FALSE,
                             body = NULL)
})

# render site -------------------------------------------------------------

quarto::quarto_render()


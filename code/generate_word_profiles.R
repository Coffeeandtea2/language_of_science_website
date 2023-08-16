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
  mutate(lemma = str_extract(lemma_for_site, "^.*?(?=( -))"),
         lemma = ifelse(is.na(lemma), lemma_for_site, lemma),
         lemma = str_remove_all(lemma, "\\(.*?\\)")) |> 
  distinct(lemma, lemma_for_site) |> 
  mutate(lemma_for_site = str_split(lemma_for_site, " - ")) |> 
  unnest_longer(lemma_for_site) |> 
  group_by(lemma) |> 
  reframe(n = 1:n(),
          n = str_c("variant_", n),
          lemma_for_site = lemma_for_site) |> 
  mutate(lemma_for_site = str_remove_all(lemma_for_site, "\\(.*?\\)")) |> 
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


# generate tasks ----------------------------------------------------------

readxl::read_xlsx("data/word_profiles.xlsx") |> 
  filter(!is.na(lemma_for_site))  |> 
  mutate(lemma = str_extract(lemma_for_site, "^.*?(?=( -))"),
         lemma = ifelse(is.na(lemma), lemma_for_site, lemma),
         lemma = str_remove_all(lemma, "\\(.*?\\)")) |> 
  pull(lemma) |> 
  unique() ->
  files

walk(files, function(i){
  write_lines("", file = str_c("tasks/", i, ".qmd"))
})

readxl::read_xlsx("data/word_profiles.xlsx") |> 
  filter(!is.na(lemma_for_site))  |> 
  left_join(readxl::read_xlsx("data/tasks.xlsx"), 
            by = join_by("lemma" == "stimulus"),
            relationship = "many-to-many") |> 
  mutate(lemma = str_extract(lemma_for_site, "^.*?(?=( -))"),
         lemma = ifelse(is.na(lemma), lemma_for_site, lemma),
         lemma = str_remove_all(lemma, "\\(.*?\\)")) |> 
  distinct(lemma, answer, task) |> 
  na.omit() ->
  generate_tasks

walk(unique(generate_tasks$lemma), function(i){
  generate_tasks |> 
    filter(lemma == i) |> 
    slice_sample(prop = 1) ->
    generate_tasks_by_lemma
  
  map2(generate_tasks_by_lemma$task, 
       generate_tasks_by_lemma$answer,
       function(task, answer) {
         
         glue("

(@) {task}

```{{r}}
#| results: asis
check_question(answer = '{answer}', 
               right = 'все верно!', 
               wrong = 'к сожалению нет, попробуйте еще раз...')
```

") }) |> 
    write_lines(str_c("tasks/", i, ".qmd"))
  
})


readxl::read_xlsx("data/tasks.xlsx") |> 
  filter(!is.na(answer)) ->
  tasks_dataset

library(udpipe)
ru <- udpipe_load_model("/home/agricolamz/work/databases/spoken_corpora/russian-syntagrus-ud-2.5-191206.udpipe")

tasks_dataset |> 
  distinct(stimulus) |> 
  pull(stimulus) |> 
  udpipe(ru) |> 
  select(sentence, upos) |> 
  rename(stimulus = sentence) |> 
  left_join(tasks_dataset) ->
  tasks_dataset

tasks_dataset |> 
  filter(upos %in% c("ADJ", "NOUN")) |> 
  slice_sample(prop = 1) ->
  generate_declension_tasks

map2(generate_declension_tasks$task, 
     generate_declension_tasks$answer,
     function(task, answer) {
       
       glue("

(@) {task}

```{{r}}
#| results: asis
checkdown::check_question(answer = '{answer}', 
                          right = 'все верно!', 
                          wrong = 'к сожалению нет, попробуйте еще раз...')
```

") }) |> 
  write_lines("declension.qmd")

tasks_dataset |> 
  filter(upos %in% c("VERB")) |> 
  slice_sample(prop = 1) ->
  generate_government_tasks

map2(generate_government_tasks$task, 
     generate_government_tasks$answer,
     function(task, answer) {
       
       glue("

(@) {task}

```{{r}}
#| results: asis
checkdown::check_question(answer = '{answer}', 
                          right = 'все верно!', 
                          wrong = 'к сожалению нет, попробуйте еще раз...')
```

") }) |> 
  write_lines("government.qmd")


# cleaning ----------------------------------------------------------------

files <- list.files(".", pattern = "qmd") 

to_remove <- files[!files %in% c("index.qmd", 
                                 "word_lists.qmd",
                                 "tasks.qmd",
                                 "about.qmd")]

file.remove(c(to_remove, list.files(".", pattern = ".html")))

rm(files, to_remove)

# generate word profiles --------------------------------------------------

readxl::read_xlsx("data/word_profiles.xlsx") |> 
  filter(!is.na(lemma_for_site)) |> 
  mutate(lemma = str_extract(lemma_for_site, "^.*?(?=( -))"),
         lemma = ifelse(is.na(lemma), lemma_for_site, lemma),
         lemma = str_remove_all(lemma, "\\(.*?\\)")) ->
  w_profiles 

w_profiles |> 
  distinct(lemma) |> 
  pull(lemma) |>
  walk(function(i){
    options(ymlthis.rmd_body = glue::glue("

[![](images/wiktionary.png){{height=7mm}}](https://ru.wiktionary.org/wiki/{i})
[![](images/ruscorpora.png){{height=7mm}}](https://ruscorpora.ru/word/main?req={i})


```{{r, child='data/{i}.html'}}
```
::: {{.panel-tabset}}

## примеры

```{{r}}
#| echo: false

library(tidyverse)
library(checkdown)
readxl::read_xlsx('data/word_profiles.xlsx') |> 
  mutate(lemma = str_extract(lemma_for_site, '^.*?(?=( -))'),
         lemma = ifelse(is.na(lemma), lemma_for_site, lemma),
         lemma = str_remove_all(lemma, '\\\\(.*?\\\\)')) |> 
  filter(lemma == '{i}') |> 
  mutate(example = ifelse(is.na(example), '', str_c('• ', example))) |> 
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

## упражнения

```{{r, child='tasks/{i}.qmd'}}
```

:::

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

quarto::quarto_render()

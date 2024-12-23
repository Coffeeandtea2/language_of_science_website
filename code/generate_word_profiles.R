setwd("/home/agricolamz/work/other_projects/language_of_science_website")
library(tidyverse)
library(glue)
library(rvest)
library(readxl)
library(udpipe)
ru <- udpipe_load_model("/home/agricolamz/work/databases/russian_ud_model/russian_model.udpipe")

# extract paradigms -------------------------------------------------------

list.files("data/", pattern = ".html") |> 
  str_remove(".html") ->
  already_present

read_xlsx("data/word_profiles.xlsx") |>
  filter(!is.na(lemma_for_site)) |>
  distinct(lemma_for_site) |>
  mutate(lemma_for_site = str_split(lemma_for_site, " - ")) |>
  unnest_longer(lemma_for_site) |>
  mutate(lemma_for_site = str_remove_all(lemma_for_site, "\\(.*?\\)")) |>
  filter(!(lemma_for_site %in% already_present)) |> 
  pull(lemma_for_site) |>
  sort() |> 
  walk(safely(function(i){
    read_html(glue("https://ru.wiktionary.org/wiki/{i}")) |>
      html_element(".morfotable") |>
      write_lines(glue("data/{i}.html"))
    
    read_lines(glue("data/{i}.html")) |>
      str_remove("float:right; ") |>
      str_remove_all('<a href.*?>') |>
      str_remove_all('</a>') |>
      append("<details>", after = 0) |>
      append("<summary>формы слова</summary>", after = 1) |>
      append("</details>") |>
      write_lines(glue("data/{i}.html"))
  }))


read_xlsx("data/word_profiles.xlsx") |>
  filter(!is.na(lemma_for_site)) |>
  distinct(lemma_for_site) |>
  mutate(lemma_for_site = str_split(lemma_for_site, " - ")) |>
  unnest_longer(lemma_for_site) |>
  mutate(lemma_for_site = str_remove_all(lemma_for_site, "\\(.*?\\)")) |>
  filter(!(lemma_for_site %in% already_present)) |> 
  pull(lemma_for_site) |>
  sort() |> 
  walk(function(i){
    write_lines("", glue("data/{i}.html"))
  })

# merge 2 paradimes of verbs with different aspects

# read_xlsx("data/word_profiles.xlsx") |>
#   filter(str_detect(lemma_for_site, "-")) |>
#   mutate(lemma = str_extract(lemma_for_site, "^.*?(?=( -))"),
#          lemma = ifelse(is.na(lemma), lemma_for_site, lemma),
#          lemma = str_remove_all(lemma, "\\(.*?\\)")) |>
#   distinct(lemma, lemma_for_site) |>
#   mutate(lemma_for_site = str_split(lemma_for_site, " - ")) |>
#   unnest_longer(lemma_for_site) |>
#   group_by(lemma) |>
#   reframe(n = 1:n(),
#           n = str_c("variant_", n),
#           lemma_for_site = lemma_for_site) |>
#   mutate(lemma_for_site = str_remove_all(lemma_for_site, "\\(.*?\\)")) |>
#   pivot_wider(names_from = n, values_from = lemma_for_site) ->
#   merge_paradigms
# 
# walk(merge_paradigms$lemma, function(i){
#   v1 <- merge_paradigms[merge_paradigms$lemma == i,]$variant_1
#   v2 <- merge_paradigms[merge_paradigms$lemma == i,]$variant_2
# 
#   read_lines(str_c("data/", v2, ".html")) |>
#     str_remove_all("<details>") |>
#     str_remove_all("<summary>парадигма</summary>") ->
#     v2_modified
# 
#   read_lines(str_c("data/", v1, ".html")) |>
#     str_remove_all("</details>") |>
#     append(v2_modified) ->
#     result
# 
#   write_lines(result, str_c("data/", i, ".html"))
# 
# })
# 
# 
# generate tasks ----------------------------------------------------------
# make tasks for words ----------------------------------------------------
readxl::read_xlsx("data/word_profiles.xlsx") |> 
  filter(!is.na(lemma_for_site))  |> 
  mutate(lemma = str_extract(lemma_for_site, "^.*?(?=( -))"),
         lemma = ifelse(is.na(lemma), lemma_for_site, lemma),
         lemma = str_remove_all(lemma, "\\(.*?\\)")) |> 
  pull(lemma) |> 
  unique() |> 
  sort() ->
  files

readxl::read_xlsx("data/tasks.xlsx") |> 
  filter(!is.na(answer)) ->
  tasks_dataset_full


walk(files, function(i){
  
# phrase

tasks_dataset_full |> 
  filter(stimulus == i) |> 
  filter(task_type == "Упорядочить слова") |> 
  slice_sample(n = 5) |> 
  pull(answer) |> 
  map(function(answer) {
    glue("

```{{r}}
checkdown::check_question(answer = stringr::str_split('{answer}', '; ') |> unlist(), 
                          type = 'in_order',
                          right = 'все верно!',
                          title = '#### Поставьте слова в правильном порядке:',
                          wrong = 'к сожалению нет, попробуйте еще раз...',
                          button_label = 'проверить')
checkdown::check_hint(hint_text = '{answer}',
           hint_title = '🔎 Нажмите, чтобы посмотреть ответ')
```

") }) -> 
  individual_phrases

# sentences  
  
tasks_dataset_full |> 
  filter(stimulus == i) |> 
  filter(task_type == "Упорядочить фрагменты предложений") |> 
  slice_sample(n = 5) |> 
  pull(answer) |> 
  map(function(answer) {
    glue("

```{{r}}
checkdown::check_question(answer = stringr::str_split('{answer}', '; ') |> unlist(), 
                          type = 'in_order',
                          alignment = 'vertical',
                          right = 'все верно!',
                          title = '#### Поставьте фрагменты предложений в правильном порядке:',
                          wrong = 'к сожалению нет, попробуйте еще раз...',
                          button_label = 'проверить')
checkdown::check_hint(hint_text = '{answer}',
           hint_title = '🔎 Нажмите, чтобы посмотреть ответ')
```

") }) ->
  individual_sentences

# government and declension

tasks_dataset_full |>
  filter(stimulus == i) |> 
  filter(!(task_type %in% c("Упорядочить слова",
                            "Упорядочить фрагменты предложений"))) |> 
  slice_sample(n = 5) ->
  generate_dec_gov_tasks

map2(generate_dec_gov_tasks$task, 
     generate_dec_gov_tasks$answer,
     function(task, answer) {
       
       glue("

```{{r}}
checkdown::check_question(answer = '{answer}', 
                          title = '#### {task}',
                          right = 'все верно!', 
                          wrong = 'к сожалению нет, попробуйте еще раз...',
                          button_label = 'проверить')
checkdown::check_hint(hint_text = '{answer}',
           hint_title = '🔎 Нажмите, чтобы посмотреть ответ')                          
```

") }) ->
  dec_gov_tasks

c(individual_phrases, individual_sentences, dec_gov_tasks) |> 
  unlist() |> 
  write_lines("", file = str_c("tasks/", i, ".qmd"))
})

# readxl::read_xlsx("data/word_profiles.xlsx") |> 
#   filter(!is.na(lemma_for_site)) |> 
#   left_join(readxl::read_xlsx("data/tasks.xlsx"), 
#             by = join_by("lemma" == "stimulus"),
#             relationship = "many-to-many") |> 
#   filter(!(task %in% c("Поставьте части предложения в правильном порядке:", 
#                         "Поставьте слова в правильном порядке:"))) |> 
#   mutate(lemma = str_extract(lemma_for_site, "^.*?(?=( -))"),
#          lemma = ifelse(is.na(lemma), lemma_for_site, lemma),
#          lemma = str_remove_all(lemma, "\\(.*?\\)")) |> 
#   distinct(lemma, answer, task) |> 
#   na.omit() |>
#   group_by(lemma) |> 
#   slice_sample(n = 5) ->
#   generate_tasks
# 
# walk(unique(generate_tasks$lemma), function(i){
#   generate_tasks |> 
#     filter(lemma == i) |> 
#     slice_sample(prop = 1) ->
#     generate_tasks_by_lemma
#   
#   map2(generate_tasks_by_lemma$task, 
#        generate_tasks_by_lemma$answer,
#        function(task, answer) {
#          
#          glue("
# ```{{r}}
# checkdown::check_question(answer = '{answer}', 
#                           title = '#### {task}',
#                           right = 'все верно!', 
#                           wrong = 'к сожалению нет, попробуйте еще раз...',
#                           button_label = 'проверить')
# checkdown::check_hint(hint_text = '{answer}',
#                       hint_title = '🔎 Нажмите, чтобы посмотреть ответ')
# ```
# 
# ") }) |> 
#     write_lines(str_c("tasks/", i, ".qmd"))
#   
# })

# make tasks for tasks section --------------------------------------------

readxl::read_xlsx("data/tasks.xlsx") |> 
  filter(!is.na(answer)) ->
  tasks_dataset_full

tasks_dataset_full |> 
  distinct(stimulus) |> 
  pull(stimulus) |>
  na.omit() |> 
  udpipe(ru) |> 
  select(sentence, upos) |> 
  rename(stimulus = sentence) |> 
  left_join(tasks_dataset_full) ->
  tasks_dataset

tasks_dataset |> 
  filter(upos %in% c("ADJ", "NOUN"),
         !(task_type %in% c("Упорядочить слова",
                            "Упорядочить фрагменты предложений"))) |> 
  slice_sample(prop = 1) |> 
  slice_sample(n = 700) ->
  generate_declension_tasks

map2(generate_declension_tasks$task, 
     generate_declension_tasks$answer,
     function(task, answer) {
       
       glue("

```{{r}}
checkdown::check_question(answer = '{answer}', 
                          title = '#### {task}',
                          right = 'все верно!', 
                          wrong = 'к сожалению нет, попробуйте еще раз...',
                          button_label = 'проверить')
checkdown::check_hint(hint_text = '{answer}',
           hint_title = '🔎 Нажмите, чтобы посмотреть ответ')                          
```

") }) |> 
  write_lines("declension.qmd")

tasks_dataset |> 
  filter(upos %in% c("VERB"),
         !(task_type %in% c("Упорядочить слова",
                            "Упорядочить фрагменты предложений"))) |> 
  slice_sample(prop = 1) |> 
  slice_sample(n = 700) ->
  generate_government_tasks

map2(generate_government_tasks$task, 
     generate_government_tasks$answer,
     function(task, answer) {
       
       glue("

```{{r}}
checkdown::check_question(answer = '{answer}', 
                          right = 'все верно!',
                          title = '#### {task}',
                          wrong = 'к сожалению нет, попробуйте еще раз...',
                          button_label = 'проверить')
checkdown::check_hint(hint_text = '{answer}',
           hint_title = '🔎 Нажмите, чтобы посмотреть ответ')
```

") }) |> 
  write_lines("government.qmd")


tasks_dataset_full |> 
  filter(task_type == "Графики") |> 
  mutate(options = str_c(options, " ; ", answer)) ->
  generate_pictures_tasks

"### Посмотрите на график и выберите правильное предложение\n\n" |> 
  write_lines("plots.qmd")

map(seq_along(generate_pictures_tasks$task), 
     function(i) {
       str_c("

(@) ",
             str_c("![](images/", generate_pictures_tasks$task[i]), ".png)",
"
```{r}
checkdown::check_question(answer = '",
              generate_pictures_tasks$answer[i],
"', 
                          options = ",
str_c("c('", 
      str_replace_all(generate_pictures_tasks$options[i], " ; ", "', '"),
      "')"),
",
                          right = 'все верно!', 
                          wrong = 'к сожалению нет, попробуйте еще раз...',
                          button_label = 'проверить',
                          type = 'radio',
                          random_answer_order = TRUE)
```

") }) |> 
  write_lines("plots.qmd", append = TRUE)

tasks_dataset_full |> 
  filter(task_type == "Упорядочить слова") |> 
  slice_sample(prop = 1) |> 
  pull(answer) |> 
  map(function(answer) {
    
       glue("

```{{r}}
checkdown::check_question(answer = stringr::str_split('{answer}', '; ') |> unlist(), 
                          type = 'in_order',
                          right = 'все верно!',
                          title = '#### Поставьте слова в правильном порядке:',
                          wrong = 'к сожалению нет, попробуйте еще раз...',
                          button_label = 'проверить')
checkdown::check_hint(hint_text = '{answer}',
           hint_title = '🔎 Нажмите, чтобы посмотреть ответ')
```

") }) |> 
  write_lines("word_in_order.qmd")

tasks_dataset_full |> 
  filter(task_type == "Упорядочить фрагменты предложений") |> 
  slice_sample(n = 300) |> 
  pull(answer) |> 
  map(function(answer) {
    
    glue("

```{{r}}
checkdown::check_question(answer = stringr::str_split('{answer}', '; ') |> unlist(), 
                          type = 'in_order',
                          alignment = 'vertical',
                          right = 'все верно!',
                          title = '#### Поставьте фрагменты предложений в правильном порядке:',
                          wrong = 'к сожалению нет, попробуйте еще раз...',
                          button_label = 'проверить')
checkdown::check_hint(hint_text = '{answer}',
           hint_title = '🔎 Нажмите, чтобы посмотреть ответ')
```

") }) |> 
  write_lines("sentence_in_order.qmd")

rm(generate_declension_tasks, generate_government_tasks, generate_tasks, 
   tasks_dataset, ru, generate_word_in_order_tasks, generate_pictures_tasks)

# cleaning ----------------------------------------------------------------

files <- list.files(".", pattern = "qmd") 

to_remove <- files[!files %in% c("index.qmd", 
                                 "word_lists.qmd",
                                 "tasks.qmd",
                                 "about.qmd",
                                 "declension.qmd",
                                 "government.qmd",
                                 "word_in_order.qmd",
                                 "sentence_in_order.qmd",
                                 "plots.qmd",
                                 "lexicon.qmd")]

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

::: {{.content-visible when-profile='english'}}
[![](images/wiktionary.png 'see in Wiktionary'){{height=7mm}}](https://ru.wiktionary.org/wiki/{i})
[![](images/ruscorpora.png 'see in RNC'){{height=7mm}}](https://ruscorpora.ru/word/main?req={i})

```{{r, child='data/{i}.html'}}
```

:::::: {{.panel-tabset}}

## constructions

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
  rename(phrase = phrase_for_site, 
         example = example) ->
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

## exercises

```{{r, child='tasks/{i}.qmd'}}
```

::::::
:::

::: {{.content-visible when-profile='russian'}}

[![](images/wiktionary.png 'смотреть в словаре Wiktionary'){{height=7mm}}](https://ru.wiktionary.org/wiki/{i})
[![](images/ruscorpora.png 'смотреть в Национальном корпусе Русского языка'){{height=7mm}}](https://ruscorpora.ru/word/main?req={i})

```{{r, child='data/{i}.html'}}
```

:::::: {{.panel-tabset}}

## конструкции

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

::::::
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

rm(w_profiles)

# render site -------------------------------------------------------------

library(quarto)
quarto_render(profile = "english")
quarto_render(profile = "russian")


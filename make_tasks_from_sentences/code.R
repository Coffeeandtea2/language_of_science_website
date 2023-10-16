setwd("/home/agricolamz/work/other_projects/language_of_science_website/make_tasks_from_sentences")
library(tidyverse)
library(readxl)
library(udpipe)
ru <- udpipe_load_model("/home/agricolamz/work/databases/spoken_corpora/russian-syntagrus-ud-2.5-191206.udpipe")

read_xlsx("../data/sentences.xlsx") |> 
  mutate(query = str_remove_all(query, "\\|\\|\\|")) ->
  df

# udpipe sentences --------------------------------------------------------

# df |> 
#   pull(query) |> 
#   unique() |> 
#   udpipe(ru) |> 
#   select(doc_id, sentence, term_id, token, lemma, upos, feats) |> 
#   mutate(feats = str_split(feats, "\\|")) |> 
#   unnest_longer(feats) |> 
#   separate(feats, into = c("feature", "value"), sep = "=") |> 
#   pivot_wider(names_from = feature, values_from = value) |> 
#   writexl::write_xlsx("udpiped_sentences.xlsx")

# create tasks from phrase ------------------------------------------------

df |> 
  filter(is.na(target)) ->
  phrase_to_delete

phrase_to_delete |> 
  rename(text = phrase) |> 
  mutate(doc_id = 1:n()) |> 
  select(text, doc_id) |> 
  udpipe(ru) |> 
  select(doc_id, lemma) |> 
  mutate(keep = TRUE) ->
  phrase_udpiped

phrase_to_delete |> 
  rename(text = n_gram) |> 
  mutate(doc_id = 1:n()) |> 
  select(text, doc_id) |> 
  udpipe(ru) |> 
  select(doc_id, token, lemma) |> 
  left_join(phrase_udpiped) |> 
  filter(!is.na(keep)) |> 
  group_by(doc_id) |> 
  summarise(to_delete = str_c(token, collapse = " "),
            lemmata = str_c(lemma, collapse = ", ")) |> 
  mutate(doc_id = as.double(doc_id),
         to_delete = str_replace(to_delete, ".", 
                                 str_c("[", 
                                       str_extract(to_delete, "."), 
                                       toupper(str_extract(to_delete, ".")), 
                                       "]"))) |> 
  arrange(doc_id) ->
  to_delete_phrase_df

phrase_to_delete |> 
  cbind(to_delete_phrase_df) |> 
  filter(lemmata != "NA") |> 
  mutate(lemmata = ifelse(please_be_careful_in_plural == "pl",
                          str_replace_all(phrase, " ", ", "),
                          lemmata),
         query = str_replace(query, to_delete, str_c("____________ (**", lemmata, "**)"))) |> 
  select(stimulus, target, to_delete, query) |> 
  mutate(task_type = "Поставить слово в правильную форму",
         fragment = str_extract(to_delete, "^.."),
         to_delete = str_remove(to_delete, "^...."),
         fragment = str_remove(fragment, "^."),
         to_delete = str_c(fragment, to_delete)) |> 
  rename(task = query,
         answer = to_delete) |> 
  select(-fragment) ->
  tasks_phrase

# create tasks from stimulus ----------------------------------------------

df |> 
  filter(target == "s") ->
  stimulus_to_delete

stimulus_to_delete |> 
  rename(text = stimulus) |> 
  mutate(doc_id = 1:n()) |> 
  select(text, doc_id) |> 
  udpipe(ru) |> 
  select(doc_id, lemma) |> 
  mutate(keep = TRUE) ->
  stimulus_udpiped

stimulus_to_delete |> 
  rename(text = n_gram) |> 
  mutate(doc_id = 1:n()) |> 
  select(text, doc_id) |> 
  udpipe(ru) |> 
  select(doc_id, token, lemma) |> 
  left_join(stimulus_udpiped)   |> 
  filter(!is.na(keep)) |> 
  mutate(doc_id = doc_id |> as.double()) |> 
  group_by(doc_id) |> 
  summarise(to_delete = str_c(token, collapse = ", ")) |> 
  mutate(doc_id = as.double(doc_id),
         to_delete = str_replace(to_delete, ".", 
                                 str_c("[", 
                                       str_extract(to_delete, "."), 
                                       toupper(str_extract(to_delete, ".")), 
                                       "]"))) |> 
  arrange(doc_id) ->
  to_delete_stimulus_df

stimulus_to_delete |> 
  rename(text = phrase) |> 
  mutate(doc_id = 1:n()) |> 
  filter(please_be_careful_in_plural == "pl") |> 
  select(doc_id, text) |> 
  udpipe(ru) |> 
  select(doc_id, token, lemma)  |> 
  left_join(stimulus_udpiped) |> 
  na.omit() |> 
  rename(change = token,
         stimulus = lemma) |> 
  select(doc_id, change, stimulus) |> 
  mutate(doc_id = as.double(doc_id)) ->
  change_df

stimulus_to_delete |> 
  cbind(to_delete_stimulus_df)  |> 
  left_join(change_df) |> 
  mutate(change = ifelse(is.na(change), stimulus, change),
         query = str_replace(query, to_delete, str_c("____________ (**", change, "**)"))) |> 
  select(stimulus, target, to_delete, query) |> 
  mutate(task_type = "Поставить слово в правильную форму",
         fragment = str_extract(to_delete, "^.."),
         to_delete = str_remove(to_delete, "^...."),
         fragment = str_remove(fragment, "^."),
         to_delete = str_c(fragment, to_delete)) |> 
  rename(task = query,
         answer = to_delete) |> 
  select(-fragment) ->
  tasks_stimulus

# create tasks from n-gram extract ----------------------------------------

read_xlsx("../data/sentences.xlsx", sheet = "Какая конструкция (предложное у") |> 
  mutate(query = str_remove_all(query, "\\|\\|\\|")) ->
  df

df |> 
  filter(str_detect(target, "lead")) |> 
  mutate(step = as.double(str_remove(target, "lead"))) ->
  extract_to_delete 
  
extract_to_delete |> 
  filter(step > 0) |> 
  mutate(first_letter = str_extract(n_gram, "."),
         extract = str_c("(?<=([", tolower(first_letter), toupper(first_letter),
                         "]", 
                         str_remove(n_gram, "."),
                         "))(\\W{1,2}\\w*){", step, "}"),
         to_delete = str_extract(query, extract),
         to_delete_n_gram = str_c(n_gram, to_delete),
         to_delete = str_squish(str_remove(to_delete, "\\W")),
         replacement = str_c(" ", str_squish(str_dup("____________ ", step)))) ->
  verb_alignment
  
verb_alignment |> 
  rename(text = to_delete) |> 
  mutate(doc_id = 1:n()) |> 
  select(text, doc_id) |> 
  udpipe(ru) |> 
  group_by(doc_id) |> 
  reframe(lemma = str_c(lemma, collapse = ", ")) |> 
  arrange(as.double(doc_id)) |> 
  rename(brackets = lemma) |> 
  select(brackets) ->
  udpiped_brackets
  
verb_alignment |> 
  bind_cols(udpiped_brackets) |> 
  mutate(query = str_replace(query, to_delete_n_gram, str_c(n_gram, replacement, " (**", brackets, "**)"))) |> 
  select(stimulus, target, to_delete, query) |> 
  mutate(task_type = "Глагольное управление") |> 
  rename(task = query,
         answer = to_delete) ->
  verb_alignment

extract_to_delete |> 
  filter(step == -1) |> 
  mutate(n_words = str_count(n_gram, " ")+1) |>
  rowwise() |> 
  mutate(to_delete = str_split_i(n_gram, pattern = " ", i = n_words)) |> 
  ungroup() ->
  extract_to_delete_lead_minus_one

extract_to_delete_lead_minus_one |>
  rename(text = to_delete) |> 
  mutate(doc_id = 1:n()) |> 
  select(text, doc_id) |> 
  udpipe(ru) |> 
  group_by(doc_id) |> 
  reframe(lemma = str_c(lemma, collapse = ", ")) |> 
  arrange(as.double(doc_id)) |> 
  rename(brackets = lemma) |> 
  select(brackets) ->
  extract_to_delete_lead_minus_one_brackets

extract_to_delete_lead_minus_one |> 
  cbind(extract_to_delete_lead_minus_one_brackets) |> 
  mutate(query = str_replace(query, 
                             n_gram, 
                             str_c(str_remove(n_gram, to_delete), "____________ (**", brackets, "**)"))) |>   
  select(stimulus, target, to_delete, query) |> 
  mutate(task_type = "Глагольное управление") |> 
  rename(task = query,
         answer = to_delete) ->
  verb_alignment_lead_minus_one

if(-2 %in% extract_to_delete$step){
  # finish later
  extract_to_delete |> 
    filter(step == -2) |> 
    mutate(n_words = str_count(n_gram, " ")+1) |> 
    rowwise() |> 
    mutate(to_delete = str_c(str_split_i(n_gram, pattern = " ", i = n_words),
                             " ",
                             str_split_i(n_gram, pattern = " ", i = n_words-1))) ->
    extract_to_delete_lead_minus_two
}

# direct extract ----------------------------------------------------------
read_xlsx("../data/sentences.xlsx") |> 
  mutate(query = str_remove_all(query, "\\|\\|\\|")) |> 
  filter(str_detect(target, "[а-я]")) |> 
  select(stimulus, phrase, n_gram, query, target, please_be_careful_in_plural) ->
  direct_extract

direct_extract |> 
  rename(text = target) |> 
  mutate(doc_id = 1:n()) |> 
  select(text, doc_id) |> 
  udpipe(ru) |> 
  group_by(doc_id) |> 
  reframe(lemma = str_c(lemma, collapse = ", ")) |> 
  arrange(as.double(doc_id)) |> 
  rename(brackets = lemma) |> 
  select(brackets) ->
  direct_extract_brackets

direct_extract |> 
  bind_cols(direct_extract_brackets) |> 
  mutate(change = ifelse(please_be_careful_in_plural == "pl",
                         str_replace_all(phrase, " ", ", "),
                         brackets),
         query = str_replace(query, 
                             target, 
                             str_c("____________ (**", change, "**)")),
         to_delete = target) |> 
  select(stimulus, target, to_delete, query) |> 
  mutate(task_type = "Поставить слово в правильную форму") |> 
  rename(task = query,
         answer = to_delete) ->
  direct_extract_1
  
read_xlsx("../data/sentences.xlsx", sheet = "Какая конструкция (предложное у") |> 
  mutate(query = str_remove_all(query, "\\|\\|\\|")) |> 
  filter(str_detect(target, "[а-я]")) |> 
  select(stimulus, phrase, n_gram, query, target) ->
  direct_extract

direct_extract |> 
  rename(text = target) |> 
  mutate(doc_id = 1:n()) |> 
  select(text, doc_id) |> 
  udpipe(ru) |> 
  group_by(doc_id) |> 
  reframe(lemma = str_c(lemma, collapse = ", ")) |> 
  arrange(as.double(doc_id)) |> 
  rename(brackets = lemma) |> 
  select(brackets) ->
  direct_extract_brackets

direct_extract |> 
  bind_cols(direct_extract_brackets) |> 
  mutate(query = str_replace(query, 
                             target, 
                             str_c("____________ (**", brackets, "**)")),
         to_delete = target) |> 
  select(stimulus, target, to_delete, query) |> 
  mutate(task_type = "Глагольное управление") |> 
  rename(task = query,
         answer = to_delete) ->
  direct_extract_2

# create multiple choise --------------------------------------------------
# 
# read_xlsx("sentences.xlsx") |> 
#   mutate(query = str_remove_all(query, "\\|\\|\\|")) |> 
#   select(stimulus, phrase, query) ->
#   df
# 
# read_xlsx("sentences.xlsx", sheet = "Глагольное управление") |> 
#   mutate(query = str_remove_all(query, "\\|\\|\\|")) |> 
#   select(stimulus, phrase, query) |> 
#   bind_rows(df) |> 
#   filter(str_count(phrase, " ") == 1) ->
#   df
# 
# df |> 
#   rename(text = phrase) |> 
#   mutate(doc_id = 1:n()) |> 
#   select(text, doc_id) |> 
#   udpipe(ru) |> 
#   filter(str_detect(feats, "Case=Nom")) |> 
#   distinct(sentence) |> 
#   rename(phrase = sentence) |> 
#   mutate(nom = TRUE) ->
#   nom_only
# 
# df |> 
#   left_join(nom_only) |> 
#   filter(nom) |> 
#   select(-nom) |>
#   filter(str_detect(query, phrase)) |> 
#   writexl::write_xlsx("multiple_choice.xlsx")
# 
# 
# # extract экономика -------------------------------------------------------
# 
# read_xlsx("sentences.xlsx") |> 
#   mutate(query = str_remove_all(query, "\\|\\|\\|")) |> 
#   select(stimulus, phrase, query) ->
#   df
# 
# read_xlsx("sentences.xlsx", sheet = "Глагольное управление") |> 
#   mutate(query = str_remove_all(query, "\\|\\|\\|")) |> 
#   select(stimulus, phrase, query) |> 
#   bind_rows(df) |> 
#   filter(str_detect(query, "экономик")) |> 
#   writexl::write_xlsx("экономика.xlsx")


# create picture tasks ----------------------------------------------------

files <- list.files("../images/")
files <- files[!(files %in% c("ruscorpora.png", "wiktionary.png"))]
files <- str_remove(files, ".png")

read_xlsx("../data/sentences.xlsx", sheet = "Какой график") |> 
  filter(task %in% files) ->
  make_tasks


map(seq_along(make_tasks$task), function(i){
  read_xlsx("../data/sentences.xlsx", sheet = "Какой график") |> 
    filter(target != make_tasks$target[i]) |> 
    slice_sample(n = 3) |> 
    pull(answer) |> 
    str_c(collapse = " ; ") ->
    options
  make_tasks |> 
    slice(i) |> 
    mutate(options = options,
           stimulus = "",
           task_type = "Графики")
}) |> 
  list_rbind() ->
  pictures
 
# create mixed word tasks -------------------------------------------------

read_xlsx("../data/word_profiles.xlsx") |> 
  distinct(lemma, example) |> 
  na.omit() |> 
  filter(!str_detect(example, "[\\.\\?]$"),
         str_count(example, " ") > 2,
         str_count(example, " ") < 5) |> 
  mutate(example = str_remove_all(example, "\\s{1,}\\(.*?\\)"),
         example = str_replace_all(example, " ", "; "),
         target = NA,
         task = "Поставьте слова в правильном порядке:",
         task_type = "Упорядочить слова") |> 
  rename(answer = example,
         stimulus = lemma) ->
  word_in_order_tasks
  
# create mixed word tasks -------------------------------------------------

read_xlsx("../data/sentences.xlsx", guess_max = 3000) |> 
  filter(is.na(not_in_sentence_in_order)) |> 
  distinct(stimulus, query) |> 
  filter(str_count(query, "[\\.\\?]") == 1) |> 
  na.omit() |>
  mutate(query = str_squish(query), 
         query = str_remove(query, "[\\.\\?]$"),
         query = str_replace(query, ".", str_to_lower(str_extract(query, "."))),
         cnt = str_count(query, " "),
         space_count = (cnt - (cnt %% 4))/4) ->
  generate_sentence_in_order_tasks

generate_sentence_in_order_tasks$query <- 
  map_chr(seq_along(generate_sentence_in_order_tasks$query), function(i){
  my_string <- generate_sentence_in_order_tasks$query[i]
  
  str_locate_all(my_string, " ") |> 
    as.data.frame() |> 
    slice(generate_sentence_in_order_tasks$space_count[i],
          generate_sentence_in_order_tasks$space_count[i]*2,
          generate_sentence_in_order_tasks$space_count[i]*3) |> 
    pull(start) ->
    cuts
  
  str_c(str_sub(my_string, 1, cuts[1]-1),
        ";",
        str_sub(my_string, cuts[1], cuts[2]-1),
        ";",
        str_sub(my_string, cuts[2], cuts[3]-1),
        ";",
        str_sub(my_string, cuts[3], nchar(my_string)))
})

generate_sentence_in_order_tasks |> 
  slice_sample(n = 200) |> 
  mutate(target = NA,
         task = "Поставьте части предложения в правильном порядке:",
         task_type = "Упорядочить фрагменты предложений") |> 
  rename(answer = query) |> 
  select(-cnt, -space_count) ->
  sentence_in_order_tasks

# merge_them_all ----------------------------------------------------------

tasks_stimulus |> 
  bind_rows(tasks_phrase, verb_alignment, verb_alignment_lead_minus_one,
            direct_extract_1, direct_extract_2, word_in_order_tasks,
            sentence_in_order_tasks) |> 
  filter(!is.na(stimulus)) |>
  mutate(options = "") |> 
  bind_rows(pictures) |> 
  arrange(task_type) |>
  mutate(task = str_replace_all(task, "'", "’"),
         task = str_replace_all(task, "населить, пункт", "населенный, пункт"),
         answer = str_remove_all(answer, "\\|\\|\\|")) |> 
  writexl::write_xlsx("../data/tasks.xlsx")

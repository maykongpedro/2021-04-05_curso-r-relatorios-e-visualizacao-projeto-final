
# criar diretório básico de projeto
# fs::dir_create("./dados")
# fs::dir_create("./scripts")
# fs::dir_create("./outputs")


# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either "yyyy-mm-dd" date or year/week works!
#tuesdata <- tidytuesdayR::tt_load('2021-03-09')
tuesdata <- tidytuesdayR::tt_load(2021, week = 11)

raw_bechdel <- tuesdata$raw_bechdel
movies <- tuesdata$movies


# criando base de legenda para a classificação
legend_rating <- 
  tibble::tribble(
    ~ rating,
    ~ description,
    ~ descricao,
    0L,
    "unscored",
    "Não testado",
    1L,
    "It has to have at least two [named] women in it",
    "Tem pelo menos duas mulheres nomeadas",
    2L,
    "Who talk to each other",
    "As mulheres falam entre si",
    3L,
    "About something besides a man",
    "As mulheres falam entre si sobre algo que não seja um homem"
  )

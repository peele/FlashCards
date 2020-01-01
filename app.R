#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Q: English sentence with English word in bold
# H: 3 Spanish words
# A: Spanish sentence with Spanish word in bold
# 
# Q: Spanish sentence with blank
# H: 3 Spanish words
# A: Spanish sentence with Spanish word in bold
# 
# Q: English word
# H: 3 English sentences with blank
# A: Spanish word
# 
# Q: Spanish word
# H: 3 Spanish sentences with blank
# A: English word

library(shiny)
library(tidyverse)
library(readxl)
library(futile.logger)

RANDOM <- 'Random'
ENGLISH_SENTENCE <- 'English Sentence'
SPANISH_SENTENCE <- 'Spanish Sentence'
ENGLISH_WORD <- 'English Word'
SPANISH_WORD <- 'Spanish Word'
QUESTION_TYPES <- c(RANDOM, ENGLISH_SENTENCE, SPANISH_SENTENCE, ENGLISH_WORD, SPANISH_WORD)
BLANK_PATTERN <- '_+'
BLANK <- '______'

handleRandom <- function(type) {
    if(type == RANDOM) {
        t <- sample(tail(QUESTION_TYPES, -1), 1)
        flog.info("Random question type selected: %s", t)
        t
    } else {
        type
    }
}

fillInBlank <- function(sentence, word) {
    parts <- str_split(sentence, BLANK_PATTERN) %>% unlist
    tags$p(parts[[1]], tags$b(word), parts[[2]])    
}

getQuestion <- function(entry, type) {
    flog.info('Entry %d choosen for %s.', entry$Entry, type)
    if(type == ENGLISH_SENTENCE) { # Q: English sentence with English word in bold 
        fillInBlank(entry$EnglishPhrase, entry$English)
    } else if(type == SPANISH_SENTENCE) { # Q: Spanish sentence with blank
        fillInBlank(entry$SpanishPhrase, BLANK)
    } else if(type == ENGLISH_WORD) { # Q: English word
        tags$p(entry$English)  
    } else if(type == SPANISH_WORD) { # Q: Spanish word
        tags$p(entry$Spanish)    
    } else {
        flog.error("Unknown type: %s", type)
    }
}

getHint <- function(entry, type, all) {
    flog.info('Show %s hint for entry %d.', type, entry$Entry)
    if(type == ENGLISH_SENTENCE || type == SPANISH_SENTENCE) { # H: 3 Spanish words 
        hints <- all %>%
            filter(Spanish != entry$Spanish) %>%
            select(Spanish) %>%
            distinct() %>%
            sample_n(2) %>%
            bind_rows(select(entry, Spanish))
        hints <- hints[sample(1:3), ] %>% unlist() %>% map(function(x) x %>% tolower() %>% tags$p())
        hints        
    } else if(type == ENGLISH_WORD) { # H: 3 English sentences with blank
        hints <- all %>%
            filter(EnglishPhrase != entry$EnglishPhrase) %>%
            select(EnglishPhrase) %>%
            distinct() %>%
            sample_n(2) %>%
            bind_rows(select(entry, EnglishPhrase))
        hints <- hints[sample(1:3), ] %>% unlist() %>% map(function(x) fillInBlank(x, BLANK))
        hints 
    } else if(type == SPANISH_WORD) { # H: 3 Spanish sentences with blank
        hints <- all %>%
            filter(SpanishPhrase != entry$SpanishPhrase) %>%
            select(SpanishPhrase) %>%
            distinct() %>%
            sample_n(2) %>%
            bind_rows(select(entry, SpanishPhrase))
        hints <- hints[sample(1:3), ] %>% unlist() %>% map(function(x) fillInBlank(x, BLANK))
        hints  
    } else {
        flog.error("Unknown type: %s", type)
    }
}

getAnswer <- function(entry, type) {
    flog.info('Show %s answer for entry %d.', type, entry$Entry)
    if(type == ENGLISH_SENTENCE) { # A: Spanish sentence with Spanish word in bold
        fillInBlank(entry$SpanishPhrase, entry$Spanish)
    } else if(type == SPANISH_SENTENCE) { # A: Spanish sentence with Spanish word in bold
        fillInBlank(entry$SpanishPhrase, entry$Spanish)
    } else if(type == ENGLISH_WORD) { # A: Spanish word
        tags$p(entry$Spanish)
    } else if(type == SPANISH_WORD) { # A: English word
        tags$p(entry$English)
    } else {
        flog.error("Unknown type: %s", type)
    }
}

inputFile <- 'data/Spanish.xlsx'
flog.info('Loading file: %s', inputFile)

data <- read_excel(inputFile)
data$Entry <- 1:nrow(data)
missingBlanks <- data %>% 
    filter(!str_detect(SpanishPhrase, BLANK_PATTERN) | !str_detect(EnglishPhrase, BLANK_PATTERN)) %>%
    select(Entry)
if(nrow(missingBlanks) > 0) {
    flog.warn('Entries [%s] are missing blanks and will be removed.', paste(missingBlanks$Entry, collapse = ', '))
    data <- data %>%
        filter(!Entry %in% missingBlanks$Entry)
}

flog.info('%s contains %d rows.', inputFile, nrow(data))

ui <- fluidPage(theme = 'flashcards.css',

    titlePanel("Spanish Flashcards"),

    sidebarLayout(
        mainPanel(
            actionLink(
                'questionBtn',
                tags$div(
                    tags$h3('Question'),
                    htmlOutput('questionTxt'),
                    class = 'card'
                ),
                class = 'cardLink'
            ),
            actionLink(
                'hintBtn',
                tags$div(
                    tags$h3('Hint'),
                    htmlOutput('hintTxt'),
                    class = 'card'
                ),
                class = 'cardLink'
            ),
            actionLink(
                'answerBtn',
                tags$div(
                    tags$h3('Answer'),
                    htmlOutput('answerTxt'),
                    class = 'card'
                ),
                class = 'cardLink'
            )
        ),
        
        sidebarPanel(
            selectInput(
                'questionType',
                'Question Type',
                QUESTION_TYPES,
                ENGLISH_SENTENCE
            )
        )
    )
)

server <- function(input, output) {

    currentEntry <- NULL
    currentType <- NULL
    hintGiven <- FALSE
    updateQuestion <- function() {
        currentEntry <<- data %>% sample_n(1)
        currentType <<- handleRandom(input$questionType)
        output$questionTxt <- renderUI({
            getQuestion(currentEntry, currentType)
        })
        output$hintTxt <- renderUI({''})
        output$answerTxt <- renderUI({''})
        hintGiven <<- FALSE
    }
    
    observeEvent(input$questionType, {
        flog.info('Question type: %s', input$questionType)
        updateQuestion()
    })
    
    observeEvent(input$questionBtn, {
        flog.info('Question button click.')
        updateQuestion()
    })
    
    observeEvent(input$hintBtn, {
        flog.info('Hint button click.')
        if(hintGiven) {
            flog.info('Hint already given.')
        } else {
            output$hintTxt <- renderUI({
                getHint(currentEntry, currentType, data)
            })
            hintGiven <<- TRUE
        }
    })
    
    observeEvent(input$answerBtn, {
        flog.info('Answer button click.')
        output$answerTxt <- renderUI({
            getAnswer(currentEntry, currentType)
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

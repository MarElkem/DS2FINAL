library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(wordcloud2)
library(shinyjs)
library(tidytext)
library(tidyverse)

kendrick_pac_final <- read_csv("~/Sds 264 F24/final/kendrick_pac_final.csv")


ui <- navbarPage("Kendrick and Tupac Analysis",
                 tags$head(
                   tags$style(HTML("
                     body {
      background-color: black !important;
      color: white !important;
      font-size: 16px !important; /* Default font size */
    }
    .navbar {
      background-color: #000000 !important;
      font-size: 18px !important; /* Navbar font size */
    }
    .tab-content {
      background-color: black !important;
      font-size: 16px !important; /* Tab content font size */
    }
    .form-control, .btn {
      background-color: #333333 !important;
      color: white !important;
      font-size: 16px !important; /* Input and button font size */
    }
    .selectize-input, .selectize-dropdown-content {
      background-color: #333333 !important;
      color: white !important;
      font-size: 16px !important; /* Dropdown font size */
    }
    .selectize-input > div {
      color: white !important;
      font-size: 16px !important;
    }
    .irs-bar, .irs-bar-edge, .irs-single {
      background-color: #444444 !important;
      color: white !important;
      font-size: 14px !important; /* Slider font size */
    }
    .slider-animate-button {
      background-color: #444444 !important;
      color: white !important;
      font-size: 14px !important;
    }
    label {
      color: black !important;
      font-size: 16px !important; /* Label font size */
    }
    h1, h2, h3, h4, h5, h6 {
      font-size: 24px !important; /* Header font size */
    }
    p {
      font-size: 20px !important; /* Paragraph font size */
    }
  "))
                 ),
                 
                 tabPanel( #This is the introduction tab which has an image and paragraphs for explanations
                   "Introduction",
                   fluidPage(
                     titlePanel("Introduction"),
                     fluidRow(
                       column(6, 
                              HTML('<img src="imgg.png" height="1000px" width="800px">')),
                       column(6,
                              p("This project explores the relationship between the commercial success and lyrical content of two legendary artists: Kendrick Lamar and Tupac Shakur."),
                              p("By examining their album sales and analyzing the sentiment of their lyrics, we are hoping to provide insights into their artistic journeys and cultural impact."),
                              p("The app is structured to guide you through an engaging story, highlighting key themes and trends. We are hoping users to explore:"),
                              tags$ul(
                                tags$li("How does sentiment affect sales?"),
                                tags$li("What are the trends in the artists’ lyrics sentiments over the years?"),
                                tags$li("What are the lyrics/words that contribute to the uniqueness of a song?"),
                                tags$li("How do both artists compare to each other?")
                              ),
                              p("Through these insights, the project not only celebrates the legacy of these iconic figures but also offers a deeper understanding of how their music connects with listeners."),
                              p("Whether you’re a fan, a researcher, or just curious, this project invites you to dive into the art of Kendrick Lamar and Tupac."),
                              p(tags$b("Note:"), "This project will include explicit words.")
                       )
                     )
                   )
                 ),
                 # This is the tab for Sales Analysis which features one interactive plot based on the action  buttons below
                 tabPanel("Sales Analysis",
                          fluidPage(
                            titlePanel("Album Sales Analysis (choose the artist by clicking on the buttons!)"),
                            fluidRow(
                              column(6, actionButton("kendrick_btn", "Kendrick Lamar")),
                              column(6, actionButton("tupac_btn", "2pac")) #recording instance of choice
                            ),
                            plotlyOutput("salesPlot"),
                            fluidRow(
                              column(12,
                                     tags$div(
                                       style = "margin-top: 20px; font-size: 18px;",
                                       p("We can see that the most sold album for Kendrick Lamar is ",
                                         tags$b("“DAMN”"), ", which was released in 2017. The album sold more than ",
                                         tags$b("4 million copies"), 
                                         "- an impressive number of sales considering that, unlike in 2Pac’s generation, album sales are no longer prominent due to streaming services being widely available."),
                                       p("On the other hand, 2Pac’s most sold album was ", 
                                         tags$b("“All Eyez on Me”"), 
                                         "- with a record of almost ", 
                                         tags$b("6 million copies sold"), "."),
                                       p("Let us explore the contents of the artists’ albums to investigate any potential correlation between album sales and sentiment.")
                                     )
                              )
                            )
                          )
                 ),
                 # This is the tab for Lyrics Analysis which includes 4 plots with description
                 tabPanel("Lyrics Analysis",
                          fluidPage(
                            useShinyjs(),  # This is for showing the desired plot based on user input
                            titlePanel("Lyrics Analysis"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("Artist", "Select Artist:", choices = NULL),
                                selectInput("PlotType", "Select Plot Type:", 
                                            choices = c("Word Cloud", "Sentiment vs Sales", "Sentiment vs Years", "TF-IDF per Album")),
                                conditionalPanel(
                                  condition = "input.PlotType == 'TF-IDF per Album'",
                                  sliderInput("numAlbums", "Number of Albums to Display:", 
                                              min = 1, max = 10, value = 5) # This is a slider for choosing the number of albums to display for tf-idf as too many albums fill up the screen
                                )
                              ),
                              mainPanel( #This is the wordcloud
                                conditionalPanel(
                                  condition = "input.PlotType == 'Word Cloud'",
                                  wordcloud2Output("wordcloud", height = "700px", width = "100%"),
                                  tags$div(
                                    style = "margin-top: 20px; font-size: 18px; color: white; text-align: left;",
                                    hr(),  #separate the word cloud and the text
                                    p("The most frequently used words in Kendrick Lamar’s songs are—",
                                      tags$b("“life”"), ", ", tags$b("“time”"), ", ", tags$b("“world”"), 
                                      ", and ", tags$b("“love”"), "."),
                                    p("Tupac Shakur’s—", tags$b("“life,”"), " ", tags$b("“time,”"), " ", 
                                      tags$b("“love,”"), " and ", tags$b("“thug”"), "."),
                                    p("By noting this, we observe similarity between 2Pac and Kendrick when it comes to themes of life and values. At the same time, there are words with strong negative connotations such as ",
                                      tags$b("“lost”"), ", ", tags$b("“bust”"), ", ", tags$b("“kill”"), 
                                      ", and ", tags$b("“dead”"), "- further shedding light on the complexity of both artists.")
                                  )
                                ),
                                conditionalPanel(  #this is the sentiment vs sales plot
                                  condition = "input.PlotType == 'Sentiment vs Sales'",
                                  plotlyOutput("sentimentPlot", height = "700px", width = "100%"),
                                  tags$div(
                                    style = "margin-top: 20px; font-size: 18px; color: white; text-align: left;",
                                    hr(),  
                                    p("The sales data reveals a fascinating trend. The highest selling album stands out with a dominance of negative sentiment in its lyrical themes."),
                                    p("We can see that for Kendrick, his album ", tags$b("DAMN."), 
                                      ", which according to this graph had mostly negative sentiment, was the most sold album. This album stands as a narrative masterpiece that explores the duality of good vs evil, purity vs sin, and weakness vs wickedness, which was the dominant theme throughout. This duality may be an explanation of why his most sold album ", tags$b("DAMN."), 
                                      " resonates so strongly with the listeners, inviting them to reflect on morality and life struggles, to become one of his most successful albums despite its heavy, negative tone."),
                                    p("We can see that the album with the highest sales for Tupac has mostly negative sentiment as well. With some research, we find that this album was released in 1996 and marked Tupac’s first release under Death Row Records. The year prior, Death Row Records had bailed him out of jail in exchange for signing him to the label."),
                                    p("This pivotal moment in Tupac’s life influenced the album’s tone and reception. The professional promotion by Death Row Records played a significant role in its commercial success, while the negative sentiment likely reflects the raw emotions stemming from Tupac’s incarceration and the turmoil surrounding his life during that period.")
                                  )
                                ),
                                conditionalPanel( #this is the sentiment vs years plot
                                  condition = "input.PlotType == 'Sentiment vs Years'",
                                  plotlyOutput("sentimentYearsPlot", height = "700px", width = "100%"),
                                  tags$div(
                                    style = "margin-top: 20px; font-size: 18px; color: white; text-align: left;",
                                    hr(),
                                    tags$h3(style = "color: white; text-align: left;", "Kendrick"),
                                    p("For Kendrick, we observe a negative sentiment of ", tags$b("-100"),
                                      " which increases in negativity over his next two albums to ", 
                                      tags$b("-122"), " and ", tags$b("-159"), " respectively. He then has his highest peak of lesser negative sentiment (", tags$b("-50"), 
                                      ") in 2016 with the album ", tags$b("Untitled Unmastered"), 
                                      ". This specific album is a compilation of outtakes from a previous album ", tags$b("To Pimp a Butterfly"), 
                                      "- with more instances of vocal singing than before. We then see a negative dip for his next two works- with the most negative sentiment."),
                                    p("In Kendrick's own words: ", tags$i("“DAMN.” is a “judgment of the soul”"), 
                                      ". The album includes a song detailing his own death that occurs from a blind assailant shooting him. This is an incredibly unique story where Kendrick details himself being in a situation that is emotional and real."),
                                    tags$h3(style = "color: white; text-align: left;", "Tupac"),
                                    p("For 2Pac, we observe a more changing sentiment, with many more ups and downs. The most negative album is ", tags$b("“All Eyez on Me”"), 
                                      " which was released in 1996 and marked Tupac’s first release under Death Row Records. The year prior, Death Row Records had bailed him out of jail in exchange for signing him to the label."),
                                    p("This pivotal moment in Tupac’s life influenced the album’s tone and reception. The professional promotion by Death Row Records played a significant role in its commercial success, while the negative sentiment likely reflects the raw emotions stemming from Tupac’s incarceration and the turmoil surrounding his life during that period."),
                                    p("Unfortunately, 2Pac had passed away in 1996 a few months after releasing ", tags$b("“All Eyez on Me”"), 
                                      ". All posthumous albums have a more positive sentiment than 2Pac's own releases— which begs the question of how one's legacy could be interpreted differently after passing.")
                                  )
                                ), #this is the tf-idf album
                                conditionalPanel(
                                  condition = "input.PlotType == 'TF-IDF per Album'",
                                  plotlyOutput("tfidfPlot", height = "700px", width = "100%"),
                                  tags$div(
                                    style = "margin-top: 20px; font-size: 18px; color: white; text-align: left;",
                                    hr(),
                                    tags$h3(style = "color: white; text-align: left;", "Kendrick"),
                                    p("The TF-IDF plot identifies the most important and distinctive words in each album. Words with high TF-IDF values are unique to specific albums, revealing central concepts in the lyrics."),
                                    p("For Kendrick, we see a progression in themes, whether political, social, or personal, across different stages of his career. For instance, in ", tags$b("Untitled Unmastered"), 
                                      ", there was a mix of emotions captured by words like ", tags$b("“crying”"), ", ", tags$b("“mortal”"), ", and ", tags$b("“hooray”"), 
                                      ". This demonstrates a shift in focus and emotion based on the type of album and the targeted audience."),
                                    tags$h3(style = "color: white; text-align: left;", "Tupac"),
                                    p("For Tupac, the TF-IDF plot reveals a duality in his lyrical themes. Some words like ", tags$b("“la la”"), " seem lighthearted, while others such as ", tags$b("“murder murder”"), 
                                      " carry heavy, serious undertones. This reflects the dynamic nature of Tupac’s artistry, where playful and intense themes coexist, showcasing his versatility as an artist.")
                                  )
                                )
                              )
                            )
                          )
                 ),
                  # This is the tab for 2Pac x Kendrick Analysis
                 tabPanel("2Pac x Kendrick",
                          fluidPage(
                            titlePanel("2Pac vs. Kendrick Analysis"),
                            fluidRow(
                              column(6, actionButton("sentiment_years_btn", "Sentiment Over The Years")),
                              column(6, actionButton("sentiment_sales_btn", "Net Sentiment and Sales"))
                            ),
                            fluidRow(
                              plotlyOutput("comparisonPlot", height = "700px", width = "100%"),
                              tags$div(
                                style = "margin-top: 20px; font-size: 18px; color: white;",
                                p("Net Sentiment Over the Years:"),
                                p("Above we can see a comparison of both artist sentiment over the years. For Tupac, there are more dramatic changes in sentiment than Kendrick. However, we must note that Tupac has more albums released than Kendrick, and some of them are posthumous."),
                                p("An observation to point out is the last released Tupac album and the first ever released Kendrick album. Kendrick released his first album in the same year Tupac’s last album was released - post his death."),
                                hr(),
                                p("Net Sentiment and Sales:"),
                                p("After fitting a line for both artists' sales and sentiment, we can observe that the higher the negative sentiment for Tupac, the less sales an album is likely to have."),
                                p("On the other hand, Kendrick’s sales seem to be unaffected by sentiment as the fitted line shows a constant slope. However, note that this is overall net sentiment rather than the trend observed for negative and positive sentiment separately in 'Lyrics Analysis.'")
                              )
                            )
                          )
                 ),
                  # This is the conclusion tab
                 tabPanel("Conclusion",
                          fluidPage(
                            tags$style(HTML("
             .fullscreen-image {
               position: fixed;
               top: 0;
               left: 0;
               width: 100%;
               height: 100%;
               z-index: -1;
               background: url('artt.jpg') no-repeat center center fixed; 
               background-size: cover;
             }
           ")),
                            div(class = "fullscreen-image"), # fullscreen background image
                            titlePanel("Conclusion"),
                            fluidRow(
                              column(12,
                                     tags$div(
                                       style = "margin-top: 20px; font-size: 18px; color: white;",
                                       p("The idea of comparing two artists from different eras is difficult and subjective to the nature of the comparison."),
                                       p("Despite the fact that both artists’ most sold albums (", tags$b("DAMN."), " and ", tags$b("All Eyez on Me"), 
                                         ") have predominantly negative sentiment, they resonate deeply with the listeners."),
                                       p("While Tupac’s sentiment shows more fluctuation over time, with negative sentiment correlating to higher sales, Kendrick's sales remain steady regardless of sentiment."),
                                       p("Both artists have left a lasting impression on previous and current generations, and it is worth stating that they’ve pushed the boundaries of Hip-Hop with their influential work throughout their careers."),
                                       tags$audio(
                                         controls = TRUE,
                                         tags$source(src = "song.wav", type = "audio/wav"), #add audio player
                                       ),
                                       tags$hr(),
                                       tags$h3("Data Sources"),
                                       tags$div(
                                         style = "margin-top: 20px; font-size: 18px; color: white;",
                                         tags$h4("Kendrick:"),
                                         p(tags$b("Lyrics:"), tags$a(href = "https://www.kaggle.com/datasets/ceebloop/kendrick-lamar-albumslyrics-dataset", "Kaggle Dataset")),
                                         p(tags$b("Sales:"), tags$a(href = "https://bestsellingalbums.org/artist/6902", "Best Selling Albums")),
                                         tags$h4("Tupac:"),
                                         p(tags$b("Lyrics:"), tags$a(href = "https://www.kaggle.com/datasets/leopoldofacci/tupac-dataset-lyrics-analyse", "Kaggle Dataset")),
                                         p(tags$b("Sales:"), tags$a(href = "https://en.wikipedia.org/wiki/Tupac_Shakur_discography#:~:text=Throughout%20his%20career%20and%20posthumously%2C%20Shakur%20sold%20over%2075%20million,music%20artists%20of%20all%20time", "Wikipedia"))
                                       )
                                     )
                              )
                            )
                          )
                 ))
                 

                 
server <- function(input, output, session) {
  
  # This stores the selected artist for Sales Analysis
  selected_artist <- reactiveVal("Kendrick")
  
  # Update selected artist based on button clicks
  observeEvent(input$kendrick_btn, {
    selected_artist("Kendrick")
  })
  
  observeEvent(input$tupac_btn, {
    selected_artist("2pac")
  })
  
  shiny::observe({
    updateSelectInput(session, "Artist", 
                      choices = unique(kendrick_pac_final$Artist),
                      selected = selected_artist())
  })
  
  # This is to filter data based on dropdown selection
  filtered_data <- reactive({
    req(input$Artist)  # Ensure input is available
    kendrick_pac_final |> 
      filter(Artist == input$Artist)
  })
  
  shiny::observe({
    artist_album_count <- kendrick_pac_final |> 
      filter(Artist == input$Artist) |> 
      distinct(Album) |> 
      nrow()
    # Slider for tf-idf
    updateSliderInput(session, "numAlbums", 
                      max = artist_album_count, 
                      value = min(5, artist_album_count)) 
  })
  
  plot_type <- reactiveVal("sentiment_years")
  
  # Change plot  based on button clicks
  observeEvent(input$sentiment_years_btn, {
    plot_type("sentiment_years")
  })
  
  observeEvent(input$sentiment_sales_btn, {
    plot_type("sentiment_sales")
  })
  
  output$comparisonPlot <- renderPlotly({
    req(plot_type())
    
    if (plot_type() == "sentiment_years") { #this only runs when the user selects it
      # This is the sentiment Over The Years Plot
      sentiment_years_data <- kendrick_pac_final |> 
        inner_join(get_sentiments("bing"), by = "word") |>  #get sentiment
        group_by(Year, Artist) |> 
        summarise(
          positive = sum(sentiment == "positive"),
          negative = sum(sentiment == "negative"),
          net_sentiment = positive - negative,
          .groups = "drop"
        )
      
      sentiment_years_plot <- ggplot(sentiment_years_data, aes(x = Year, y = net_sentiment, color = Artist)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        theme_minimal() +
        labs(
          title = "Sentiment Over The Years: 2Pac vs. Kendrick",
          x = "Year",
          y = "Net Sentiment"
        ) +
        scale_color_manual(values = c("2pac" = "blue", "Kendrick Lamar" = "darkred"))
      
      ggplotly(sentiment_years_plot, tooltip = c("x", "y", "color"))
      
    } else if (plot_type() == "sentiment_sales") {
      # This is the Net Sentiment and Sales Plot
      sentiment_sales_data <- kendrick_pac_final |> 
        inner_join(get_sentiments("bing"), by = "word") |> 
        group_by(Album, Artist) |> 
        summarise(
          positive = sum(sentiment == "positive"),
          negative = sum(sentiment == "negative"),
          net_sentiment = positive - negative,
          Sales = sum(Sales, na.rm = TRUE),
          .groups = "drop"
        )
      
      sentiment_sales_plot <- ggplot(sentiment_sales_data, aes(x = net_sentiment, y = Sales, color = Artist)) +
        geom_point(size = 3, alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE, aes(color = Artist, linetype = Artist)) +
        theme_minimal() +
        labs(
          title = "Net Sentiment and Sales: 2Pac vs. Kendrick",
          x = "Net Sentiment",
          y = "Sales (in millions)"
        ) +
        scale_color_manual(values = c("2pac" = "blue", "Kendrick Lamar" = "darkred")) +
        guides(
          color = guide_legend(title = "Artist"),
          linetype = "none"
        )
      
      ggplotly(sentiment_sales_plot, tooltip = c("x", "y", "color")) #For interactivity
    }
  })

  
  output$salesPlot <- renderPlotly({
    sales_final <- filtered_data() |> 
      distinct(Album, Year, Sales) |> 
      rename(album = Album, year = Year, sales = Sales)
    
    sales_final$sales <- as.numeric(sales_final$sales)
    sales_final$album <- factor(sales_final$album, levels = sales_final$album[order(sales_final$year)])
    
    gg_plot <- ggplot(sales_final, aes(x = album, y = sales, group = 1)) +
      geom_line(aes(color = factor(year)), size = 1) +
      geom_point(aes(color = factor(year)), size = 4) + 
      labs(title = paste(selected_artist(), "Album Sales by Year"),
           x = "Album",
           y = "Sales (in millions)") +
      theme_minimal() +
      theme(legend.position = "none",  
            axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    interactive_plot <- ggplotly(gg_plot)
    interactive_plot
  })
  
  
  
  #Word Cloud
  output$wordcloud <- renderWordcloud2({
    req(input$Artist, input$PlotType == "Word Cloud")
    
    # Filter data for the selected artist
    artist_data <- filtered_data()
    
    # Generate word cloud for all words with more than 3 letters
    filtered_word_data <- artist_data |> 
      filter(!is.na(word)) |> 
      filter(str_length(word) > 3) |> 
      anti_join(stop_words, by = "word") |> 
      count(word, sort = TRUE)
    
    
    filtered_word_data <- filtered_word_data |> 
      arrange(desc(n)) |> 
      rename(freq = n)
    
    wordcloud2(filtered_word_data, size = 1.5, color = "random-light", backgroundColor = "black")
  })
  
  output$sentimentPlot <- renderPlotly({
    req(input$Artist, input$PlotType == "Sentiment vs Sales")
    
    # Get sentiment
    sentiment_analysis <- filtered_data() |> 
      inner_join(get_sentiments("bing"), by = "word") |> 
      count(Album, sentiment) |> 
      pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |> 
      mutate(positive = replace_na(positive, 0), negative = replace_na(negative, 0))
    
    # Join with album sales
    album_sales_data <- filtered_data() |> 
      distinct(Album, Sales) |> 
      left_join(sentiment_analysis, by = "Album") |> 
      filter(!is.na(Sales))  # Ensure no missing sales data
    

    
    sentiment_plot <- ggplot(album_sales_data) +
      geom_point(aes(x = positive, y = Sales, color = "Positive"), size = 3, alpha = 0.7) +
      geom_point(aes(x = negative, y = Sales, color = "Negative"), size = 3, alpha = 0.7) +
      geom_smooth(aes(x = positive, y = Sales, color = "Positive"), method = "lm", se = FALSE) +
      geom_smooth(aes(x = negative, y = Sales, color = "Negative"), method = "lm", se = FALSE) +
      theme_minimal() +
      labs(
        title = paste(selected_artist(), "Sentiment vs. Album Sales"),
        x = "Sentiment Score",
        y = "Album Sales (in millions)"
      ) +
      scale_color_manual(values = c("Positive" = "darkred", "Negative" = "blue"))
    
    ggplotly(sentiment_plot, tooltip = c("x", "y", "color")) %>%
      plotly::layout(showlegend = TRUE, legend = list(title = list(text = "Sentiment")))
  })
  
  
  
  #  Sentiment x Years Plot
  output$sentimentYearsPlot <- renderPlotly({
    req(input$Artist, input$PlotType == "Sentiment vs Years")
    
    bing_sentiment <- filtered_data() |> 
      inner_join(get_sentiments("bing"), by = "word") |> 
      count(Year, sentiment) |> 
      pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |> 
      mutate(net_sentiment = positive - negative)
    
    sentiment_years_plot <- ggplot(bing_sentiment, aes(x = Year, y = net_sentiment)) +
      geom_line(color = "darkred", size = 1.2) +
      geom_point(color = "darkred", size = 2) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = "Net Sentiment Over Time", 
        x = "Year", 
        y = "Net Sentiment"
      ) +
      scale_x_continuous(breaks = unique(bing_sentiment$Year))
    
    ggplotly(sentiment_years_plot, tooltip = c("x", "y"))
  })
  
  #  TF-IDF per Album Plot
  output$tfidfPlot <- renderPlotly({
    req(input$Artist, input$PlotType == "TF-IDF per Album", input$numAlbums)
    
    artist_data <- filtered_data()
    
    artist_bigrams <- artist_data |> 
      group_by(Song, Album) |> 
      summarise(Lyrics = str_c(word, collapse = " "), .groups = "drop") |>  #The kendrick_pac data is unnested so we have to nest it again
      unnest_tokens(bigram, Lyrics, token = "ngrams", n = 2) |> 
      filter(!is.na(bigram))
    
    bigram_tf_idf <- artist_bigrams |> 
      separate(bigram, into = c("word1", "word2"), sep = " ") |> 
      filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) |> 
      unite(bigram, word1, word2, sep = " ") |> 
      count(Album, bigram, sort = TRUE) |> 
      bind_tf_idf(bigram, Album, n)
    
    selected_albums <- bigram_tf_idf |> 
      group_by(Album) |> 
      summarise(total_tfidf = sum(tf_idf, na.rm = TRUE)) |> 
      arrange(desc(total_tfidf)) |> 
      slice_max(total_tfidf, n = input$numAlbums) |> 
      pull(Album)
    
    tfidf_data <- bigram_tf_idf |> 
      filter(Album %in% selected_albums) |> 
      group_by(Album) |> 
      arrange(desc(tf_idf)) |> 
      slice_max(tf_idf, n = 10, with_ties = FALSE) |> 
      ungroup()
    
    tfidf_plot <- ggplot(tfidf_data, aes(x = fct_reorder(bigram, tf_idf), y = tf_idf, fill = Album)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      facet_wrap(~Album, scales = "free") +
      theme_minimal() +
      labs(
        title = paste("Top 10 TF-IDF Bigrams for Top", input$numAlbums, "Albums"),
        x = "Bigram",
        y = "TF-IDF"
      )
    
    ggplotly(tfidf_plot, tooltip = c("x", "y"))
  })
  
  #toggling to change what the user sees based on their selection
  shiny::observe({
    toggle("wordcloud", condition = input$PlotType == "Word Cloud")
    toggle("sentimentPlot", condition = input$PlotType == "Sentiment vs Sales")
    toggle("sentimentYearsPlot", condition = input$PlotType == "Sentiment vs Years")
    toggle("tfidfPlot", condition = input$PlotType == "TF-IDF per Album")
  })
}




shinyApp(ui = ui, server = server)

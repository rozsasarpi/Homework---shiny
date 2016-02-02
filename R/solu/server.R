# in general this requires no modification

# if the shinyapp is called from markdown document then it cannot find the global.R automatically
source("R/solu/global.R")

shinyServer(function(input, output, session) { 
  
  output$select_problem <- renderUI({
    list(selectInput("prob_n", label = Problem_txt, 
      choices = prob_choices)
    )
  })

  output$solu_ui <- eventReactive(input$show_solu, {
    
    if (!any(input_data$Neptun == input$neptun)) {
      HTML(unknownneptun_msg)
    } else {
      solu_file = paste(input$prob_n,"_solu_template", sep = "")
      
      td = tempdir()
      # create the *md file with user specific values 
      rmarkdown::render(paste(solu_file, ".Rmd", sep = ""),
        output_dir = td, intermediates_dir = td,
        encoding = "UTF-8",
        quiet = F, clean = T,
        params = list(stud_neptun = input$neptun))
      
      # create html file from md
      solu_ui <- markdown::renderMarkdown(file.path(td, paste(solu_file, ".md", sep = "")))
      
      # remove temporary files (folder created?)
      unlink(file.path(td, paste(solu_file, "*", sep = "")))
      
      # required to be rendered as MathJax
      solu_ui = paste(solu_ui, "<script>MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>")
    }
  })
})
# in general this requires no modification

# if the shinyapp is called from markdown document then it cannot find the global.R automatically
source("R/submit/global.R")

shinyServer(function(input, output, session) {
  
  ## Programatically generate the ui for sumbission page
  output$submit_ui <- renderUI({
    # number of problems
    probs  = sort(as.numeric(unique(levels(comp_solu_names$problem_n))))
    n_prob = length(probs)
    # n_prob = max(comp_solu_names$problem_n)
    # number of inputs
    n_input = dim(comp_solu_names)[1]
    
    LL = vector("list", n_input + n_prob*2)
    oo = 1 # counter of all elements
    ss = 1 # required input variable counter
    
    # loop through the problems
    for (ii in 1:n_prob) {
      LL[[oo]] = list(hr())
      LL[[oo+1]] = list(h2(paste(probs[ii], problem_txt, sep = "")))
      oo = oo + 2
      n_var = sum(comp_solu_names$problem_n == probs[ii])
      # loop through the required input variables
      for (jj in 1:n_var){
        LL[[oo]] = list(user_input(ss, comp_solu_names))
        ss = ss + 1
        oo = oo + 1
      }
    }
    return(LL)
  })
  
  ## Diagnosis messages for submission
  diag = eventReactive(input$submit, {
    if (!any(solu_data$Neptun == input$neptun)) {
      diag_message = unknownneptun_msg
      diag_code    = FALSE
    } else if (!check_input_type(input, comp_solu_names)){
      diag_message = notnumber_msg
      diag_code    = FALSE
    } else {
      diag_message = success_msg
      diag_code    = TRUE
    }
    # print(diag_message)
    diag = list(message = diag_message, code = diag_code)
  })
  
  
  ## Submit, save and evaluate
  observeEvent(input$submit, {
    # Feedback for user and prevent submitting again before upload completed
    shinyjs::text("submit_msg", diag()$message)
    shinyjs::show("submit_msg")
    
    if (diag()$code) {
      shinyjs::disable("submit")
      
      # evaluate the inputs and color them accordingly
      student_solu = clean_input(input, comp_solu_names)[,-1]
      solu = as.numeric(comp_solu_data[comp_solu_data$Neptun == input$neptun,-1])
      c_par = as.character(comp_solu_names$comp_parameter)
      # solu_eval = as.numeric(signif(student_solu,3) == signif(solu,3)) # WARNING! check with 3 significant digit precision
      

      # WARNING! check with 1% precision
      idx = solu == 0
      solu_eval = vector('integer', length(solu))
      
      if (all(!idx)) {
        solu_eval[!idx] = as.numeric(abs((student_solu[!idx] - solu[!idx])/solu[!idx]) <= 0.01)
      } else {
        solu_eval[idx] = as.numeric(abs(student_solu[idx] - solu[idx]) <= 0.01)
        solu_eval[!idx] = as.numeric(abs((student_solu[!idx] - solu[!idx])/solu[!idx]) <= 0.01)
      }
      # WARNING! check with 1% precision
      

      max_points = as.numeric(levels(comp_solu_names$point))[comp_solu_names$point]
      sum_points = sum(solu_eval*max_points, na.rm = TRUE)

      # save the provided input
      save_input(input, comp_solu_names, sheet_key, sum_points)
      
      # loop through the required solu parameters and change their color
      for (ii in 1:length(c_par)){
        if (is.na(student_solu[[ii]])) {
          shinyjs::removeClass(c_par[ii], "red")
          shinyjs::removeClass(c_par[ii], "green")
        }
        else if (solu_eval[ii] == TRUE) {
          shinyjs::removeClass(c_par[ii], "red")
          shinyjs::addClass(c_par[ii], "green")
        } else {
          shinyjs::removeClass(c_par[ii], "green")
          shinyjs::addClass(c_par[ii], "red")
        }
      }
      
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
      shinyjs::show("sucess_img")
    }
    
    # notify the user that the input is saved; within the above block it would interfere with on.exit()
    if (diag()$code) {
      shinyjs::info(save_msg)
    }
  })
  
  renderText(diag()$message)
})
##############################################################################
## Code for: Indicator Scores Function
## Authors: Ruben Sanchez Ramirez
## Contact: Ruben Sanchez Ramirez (rubensanchezramirez@bren.ucsb.edu)
################################################################################

## Background: 
## Write a function that can take a data frame of survey scores from our survey and produce a table with grading standard and scores.

## Take the raw scores and max scores to find the percent for each question
## Combine questions based on respones id and category
## Find the percent score per indicator based on id by adding the scores within the same category and dividing by the number of questions.
## Set a grading scale based on percent scores and add the grading standard and score in one column
## Create a table to show the standard and score for each indicator and the overall scores based on response id

################################################################################
## Function: Create function.
################################################################################

# name the function and set the inputs
indicator_scores_function <- function(survey_scores){
  
  survey_scores = survey_scores %>% 
    # find the precent scored per question
    mutate(pct_score = score/max) %>% 
    # group each question by id and category
    group_by(id,category) %>%
    summarise(
      # add the scores up by categoy
      total_score = sum(pct_score),
      # find the number of questions in each category
      n = n()) %>% 
    # divide the total score by the number if questions in each category
    mutate(pct_category = total_score/n)
  
  # create a new data frame and add all the categories together per id
  id_scores = survey_scores %>%
    group_by(id) %>% 
    summarise(
      # divide by the number of categories to find the over all percent score 
      id_score = sum(pct_category)/4
    ) %>% 
    # convert the scores to percentages with 2 decimal places
    mutate(id_score = round((id_score*100), digits = 2)) %>% 
    mutate(id_management = case_when(
      # set grading standard based on the score
      id_score <= 40.99 ~ "Very Inferior Standard",
      id_score >= 41 & id_score < 54.99 ~ "Inferior Standard",
      id_score >= 55 & id_score < 69.99 ~ "Average Standard",
      id_score >= 70 & id_score < 84.99 ~ "High Standard",
      id_score >= 85 ~ "Standard of Excellence")) %>% 
    # unite the grading standard and the score into one column
    unite(overall_score, c(id_management, id_score), sep = " ", remove = FALSE) %>% 
    # remove all other columns other than id and overall scores
    select(id, overall_score)
  
  survey_scores = survey_scores %>%
    # convert the scores to percentages with 2 decimal places
    mutate(pct_category = round((pct_category*100), digits = 2)) %>% 
    mutate(management = case_when(
      # set grading standard based on the score
      pct_category <= 40.99 ~ "Very Inferior Standard",
      pct_category >= 41 & pct_category < 54.99 ~ "Inferior Standard",
      pct_category >= 55 & pct_category < 69.99 ~ "Average Standard",
      pct_category >= 70 & pct_category < 84.99 ~ "High Standard",
      pct_category >= 85 ~ "Standard of Excellence")) %>% 
    # unite the grading standard and the score into one column
    unite(management_score, c(management, pct_category), sep = " ", remove = FALSE) %>% 
    # remove all other columns other than id and overall scores
    select(id, category, management_score) %>% 
    # create a column for each indicator
    pivot_wider(names_from = category, values_from = management_score)
  
  # merge the two data sets by id
  survey_scores = merge(survey_scores, id_scores,by = "id") %>%  
    # change the names 
    rename("Response ID" = id,
           "Ecological" = ecological,
           "Economic" = economical,
           "Institution" = instatution,
           "Social" = social,
           "Overall" = overall_score)
  
  # create a table of results using formattable
  scores_table = formattable(survey_scores, align =c("l","c","c","c","c", "r"), list(
    `Responds ID` = formatter("span", style = ~ style(color = "grey",font.weight = "bold"))
  ))
  
  return(scores_table)
  
}

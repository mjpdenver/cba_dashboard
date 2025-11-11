# Generate a summary of all events in the dataset
library(dplyr)

source('R/read_file.R')

cat("=== CBA Competition Data Summary ===\n\n")
cat("Note: Invalid school names (starting with 'CBA' or 'Class') are automatically filtered.\n\n")

# Summary by Event Type
cat("1. Event Types:\n")
type_summary <- scores_labeled_df %>%
    group_by(Event_Type) %>%
    summarise(
        Schools = n(),
        Competitions = n_distinct(Competition_Name)
    ) %>%
    arrange(desc(Schools))
print(type_summary)

cat("\n2. Event Rounds:\n")
round_summary <- scores_labeled_df %>%
    filter(!is.na(Event_Round)) %>%
    group_by(Event_Round) %>%
    summarise(
        Schools = n(),
        Competitions = n_distinct(Competition_Name)
    ) %>%
    arrange(desc(Schools))
print(round_summary)

cat("\n3. Invitational Events (with Prelims/Finals):\n")
invitational_summary <- scores_labeled_df %>%
    filter(Event_Type == "Invitational") %>%
    group_by(Competition_Name, Event_Round) %>%
    summarise(
        Date = first(Event_Date),
        Schools = n(),
        .groups = "drop"
    ) %>%
    arrange(Date, Competition_Name, Event_Round)
print(invitational_summary, n = 30)

cat("\n4. Regional Events:\n")
regional_summary <- scores_labeled_df %>%
    filter(Event_Type == "Regional") %>%
    group_by(Competition_Name) %>%
    summarise(
        Date = first(Event_Date),
        Schools = n(),
        .groups = "drop"
    ) %>%
    arrange(Date)
print(regional_summary)

cat("\n5. State Championship Events:\n")
state_summary <- scores_labeled_df %>%
    filter(Event_Type == "State") %>%
    group_by(Competition_Name, Event_Round) %>%
    summarise(
        Date = first(Event_Date),
        Class = first(Event_Class),
        Schools = n(),
        .groups = "drop"
    ) %>%
    arrange(Date, Event_Round)
print(state_summary, n = Inf)

source('R/read_file.R')

cat('Total schools extracted:', nrow(scores_labeled_df), '\n')
cat('Total columns:', ncol(scores_labeled_df), '\n\n')

cat('Column names:\n')
print(names(scores_labeled_df))

cat('\n\nTest.pdf schools with sample values:\n')
test_schools <- scores_labeled_df[scores_labeled_df$File == 'test.pdf', ]
print(test_schools[, c('School', 'Event_Class', 'Music_Ind_Tot', 'Music_Total',
                       'Visual_Total', 'Sub_Total', 'Weighted_Total', 'Final_Total')])

cat('\n\nAll files summary:\n')
print(table(scores_labeled_df$File))

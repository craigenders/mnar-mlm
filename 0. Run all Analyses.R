fdir::set()

library(rblimp)

# RUN V4 ----

# set_blimp('/applications/blimp/blimp')

# source('1. Analysis Growth Dropout.R')

# write.csv(tab_growth_do,file = '~/desktop/MNAR Results/tab_growth_do.csv')
# write.csv(es_growth_do,file = '~/desktop/MNAR Results/es_growth_do.csv')
# write.csv(conv_growth_do,file = '~/desktop/MNAR Results/conv_growth_do.csv')

# source('2. Analysis Growth Intermittent.R')

# write.csv(tab_growth_im,file = '~/desktop/MNAR Results/tab_growth_im.csv')
# write.csv(es_growth_im,file = '~/desktop/MNAR Results/es_growth_im.csv')
# write.csv(conv_growth_im,file = '~/desktop/MNAR Results/conv_growth_im.csv')

# source('3. Analysis Intensive Dropout.R')

# write.csv(tab_intensive_do,file = '~/desktop/MNAR Results/tab_intensive_do.csv')
# write.csv(es_intensive_do,file = '~/desktop/MNAR Results/es_intensive_do.csv')
# write.csv(conv_intensive_do,file = '~/desktop/MNAR Results/conv_intensive_do.csv')

# source('4. Analysis Intensive Intermittent.R')

# write.csv(tab_intensive_im,file = '~/desktop/MNAR Results/tab_intensive_im.csv')
# write.csv(es_intensive_im,file = '~/desktop/MNAR Results/es_intensive_im.csv')
# write.csv(conv_intensive_im,file = '~/desktop/MNAR Results/conv_intensive_im.csv')

# RUN NIGHTLY ----

set_blimp('/applications/blimp/blimp-nightly')

source('1. Analysis Growth Dropout.R')

write.csv(tab_growth_do,file = '~/desktop/MNAR Results/tab_growth_do_ni.csv')
write.csv(es_growth_do,file = '~/desktop/MNAR Results/es_growth_do_ni.csv')
write.csv(conv_growth_do,file = '~/desktop/MNAR Results/conv_growth_do_ni.csv')

source('2. Analysis Growth Intermittent.R')

write.csv(tab_growth_im,file = '~/desktop/MNAR Results/tab_growth_im_ni.csv')
write.csv(es_growth_im,file = '~/desktop/MNAR Results/es_growth_im_ni.csv')
write.csv(conv_growth_im,file = '~/desktop/MNAR Results/conv_growth_im_ni.csv')

source('3. Analysis Intensive Dropout.R')

write.csv(tab_intensive_do,file = '~/desktop/MNAR Results/tab_intensive_do_ni.csv')
write.csv(es_intensive_do,file = '~/desktop/MNAR Results/es_intensive_do_ni.csv')
write.csv(conv_intensive_do,file = '~/desktop/MNAR Results/conv_intensive_do_ni.csv')

source('4. Analysis Intensive Intermittent.R')

write.csv(tab_intensive_im,file = '~/desktop/MNAR Results/tab_intensive_im_ni.csv')
write.csv(es_intensive_im,file = '~/desktop/MNAR Results/es_intensive_im_ni.csv')
write.csv(conv_intensive_im,file = '~/desktop/MNAR Results/conv_intensive_im_ni.csv')

# RUN DROPOUT COMMAND ----

set_blimp('/applications/blimp/blimp-nightly')

source('1. Analysis Growth Dropout FIN.R', echo = TRUE)

write.csv(table_summary,file = '~/desktop/MNAR Results/tab_growth_do_fi.csv')
write.csv(table_diff,file = '~/desktop/MNAR Results/es_growth_do_fi.csv')
write.csv(table_diag,file = '~/desktop/MNAR Results/conv_growth_do_fi.csv')

source('2. Analysis Growth Intermittent FIN.R', echo = TRUE)

write.csv(table_summary,file = '~/desktop/MNAR Results/tab_growth_im_fi.csv')
write.csv(table_diff,file = '~/desktop/MNAR Results/es_growth_im_fi.csv')
write.csv(table_diag,file = '~/desktop/MNAR Results/conv_growth_im_fi.csv')

source('3. Analysis Intensive Dropout FIN.R', echo = TRUE)

write.csv(table_summary,file = '~/desktop/MNAR Results/tab_intensive_do_fi.csv')
write.csv(table_diff,file = '~/desktop/MNAR Results/es_intensive_do_fi.csv')
write.csv(table_diag,file = '~/desktop/MNAR Results/conv_intensive_do_fi.csv')

source('4. Analysis Intensive Intermittent FIN.R', echo = TRUE)

write.csv(table_summary,file = '~/desktop/MNAR Results/tab_intensive_im_fi.csv')
write.csv(table_diff,file = '~/desktop/MNAR Results/es_intensive_im_fi.csv')
write.csv(table_diag,file = '~/desktop/MNAR Results/conv_intensive_im_fi.csv')


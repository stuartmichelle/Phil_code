# a script to determine how many anemones visited and breeding pairs

source("conleyte.R")
leyte <- conleyte()

# how many tagged anemones were observed in 2016?
dive <- leyte %>% tbl("diveinfo") %>% filter(date > "2016-01-01" % date < "2017-01-01")
anem <- leyte %>% tbl("anemones")  %>% filter

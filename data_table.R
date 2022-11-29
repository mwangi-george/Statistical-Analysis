x <- data.frame(id= 1:2,
           name= c("a", "b"))
class(x$name)

library(data.table)
y <- data.table(
  id= 1:2,
  name= c("a", "b")
)

class(y$name)
y
rownames(y)

row.names(y) <- c("x","l")
y

class(economics)

library(magrittr)

economics %<>%
  as.data.table(economics)

economics[!(1:3)]

economics[.N]

economics[1:(.N-10)]
economics[pop >= 100000 | unemploy < 4.5] %>% view()


dt <- data.table(
  x = sample(10000, 10e6, T),
  y = sample(letters, 10e6, T)
)
indices(dt)
system.time(dt[x == 900])

indices(dt)
system.time(dt[x == 900])

dt[.N]
nrow(dt)


economics

# import the data as a data.table for faster processing
jobs <- as.data.table(
  # read_csv is clever for data types and white spaces are removed
  read_csv("datasets/mobile_money_data.csv",
    # dont show column types
    show_col_types = F
  ) %>%
    # give every row an id
    rowid_to_column() %>%
    # clean row names
    clean_names()
)

jobs %>% 
  view()
class(jobs)

jobs[
  account_type %like% "bile"
] %>% view()

jobs[
  rowid %between% c(50, 200)
] %>% view()

jobs %>% 
  filter(between(rowid, 50, 200))

jobs[
  account_type %chin% c(
    "VSLA Account", "Mobile Money"
  )
] %>% view()











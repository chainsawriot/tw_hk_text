require(quanteda)

x <- readLines('chinese.txt')

x[5]
tokens(x[5])

dfm(tokens(x))

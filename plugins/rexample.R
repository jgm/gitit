#!/usr/bin/Rscript

# Example of how to write an external PageTransform plugin in R.

cat('Hello from R!')
cat('<br/>')

cat('Your script recieved these args:')
cat('<br/>')
cat(commandArgs(trailingOnly=TRUE))
cat('<br/>')

cat('And this text from stdin:')
cat('<br/>')
cat(readLines('stdin'))
cat('<br/>')
cat('<br/>')

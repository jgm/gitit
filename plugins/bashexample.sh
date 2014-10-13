#!/bin/bash

# Example of how to write an external PageTransform plugin in Bash.

echo 'Hello from Bash!'
echo '<br/>'
echo 'Your script recieved these args:'
echo '<br/>'
echo "$@"
echo '<br/>'
echo 'And this text from stdin'
echo '<br/>'
while read line; do
  echo $line
done

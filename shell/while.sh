#!/bin/bash

answer="yes"

while [ $answer = "yes" ]

do 
   echo "what is your word"
   read word
   echo $word
   echo "would you like to enter another word?"
   read answer

done

if [ answer =  "no" ]; 
  then  echo "Good job"
fi

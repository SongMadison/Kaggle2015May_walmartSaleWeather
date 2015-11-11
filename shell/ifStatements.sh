#!/bin/bash

echo "What is your age"

read age

if [ $age -lt 21 ];
# there must be a semicolon after condition sentense

 then 
  echo "you are undrage"
else 
  echo "you are legal!"
fi

exit 0

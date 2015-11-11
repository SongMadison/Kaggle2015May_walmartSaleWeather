#!/bin/bash

myvar=6
echo $myvar

let  myvar+=1 # use let or ((   ))
echo $myvar

z=6
((z+=5))
echo $z

exit 0

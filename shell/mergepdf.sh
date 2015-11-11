#!/bin/bash
cd itemplots
a="item_1.pdf"
b=$a
for(( i=2;i<112;i++)) do
    a="item_"$i".pdf"
    echo $a
    
    b="$b item_$i.pdf"
    #convert $a merged.pdf merged.pdf
done

convert $b merged.pdf

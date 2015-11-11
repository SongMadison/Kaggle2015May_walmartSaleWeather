
#!/bin/bash
cd storeplots
a="store_1.pdf"
b=$a
for(( i=2;i<45;i++)) do
    a="store_"$i".pdf"
    echo $a

    b="$b store_$i.pdf"
    #convert $a merged.pdf merged.pdf
done

echo $b
convert $b merged.pdf



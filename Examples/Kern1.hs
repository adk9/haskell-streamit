kern1 =
do x <- pop -- HOAS
        push (x+1)

s1 = ...
s2 = filt (1,1,1) kern1
        (filt (1,1,1) kern1 s1)


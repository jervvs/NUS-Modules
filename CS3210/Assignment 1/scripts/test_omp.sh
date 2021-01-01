for size in 8 100 1000 10000 35000 45000 100000
do 
    for threads in 1 2 4 8 16 32 64 128 256
    do
    printf "$type\t$size\t$threads" >> results
    perf stat ./LCS-omp DNA-$size-1.in DNA-$size-2.in $threads 2>&1 >/dev/null | grep "seconds time elapsed" >> results
    echo $type $size $threads
    done
done
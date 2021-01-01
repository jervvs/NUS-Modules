CS3210 Assignment 1 Part 1
Done by: Jervis Chan (A0134191M)

Lab Machines Used:
    Xeon: soctf-pdc-004
    i7: soctf-pdc-009

Compilation Instructions:
    gcc -o LCS-seq LCS-seq.c
    gcc -o LCS-threads -pthread LCS-threads.c
    gcc -o LCS-processes -pthread LCS-processes.c

Execution Instructions:
    ./LCS-seq <input 1> <input 2>
    ./LCS-threads <input 1> <input 2> <num_of_threads>
    ./LCS-processes <input 1> <input 2> <num_of_processes>

Measuring Execution:
    To measure the execution time, I used perf stat.
        eg. perf stat ./LCS-threads DNA-8-1.in DNA-8-2.in 4


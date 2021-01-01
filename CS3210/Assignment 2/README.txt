CS3210 Assignment 2
Done by: Jervis Chan (A0134191M)

Nodes Used:
    xgpc5 (Tesla V100 with Compute Capability of 7.0)
    xgpf5 (Tesla V4 with Compute Capability of 7.5)

Compilation Instructions:
    nvcc -arch=sm_<70/75> -rdc=true -o a2 a2.cu hash.cu

Execution Instructions:
    ./a2 <input_file>

Using nvprof for profiling:
    nvprof --log-file output-test1.p ./a2 test1.in
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include "hash.h"

// CUDA runtime
#include <cuda_runtime.h>

#define DIGEST_LENGTH 52 // Size in words
#define HASH_LENGTH 32  // Size in words

int check_cuda_errors()
{
    cudaError_t rc;
    rc = cudaGetLastError();
    if (rc != cudaSuccess)
    {
        printf("Last CUDA error %s\n", cudaGetErrorString(rc));
        return 1;
    }
    return 0;
}

long long wall_clock_time()
{
#ifdef __linux__
    struct timespec tp;
    clock_gettime(CLOCK_REALTIME, &tp);
    return (long long)(tp.tv_nsec + (long long)tp.tv_sec * 1000000000ll);
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (long long)(tv.tv_usec * 1000 + (long long)tv.tv_sec * 1000000000ll);
#endif
}

//TODO global hash
__global__
void globalHash(
    uint8_t *hash, 
    const uint8_t *X,  
    unsigned long long n, 
    int* found, 
    unsigned long long *res_nonce,
    unsigned long long offset
){
    //CONSTRUCTING INPUT
    uint8_t input[DIGEST_LENGTH];
    for (int i = 0; i < 44; i++) { 
        input[i] = X[i];
    }

    unsigned long long threadID = blockIdx.x * blockDim.x + threadIdx.x;
    unsigned long long nonce = threadID + offset;

    for (int i = 44; i < DIGEST_LENGTH; i++) {
        input[i] = (nonce >> 8 * (DIGEST_LENGTH - i - 1)) & 0xFF;
    }

    uint8_t localHash[HASH_LENGTH];
    sha256(localHash, input, DIGEST_LENGTH);

    // Check 64-bit prefix of SHA256(digest)
    unsigned long long prefix = 0x0000;
    for (int i = 0; i < 8; i++) {
        prefix = prefix | ((unsigned long long) localHash[i] << (8*(7-i)));
    }

    if (prefix < n) {
        if (atomicExch(found, 1) == 0) {
            memcpy(hash, localHash, sizeof(uint8_t) * HASH_LENGTH);
            memcpy(res_nonce, &nonce, sizeof(unsigned long long));
        }
    }
}


int main(int argc, char *argv[]) {
    //INPUT HANDLING
    if (argc != 2 ) {
        printf("Usage: [executable] [file 1]\n");
        return 1;
    }
    
    FILE* file = fopen(argv[1], "r"); /* should check the result */
  
    char *prevDigest = (char*) malloc(64+1);
    uint8_t *tid = (uint8_t*) malloc(sizeof(uint8_t));
    unsigned long long n;

    fscanf(file, "%s", prevDigest);
    fscanf(file, "%s", tid);
    fscanf(file, "%llu", &n);
    fclose(file);

    //PREV DIGEST
    uint8_t *prev = (uint8_t*) malloc(sizeof(uint8_t) * 32);
    char *buff = prevDigest;
    for (int i = 0; i<32; i++){
        sscanf(buff, "%02hhX", &prev[i]);
        buff += 2; 
    }

    //Getting UNIX Timestamp
    uint32_t timeNow = (uint32_t) time(NULL);
    uint8_t *t = (uint8_t*) malloc(sizeof(uint8_t) * 4);
    t[0] = (uint8_t) (timeNow >> 24);
    t[1] = (uint8_t) (timeNow >> 16);
    t[2] = (uint8_t) (timeNow >> 8);
    t[3] = (uint8_t) timeNow;

    //HOST DATA
    //CHANGE GRID AND BLOCK SIZES HERE
    const uint32_t block_count = 80;
	const uint32_t threads_per_block = 256;
    const uint32_t thread_count = block_count * threads_per_block;
    
    //DEVICE DATA
    uint8_t *hash;
    uint8_t *X; 
    unsigned long long *res_nonce;
    unsigned long long offset = 0;
    int *found;  

    // "Malloc" device memory
    cudaMallocManaged((void **)&hash, HASH_LENGTH * sizeof(uint8_t));
    cudaMallocManaged((void **)&X, DIGEST_LENGTH * sizeof(uint8_t));
    cudaMallocManaged(&res_nonce, sizeof(unsigned long long));
    cudaMallocManaged(&found, sizeof(int));
    *found = 0;

    //FILL X WITH THE INPUT VALUES
    int i, j;
    for (i = 0, j = 0; i < 32; i++) { 
        X[i] = prev[j];
        j++;
    }
    for (i = 32, j = 0; i < 36; i++) { 
        X[i] = t[j];
        j++;
    }
    for (i = 36, j = 0; i < 44; i++) { 
        X[i] = tid[j];
        j++;
    }

    int start = wall_clock_time();
    while (!(*found)) {
        globalHash<<<block_count, threads_per_block>>>(hash, X, n, found, res_nonce, offset);
        cudaDeviceSynchronize();
        if (check_cuda_errors()){break;}
        offset += thread_count;
    }
    int end = wall_clock_time();
    printf("The process took %1.2f seconds\n", ((float)(end - start))/1000000000);

    // OUTPUT
    printf("%d\n", timeNow);
    printf("%llu\n", *res_nonce);
    for (int i = 0; i < HASH_LENGTH; i++) {
        printf("%02x", hash[i]);
    }
    printf("\n");

    // CLEANUP
    cudaFree(hash);
    cudaFree(X);
    cudaFree(found);
    cudaFree(res_nonce);

    cudaDeviceReset();
    return 0;
}
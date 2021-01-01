#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>
#include <xmmintrin.h>
#include <unistd.h>
#include <errno.h>
#include <sys/shm.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <string.h>

#define POSSIBLE_LETTERS 4

int NUM_THREADS;
int *mat[2];
int *DP[POSSIBLE_LETTERS + 1];
int M, N;
char *seq1;
char *seq2;

void *prepareScoreTable(){
#pragma omp parallel for schedule(static,1)
    for (int i = 1; i <= N; i++) {
        int old = 0;
        int new = 1;
        char a = seq1[i-1];
        int x,y,z;
        
        if (a == 'A') x = 1;
        else if (a == 'T') x = 2;
        else if (a == 'C') x = 3;
        else if (a == 'G') x = 4;
        for (int j = 0; j<=M; j++){
            y = (0-DP[x][j]) < 0;
            z = (0 - (mat[old][j] - y * mat[old][DP[x][j]-1])) < 0;
            mat[new][j] = mat[old][j] + y * (z^1);
        }
            // Swap the previous and current row
        old = old ^ new;
        new = new ^ old;
        old = old ^ new;
    }
}

int main(int argc, char *argv[]) {
    // Accepts filepaths to two input DNA sequences as command-line arguments
    if (argc != 4) {
        printf("Usage: [executable] [file 1] [file 2] [NUM_THREADS]\n");
        return 1;
    }

    NUM_THREADS = atoi(argv[3]);

    // Validate both filepaths
    FILE *file1 = fopen(argv[1], "r");
    FILE *file2 = fopen(argv[2], "r");
    if (!file1 || !file2) {
        printf("Input files are not found!\n");
        return 1;
    }

    fscanf(file1, "%d", &M);
    fscanf(file2, "%d", &N);

    // Read two input sequences
    seq1 = (char*) malloc(M + 1);
    seq2 = (char*) malloc(N + 1);
    fscanf(file1, "%s", seq1);
    fscanf(file2, "%s", seq2);
    fclose(file1);
    fclose(file2);

    //We do this because of the 1-based index
    char A[POSSIBLE_LETTERS + 1] = "BATGC";

    for (int i = 0; i<= POSSIBLE_LETTERS; i++){
        DP[i] = calloc(M+1, sizeof(int));
    }

    for (int i = 0; i<= POSSIBLE_LETTERS; i++){
        for (int j = 0; i<=M; j++){
            if (j == 0) continue;
            if (seq2[j-1] == seq1[i]){
                DP[i][j] = j;
            } else{
                DP[i][j] = DP[i][j-1];
            }
        }
    }
    
    mat[0] = calloc(M+1, sizeof(int));
    mat[1] = malloc((M+1) * sizeof(int));
    mat[1][0] = 0;
    
    omp_set_num_threads(NUM_THREADS);

    //split work
    prepareScoreTable();

    if (N % 2 == 0){
        printf("%d\n", mat[0][M]);
    } else
    {
        printf("%d\n", mat[1][M]);
    }

    free(seq1);
    free(seq2);
    free(mat[0]);
    free(mat[1]);

    for (int i = 0; i < POSSIBLE_LETTERS + 1; i++) {
        free(DP[i]);
    }
}


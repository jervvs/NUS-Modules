#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <sys/shm.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <semaphore.h>
#include <fcntl.h>
#include <string.h>

#define POSSIBLE_LETTERS 4

typedef struct thread_info{
    pthread_t tid;
    int id;
    int last;
} ThreadInfo;

pthread_barrier_t *barr;
int NUM_THREADS;
int *mat[2];
int *DP[POSSIBLE_LETTERS + 1];
int thread_split;
int remaining;
int M, N;
char *seq1;
char *seq2;

void *threadCompute(void *thread_info);

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

    //Implement barrier
    barr = malloc(sizeof(pthread_barrier_t));
    if (barr == (void *) -1){
        printf("Error allocating mem for barrier.\n");
        exit(1);
    }

    int err = pthread_barrier_init(barr, NULL, NUM_THREADS);
    if (err){
        printf("Failed to init barrier\n");
        exit(err);
    }

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

    //split work
    thread_split = M/NUM_THREADS;
    remaining = M-(thread_split + NUM_THREADS);

    //initiating thread info for each thread
    int last = 0;
    ThreadInfo child_thread[NUM_THREADS];
    for (int i = 0; i < NUM_THREADS; i++){
        if (i == NUM_THREADS -1){
            last = 1;
        }

        child_thread[i].id = i;
        child_thread[i].last = last;
        pthread_create(&(child_thread[i].tid), NULL, threadCompute, (void *)(&child_thread[i]));
    }

    for (int i = 0; i < NUM_THREADS; i++){
        pthread_join(child_thread[i].tid, NULL);
    }

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
    pthread_barrier_destroy(barr);
}

void *threadCompute(void *thread_info){
    ThreadInfo *local_info = (ThreadInfo *)thread_info;
    int last = local_info->last;
    int id = local_info->id;

    int starting_col = thread_split * id + 1;
    int ending_col = starting_col + thread_split - 1;
    if (last){
        ending_col = ending_col + remaining;
    }

    int old = 0;
    int new = 1;
    for (int i = 1; i <= N; i++) {
        //Use barrier to sync between rows
        int p_err = pthread_barrier_wait(barr);
        if (p_err != 0 && p_err != PTHREAD_BARRIER_SERIAL_THREAD){
            printf("Error in barrier\n");
        }

        char a = seq1[i-1];
        int x,y,z;

        if (a == 'A') x = 1;
        else if (a == 'T') x = 2;
        else if (a == 'C') x = 3;
        else if (a == 'G') x = 4;
        for (int j = starting_col; j<=ending_col; j++){
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
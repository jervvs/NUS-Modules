#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/shm.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <semaphore.h>
#include <fcntl.h>
#include <pthread.h>
#include <string.h>
#include <errno.h>

#define POSSIBLE_LETTERS 4

int main(int argc, char *argv[]) {
    // Accepts filepaths to two input DNA sequences as command-line arguments
    if (argc != 4) {
        printf("Usage: [executable] [file 1] [file 2] [NUM_PROCESSES]\n");
        return 1;
    }

    int NUM_PROCESSES = atoi(argv[3]);

    // Validate both filepaths
    FILE *file1 = fopen(argv[1], "r");
    FILE *file2 = fopen(argv[2], "r");
    if (!file1 || !file2) {
        printf("Input files are not found!\n");
        return 1;
    }

    int M, N;
    fscanf(file1, "%d", &M);
    fscanf(file2, "%d", &N);

    // Read two input sequences
    char *seq1;
    char *seq2;
    seq1 = (char*) malloc(M + 1);
    seq2 = (char*) malloc(N + 1);
    fscanf(file1, "%s", seq1);
    fscanf(file2, "%s", seq2);
    fclose(file1);
    fclose(file2);

    //Implement barrier
    int mat_key = ftok("/dev/null", 10);
    int P_key = ftok("/dev/null", 11);
    int barrier_key = ftok("/dev/null", 23);
    int mat_id = shmget(mat_key, sizeof(int[2][M + 1]), IPC_CREAT | 0644);
    int P_id = shmget(P_key, sizeof(int[POSSIBLE_LETTERS + 1][M + 1]), IPC_CREAT | 0644);
    int barrier_id = shmget(barrier_key, sizeof(pthread_barrier_t), IPC_CREAT | 0644);
    if (barrier_id == -1) {
        printf("Error getting SHM\n");
        exit(1);
    }

    int (*mat)[M + 1] = shmat(mat_id, NULL, 0);
    if (mat == (void *) -1) {
        printf("Error on attaching mat SHM\n");
        exit(1);
    }
    int (*DP)[M + 1] = shmat(P_id, NULL, 0);
    if (DP == (void *) -1) {
        printf("Error attaching DP SHM\n");
        exit(1);
    }
    pthread_barrier_t *barr = shmat(barrier_id, NULL, 0);
    if (barr == (void *) -1) {
        printf("Error attaching barrier SHM\n");
        exit(1);
    }
    pthread_barrierattr_t *barrier_attr = malloc(sizeof(pthread_barrierattr_t *));
    
    pthread_barrierattr_init(barrier_attr);
    pthread_barrierattr_setpshared(barrier_attr, PTHREAD_PROCESS_SHARED);
    int err = pthread_barrier_init(barr, barrier_attr, NUM_PROCESSES);
    if (err) {
        printf("Failed to init barrier\n");
        exit(err);
    }

    //We do this because of the 1-based index
    char A[POSSIBLE_LETTERS + 1] = "BATGC";

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
    
    for (int j = 0; j < M+1; j++){
        mat[0][j] = 0;
    }
    mat[1][0] = 0;

    //split work
    int process_split = M/NUM_PROCESSES;
    int remaining = M-(process_split + NUM_PROCESSES);

    int pid;
    
    // ID for each process/thread. Just to determine what cols it should handle
    int id = 0;
    int last = 0;
    for (int i = 0; i < NUM_PROCESSES; i++) {
        if (i == NUM_PROCESSES - 1) {
            last = 1;
        }
        pid = fork();
        if (pid == -1){
            printf("Error in fork\n");
        } 
        if (pid == 0) break;
        id++;
    }

    if (pid != 0) {
        // Parent Process
        while (pid = waitpid(-1, NULL, 0)) {
            if (errno == ECHILD) break;
        }
        
        if (N % 2 == 0) {
            printf("%d", mat[0][M]);
        } else {
            printf("%d", mat[1][M]);
        }

        free(seq1);
        free(seq2);
        shmdt(mat);
        shmdt(DP);
        shmctl(mat_id, IPC_RMID, 0);
        shmctl(P_id, IPC_RMID, 0);
        pthread_barrierattr_destroy(barrier_attr);
        pthread_barrier_destroy(barr);

    } else {
        // Child process
        int starting_col = process_split * id + 1;
        int ending_col = starting_col + process_split - 1;
        if (last){
            ending_col = ending_col + remaining;
        }
        int old = 0;
        int new = 1;

        for (int i = 1; i <= N; i++) {
            //Use barrier to sync between rows
            int p_err = pthread_barrier_wait(barr);
            if (p_err != 0 && p_err != PTHREAD_BARRIER_SERIAL_THREAD) {
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
}

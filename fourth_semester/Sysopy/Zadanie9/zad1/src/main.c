#include "main.h"

char v = 0;
int writer_num;
int writer_turns;
int reader_num;
int reader_turns;
int readers_waiting;
int memory_id;
int *memory;
sem_t *res_write;
sem_t *res_read;
pthread_t *reader_ids;
pthread_t *writer_ids;
pthread_barrier_t mybarrier;

void cleanup(){
	CREATE_SEMAPHORE(res_read, 		RES_READ)
	CREATE_SEMAPHORE(res_write,   RES_WRITE)
	create_shared_memory(&memory, SHM_NAME);
	DELETE_SHARED_MEM(memory, SHM_NAME)
	DELETE_SEMAPHORE(res_read, RES_READ)
	DELETE_SEMAPHORE(res_write, RES_WRITE)
	println("Cleaned")
}

int main(int argc, char **argv){
	srand((unsigned) time(NULL));
	cleanup();

	CHECK_NEGATIVE("main", argc-5, "Usage: ./main writers_num writer_turns readers_num reader_turns [-i]")
	CHECK_NEGATIVE("main", 6-argc, "Usage: ./main writers_num writer_turns readers_num reader_turns [-i]")
	v = argc == 6;
	setup(argv);

	for(int i=0; i<reader_num; i++){
		pthread_join(reader_ids[i], NULL);
	}

	for(int i=0; i<writer_num; i++){
		pthread_join(writer_ids[i], NULL);
	}

	teardown();
	return 0;
}

void setup(char **argv){
	writer_num 		= 	atoi(argv[1]);
	writer_turns 	= 	atoi(argv[2]);
	reader_num 		= 	atoi(argv[3]);
	reader_turns 	= 	atoi(argv[4]);

	readers_waiting = 0;

	CREATE_SEMAPHORE(res_read, 		RES_READ)
	CREATE_SEMAPHORE(res_write,   RES_WRITE)
	memory_id = create_shared_memory(&memory, SHM_NAME);
	reader_ids = calloc(reader_num, sizeof(pthread_t));
	writer_ids = calloc(writer_num, sizeof(pthread_t));

  pthread_barrier_init(&mybarrier, NULL, writer_num + reader_num);

	for(int i=0; i<writer_num; i++){
		pthread_create(writer_ids + i, NULL, writer, NULL);
	}
	for(int i=0; i<reader_num; i++){
		pthread_create(reader_ids + i, NULL, reader, NULL);
	}
}

void *writer(void* arg) {
	pthread_barrier_wait(&mybarrier);
	for (int i = 0; i < writer_turns; i++) {
		prolaag(res_write);

		int modified = 1 + rand() % SENSIBLE_VALUE;
    for(int j=0; j<modified; j++){
			int index = rand() % ARRAY_LENGTH, value = rand();

			if(v) print("Writer writing %d at %d overwriting %d", value, index, memory[index])

			memory[index] = value;
		}
		print("Writer %ld wrote %d values", pthread_self(), modified)

		verhoog(res_write);

    usleep(100 + rand() % 500);
  }

  return 0;
}

void *reader(void* data) {
	pthread_barrier_wait(&mybarrier);
	int checker = rand() % SENSIBLE_VALUE;

  for (int i = 0; i < reader_turns; i++) {
		prolaag(res_read);
		readers_waiting++;
		if(readers_waiting == 1){
			prolaag(res_write);
		}
		verhoog(res_read);
		int correct = 0;
		for(int j=0; j<ARRAY_LENGTH; j++){
			if(memory[j] % checker == 0){
				 correct++;
				 if(v){
					 print("%ld has found %d which is divisible by %d at %d", pthread_self(), memory[j], checker, j)
				 }
			 }
		}
		print("%ld reader found %d numbers divisible by %d", pthread_self(), correct, checker)

		prolaag(res_read);
		readers_waiting--;
		if(readers_waiting == 0){
			verhoog(res_write);
		}
		verhoog(res_read);

    usleep(100+rand()%300);
  }

  return 0;
}

void teardown(){
	DELETE_SHARED_MEM(memory, SHM_NAME)
	DELETE_SEMAPHORE(res_read, RES_READ)
	DELETE_SEMAPHORE(res_write, RES_WRITE)
	free(reader_ids);
	free(writer_ids);
}

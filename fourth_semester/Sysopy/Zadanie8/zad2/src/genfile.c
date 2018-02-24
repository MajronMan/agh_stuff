#include "common.h"
#include "genfile.h"

char* rand_string(int len, char *acc) {
  if (len == 0) {
    return acc;
  }
  char appended[2];
  appended[0] = possible[rand() % possible_length];
  appended[1] = 0;
  strcat(acc, appended);
  return rand_string(len - 1, acc);
};

char *rand_record_text(int id, char *acc){
  char id_str[11];
  sprintf(id_str, "%-10d", id); //INT_MAX is 10 chars long
  strcat(acc, id_str);
  return rand_string(text_length(strlen(id_str)), acc);
}

char *rand_record_text_with_word(int id, char * word, char *acc){
  char id_str[11];
  sprintf(id_str, "%-10d", id); //INT_MAX is 10 chars long
  strcat(acc, id_str);

  int wlen = strlen(word), left = text_length(strlen(id_str) + wlen);
  int start = rand() % left;
  rand_string(start, acc);
  strcat(acc, word);
  return rand_string(left-start, acc);
}

void gen_file(char *file_name, char* word, int records){
  FILE *f = fopen(file_name, "w");
  int special = rand() % records;
  for(int i=0; i<records; i++) {
    char acc[RECORD_SIZE] = {};
    if(i == special) {
      rand_record_text_with_word(i, word, acc);
    }
    else {
      rand_record_text(i, acc);
    }
    fwrite(acc, sizeof(char), RECORD_SIZE, f);
  }
  fclose(f);
}

int main(int argc, char **argv){
  CHECK_UNEQUAL("main", argc, 4, "Usage: ./genfile filename secret_word records_count")

  char *filename = argv[1], *word = argv[2];
  int records_count = atoi(argv[3]);

  gen_file(filename, word, records_count);

  return 0;
}

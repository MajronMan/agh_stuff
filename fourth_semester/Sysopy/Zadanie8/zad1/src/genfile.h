#ifndef GENFILE_H
#define GENFILE_H

const char* possible =
  "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
const int possible_length = 62;
char *rand_string(int len, char *acc);
char *rand_record_text(int id, char *acc);
char *rand_record_text_with_word(int id, char * word, char *acc);
void gen_file(char *file_name, char* word, int records);



#endif /* end of include guard: GENFILE_H */

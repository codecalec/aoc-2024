#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int getvalue(char* data, size_t start, size_t end) {
    /* char buffer[100]; */
    /*  */
    /* if (end - start > 100) { */
    /*     fprintf(stderr, "input too long to be valid. returning 0"); */
    /*     return 0; */
    /* } */

    /* strncpy(buffer, &data[start], end - start); */
    /* printf("%s\n", buffer); */

    /* if (strcmp(sub, "mul(") == 0) { */

    char* ptr = &data[start+4];
    char* result = strchr(ptr, ',');

    char* numptr;
    const int value1 = strtol(&data[start+4], &numptr, 10);
    if (value1 == 0) return 0;

    const int value2 = strtol(numptr+sizeof(char), &numptr, 10);
    if (value2 == 0) return 0;

    if (*numptr != ')') return 0;

    return value1 * value2;

}

int process(char* data, size_t length) {
    int total = 0;

    int domul = 1;

    for (size_t i = 0; i < length-6; i++) {

        if (strncmp("do()", &data[i], 4) == 0) {
            domul = 1;
        } else if (strncmp("don't()", &data[i], 6) == 0) {
            domul = 0;
        }


        if (domul && strncmp("mul(", &data[i], 4) == 0) {
            char* result = strchr(&data[i], ')');
            int length = result - &data[i];

            total += getvalue(data, i, i + length + 1);
        }
    }

    printf("total: %d", total);
}

int main() {
    char* source = NULL;
    size_t bufsize;

    FILE *fp = fopen("input", "r");

    if (fp != NULL) {
        /* Go to the end of the file. */
        if (fseek(fp, 0L, SEEK_END) == 0) {
            /* Get the size of the file. */
            bufsize = ftell(fp);
            if (bufsize == -1) { /* Error */ }

            /* Allocate our buffer to that size. */
            source = malloc(sizeof(char) * (bufsize + 1));

            /* Go back to the start of the file. */
            if (fseek(fp, 0L, SEEK_SET) != 0) { /* Error */ }

            /* Read the entire file into memory. */
            size_t newLen = fread(source, sizeof(char), bufsize, fp);
            if ( ferror( fp ) != 0 ) {
                fputs("Error reading file", stderr);
            }
        }

        process(source, bufsize);

        fclose(fp);
    }

    free(source); /* Don't forget to call free() later! */
}


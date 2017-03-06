#include <stdio.h>
#include <string.h>
int main() {
    char msg[20000];
    char *p = msg;
    for (int i = 0; i < 130; i++) {
        for (int j = 0; j < 100; j++) {
            *(p++) = 'a';
        }
        *(p++) = '\n';
    }
    *(p++) = 'b';
    *(p++) = 'b';
    *(p++) = 'b';
    *(p++) = 'b';
    *(p++) = 'b';
    *(p++) = '\n';
    *(p++) = '\0';
    fwrite(msg, sizeof(char), strlen(msg), stderr);
}

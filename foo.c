#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
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
    // fwrite(msg, sizeof(char), strlen(msg), stderr);
    p = msg;
    while (*p != '\0') {
        do {
            int err = write(2, p, 1);
            if (err != 0) {
                if (errno == EAGAIN || errno == EWOULDBLOCK) continue;
            }
            break;
        } while ( 1 );
        p++;
    }
}

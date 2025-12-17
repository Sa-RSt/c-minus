void main(void) {
    int x;
    x = 10;
    x = x @ 2;   /* '@' é caractere inválido em Cminus — lexer deve sinalizar erro */
}

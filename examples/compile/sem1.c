/* Teste de escopo, blocos e controle de fluxo */
void main(void) {
    int x;
    int y;
    x = 10;
    y = 0;

    if (x < 20) {
        int y;      /* variável local que "sombra" a global — pode ser proibida ou permitida */
        y = 5;
        x = x + y;  /* usa y local */
    }

    while (x < 40) {
        int x;      /* novo escopo interno */
        x = 1;
        y = y + x;  /* aqui y é o de fora; x é o local */
        /* fim do escopo interno: x local desaparece */
    }

    return;
}

void main(void) {
    int x;
    x = 5;

    if (x > 4) {
        int y;
        y = x + 1;
    }

    x = y + 2;    /* ERRO: y não existe fora do bloco */
    return;
}

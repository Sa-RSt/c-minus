/* Teste de lexer: tokens, comentários e whitespace (C-Minus do Louden) */
void main(void) {
    /* declaração de variáveis */
    int x;
    int y;
    int iff;
    int whilee;

    iff = 10 + x;    /* 'iff' NÃO é keyword, deve ser identificador */
    whilee = iff + 1;
}

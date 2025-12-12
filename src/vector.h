#pragma once

#include "stringify.h"
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/*
 * Template para tipo de dado "Vector".
 * A macro DECLARE_VECTOR_TYPE, dado um tipo de dado para os elementos do vetor,
 * um tipo de dado para a chave de busca, uma função/macro que retorna a chave
 * de um dado elemento e uma função/macro que retorna zero se duas chaves forem
 * iguais, cria uma struct de nome "Vector_TIPO", substituindo TIPO pelo tipo
 * do elemento, com várias funções prontas.
 *
 * A estrutura tem 3 atributos:
 * - size: Número de elementos no vetor.
 * - capacity: Número de elementos que o espaço de memória pode comportar.
 * - collection: Ponteiro para o espaço de memória do vetor.
 * No entanto, o acesso aos atributos é CONTRAINDICADO. Para obter o tamanho
 * e capacidade pode usar, respectivamente, os métodos vecLength_TIPO e
 * vecCapacity_TIPO. Para acessar collection diretamente, use vecTake_TIPO,
 * com atenção à. semântica dessa função.
 *
 * ## [ Referência ] ##
 *
 * Vector_TIPO vecCreateEmpty_TIPO();
 * Cria e retorna um vetor vazio. Nunca crie uma estrutura "Vector"
 * manualmente, use sempre uma das funções que criam "Vector"s.
 *
 * size_t vecLength_TIPO(Vector_TIPO *vec);
 * Retorna a quantidade de elementos atualmente armazenados no vetor.
 *
 * size_t vecCapacity_TIPO(Vector_TIPO *vec);
 * Retorna o número de elementos que o espaço de memória pode armazenar.
 *
 * void vecGrow_TIPO(Vector_TIPO *vec, size_t new_capacity);
 * Possivelmente realoca o espaço de memória do vetor para que tenha,
 * no mínimo, a capacidade especificada.
 *
 * void vecInsert_TIPO(Vector_TIPO *vec, TIPO item, size_t index);
 * Insere um item na dada posição.
 *
 * TIPO vecDelete_TIPO(Vector_TIPO *vec, size_t index);
 * Remove o item da dada posição do vetor. Essa função retorna
 * o item que foi retirado do vetor.
 *
 * void vecPushRight_TIPO(Vector_TIPO *vec, TIPO item);
 * Insere um item no final do vetor.
 *
 * void vecPushLeft_TIPO(Vector_TIPO *vec, TIPO item);
 * Insere um item no início do vetor.
 *
 * TIPO vecPopRight_TIPO(Vector_TIPO *vec);
 * Remove e retorna o último elemento do vetor.
 *
 * TIPO vecPopLeft_TIPO(Vector_TIPO *vec);
 * Remove e retorna o primeiro elemento do vetor.
 *
 * TIPO *vecIndex_TIPO(Vector_TIPO *vec, size_t index);
 * Retorna um pointeiro para o elemento da dada posição,
 * contada a partir de zero.
 *
 * TIPO *vecSearch_TIPO(Vector_TIPO *vec, K key, size_t start, int64_t step);
 * Busca e retorna um ponteiro à primeira ocorrência encontrada de um item com
 * chave igual (considerando a definição de igualdade fornecida na declaração
 * do tipo de vetor) à fornecida. A busca começa no índice `start` e tem passo
 * `step`. Retorna NULL caso o item não tenha sido encontrado. Não é recomendado
 * usar essa função diretamente; use vecSearchLeft, vecSearchRight, vecLookup ou
 * vecKey.
 *
 * TIPO *vecSearchLeft_TIPO(Vector_TIPO *vec, K key);
 * Busca e retorna um ponteiro à primeira ocorrência de item com chave igual
 * à fornecida (v. vecSearch_TIPO), varrendo o vetor da esquerda para a
 * direita. Retorna NULL caso o item não tenha sido encontrado.
 *
 * TIPO *vecSearchRight_TIPO(Vector_TIPO *vec, K key);
 * Busca e retorna um ponteiro à primeira ocorrência de item com chave igual
 * à fornecida (v. vecSearch_TIPO), varrendo o vetor da direita para a
 * esquerda. Retorna NULL caso o item não tenha sido encontrado.
 *
 * TIPO *vecLookup_TIPO(Vector_TIPO *vec, K key);
 * Equivalente a vecSearchRight.
 *
 * TIPO *vecKey_TIPO(Vector_TIPO *vec, K key);
 * Equivalente a vecLookup, porém, em vez de retornar NULL caso o item não
 * seja encontrado, interrompe o programa com uma mensagem de erro.
 *
 * bool vecRemove_TIPO(Vector_TIPO *vec, K key, size_t start, int64_t step, T
 * *out);
 * Busca, remove e retorna, por meio do ponteiro "out" especificado,
 * a primeira ocorrência encontrada de um item com chave igual à fornecida (v.
 * vecSearch_TIPO). Retorna true se o item foi removido e false caso não tenha
 * sido encontrado. A lógica da busca é a mesma de vecSearch_TIPO. Não é
 * recomendado usar essa função diretamente; use vecRemoveLeft,
 * vecRemoveRight ou vecRemoveAll.
 *
 * bool vecRemoveLeft_TIPO(Vector_TIPO *vec, K key, T *out);
 * Busca, remove e retorna, por meio do ponteiro "out" especificado, a primeira
 * ocorrência de um item com chave igual à fornecida (v. vecSearch_TIPO),
 * varrendo o vetor da esquerda para a direita. Retorna true se o item foi
 * removido e false caso não tenha sido encontrado.
 *
 * bool vecRemoveRight_TIPO(Vector_TIPO *vec, K key, T *out);
 * Busca, remove e retorna, por meio do ponteiro "out" especificado, a primeira
 * ocorrência de um item com chave igual à fornecida (v. vecSearch_TIPO),
 * varrendo o vetor da direita para a esquerda. Retorna true se o item foi
 * removido e false caso não tenha sido encontrado.
 *
 * uint64_t vecRemoveAll_TIPO(Vector_TIPO *vec, K key);
 * Remove todos os itens do vetor com chave igual à fornecida (v.
 * vecSearch_TIPO) e retorna o número de itens removidos.
 *
 * void vecFree_TIPO(Vector_TIPO *vec);
 * Libera o espaço de memória controlado pelo vetor. Após essa operação, o vetor
 * ficará no mesmo estado que o retornado por vecCreateEmpty, ou seja, vazio e
 * pronto para uso.
 *
 * void vecClear_TIPO(Vector_TIPO *vec);
 * */

#define HEADER_VECTOR_TYPE(T, K)                                               \
  typedef struct Vector_##T {                                                  \
    size_t size;                                                               \
    size_t capacity;                                                           \
    T *collection;                                                             \
  } Vector_##T;                                                                \
  Vector_##T vecCreateEmpty_##T();                                             \
  size_t vecLength_##T(const Vector_##T *vec);                                 \
  size_t vecCapacity_##T(const Vector_##T *vec);                               \
  void vecGrow_##T(Vector_##T *vec, size_t new_capacity);                      \
  void vecInsert_##T(Vector_##T *vec, T item, size_t index);                   \
  void vecSwap_##T(Vector_##T *a, Vector_##T *b);                              \
  Vector_##T vecCreateWithCapacity_##T(size_t capacity);                       \
  Vector_##T vecCopiedFromArray_##T(const T *array, size_t size);              \
  const T *vecBorrow_##T(const Vector_##T *vec, size_t *out_size);             \
  T *vecTakeCopy_##T(const Vector_##T *vec, size_t *out_size);                 \
  Vector_##T vecDuplicate_##T(const Vector_##T *vec);                          \
  void vecExtend_##T(Vector_##T *dest, const Vector_##T *src);                 \
  T *vecTake_##T(Vector_##T *vec, size_t *out_size);                           \
  void vecClear_##T(Vector_##T *vec);                                          \
  void vecFree_##T(Vector_##T *vec);                                           \
  T *vecKey_##T(const Vector_##T *vec, K key);                                 \
  T *vecLookup_##T(const Vector_##T *vec, K key);                              \
  T *vecIndex_##T(const Vector_##T *vec, size_t index);                        \
  uint64_t vecRemoveAll_##T(Vector_##T *vec, K key);                           \
  bool vecRemoveRight_##T(Vector_##T *vec, K key, T *out);                     \
  bool vecRemoveLeft_##T(Vector_##T *vec, K key, T *out);                      \
  bool vecRemove_##T(Vector_##T *vec, K key, size_t start, int64_t step,       \
                     T *out);                                                  \
  T *vecSearchRight_##T(const Vector_##T *vec, K key);                         \
  T *vecSearchLeft_##T(const Vector_##T *vec, K key);                          \
  T *vecSearch_##T(const Vector_##T *vec, K key, size_t start, int64_t step);  \
  T vecPopLeft_##T(Vector_##T *vec);                                           \
  T vecPopRight_##T(Vector_##T *vec);                                          \
  void vecPushLeft_##T(Vector_##T *vec, T item);                               \
  void vecPushRight_##T(Vector_##T *vec, T item);                              \
  T vecDelete_##T(Vector_##T *vec, size_t index);                              \
  Vector_##T vecCreateSingle_##T(T item);

#define DECLARE_VECTOR_TYPE(T, K, K_get, K_cmp)                                \
  Vector_##T vecCreateEmpty_##T() {                                            \
    Vector_##T vec;                                                            \
    vec.size = vec.capacity = 0;                                               \
    vec.collection = NULL;                                                     \
    return vec;                                                                \
  }                                                                            \
                                                                               \
  size_t vecLength_##T(const Vector_##T *vec) { return vec->size; }            \
                                                                               \
  size_t vecCapacity_##T(const Vector_##T *vec) { return vec->capacity; }      \
                                                                               \
  void vecGrow_##T(Vector_##T *vec, size_t new_capacity) {                     \
    if (new_capacity > vec->capacity) {                                        \
      vec->collection = realloc(vec->collection, new_capacity * sizeof(T));    \
      vec->capacity = new_capacity;                                            \
    }                                                                          \
  }                                                                            \
                                                                               \
  void vecInsert_##T(Vector_##T *vec, T item, size_t index) {                  \
    assert(index <= vec->size);                                                \
    vecGrow_##T(vec, (vec->size + 2) * 3 / 2);                                 \
    for (size_t i = vec->size; i > index; i--) {                               \
      vec->collection[i] = vec->collection[i - 1];                             \
    }                                                                          \
    vec->size += 1;                                                            \
    vec->collection[index] = item;                                             \
  }                                                                            \
                                                                               \
  T vecDelete_##T(Vector_##T *vec, size_t index) {                             \
    assert(vec->collection != NULL);                                           \
    assert(index < vec->size);                                                 \
    T ret = vec->collection[index];                                            \
    for (size_t i = index; i < vec->size - 1; i++) {                           \
      vec->collection[i] = vec->collection[i + 1];                             \
    }                                                                          \
    vec->size -= 1;                                                            \
    return ret;                                                                \
  }                                                                            \
                                                                               \
  void vecPushRight_##T(Vector_##T *vec, T item) {                             \
    vecInsert_##T(vec, item, vec->size);                                       \
  }                                                                            \
                                                                               \
  void vecPushLeft_##T(Vector_##T *vec, T item) {                              \
    vecInsert_##T(vec, item, 0);                                               \
  }                                                                            \
                                                                               \
  T vecPopRight_##T(Vector_##T *vec) {                                         \
    return vecDelete_##T(vec, vec->size - 1);                                  \
  }                                                                            \
                                                                               \
  T vecPopLeft_##T(Vector_##T *vec) { return vecDelete_##T(vec, 0); }          \
                                                                               \
  T *vecSearch_##T(const Vector_##T *vec, K key, size_t start, int64_t step) { \
    int64_t position = (int64_t)start;                                         \
    assert(vec->collection != NULL);                                           \
    while (K_cmp(key, K_get(vec->collection[position]))) {                     \
      position += step;                                                        \
      if (position < 0 || position >= (int64_t)vec->size) {                    \
        return NULL;                                                           \
      }                                                                        \
    }                                                                          \
    return vec->collection + position;                                         \
  }                                                                            \
                                                                               \
  T *vecSearchLeft_##T(const Vector_##T *vec, K key) {                         \
    return vecSearch_##T(vec, key, 0, 1);                                      \
  }                                                                            \
                                                                               \
  T *vecSearchRight_##T(const Vector_##T *vec, K key) {                        \
    return vecSearch_##T(vec, key, vec->size - 1, -1);                         \
  }                                                                            \
                                                                               \
  bool vecRemove_##T(Vector_##T *vec, K key, size_t start, int64_t step,       \
                     T *out) {                                                 \
    int64_t position = (int64_t)start;                                         \
    assert(vec->collection != NULL);                                           \
    while (K_cmp(key, K_get(vec->collection[position]))) {                     \
      position += step;                                                        \
      if (position < 0 || position >= (int64_t)vec->size) {                    \
        return false;                                                          \
      }                                                                        \
    }                                                                          \
    if (out != NULL) {                                                         \
      *out = vec->collection[position];                                        \
    }                                                                          \
    for (int64_t i = position; i < (int64_t)vec->size - 1; i++) {              \
      vec->collection[i] = vec->collection[i + 1];                             \
    }                                                                          \
    vec->size -= 1;                                                            \
    return true;                                                               \
  }                                                                            \
                                                                               \
  bool vecRemoveLeft_##T(Vector_##T *vec, K key, T *out) {                     \
    return vecRemove_##T(vec, key, 0, 1, out);                                 \
  }                                                                            \
                                                                               \
  bool vecRemoveRight_##T(Vector_##T *vec, K key, T *out) {                    \
    return vecRemove_##T(vec, key, vec->size - 1, -1, out);                    \
  }                                                                            \
                                                                               \
  uint64_t vecRemoveAll_##T(Vector_##T *vec, K key) {                          \
    uint64_t count = 0;                                                        \
    while (vecRemoveRight_##T(vec, key, NULL)) {                               \
      count += 1;                                                              \
    }                                                                          \
    return count;                                                              \
  }                                                                            \
                                                                               \
  T *vecIndex_##T(const Vector_##T *vec, size_t index) {                       \
    assert(index < vec->size);                                                 \
    assert(vec->collection != NULL);                                           \
    return vec->collection + index;                                            \
  }                                                                            \
                                                                               \
  T *vecLookup_##T(const Vector_##T *vec, K key) {                             \
    return vecSearchRight_##T(vec, key);                                       \
  }                                                                            \
                                                                               \
  T *vecKey_##T(const Vector_##T *vec, K key) {                                \
    T *res = vecLookup_##T(vec, key);                                          \
    assert(res != NULL);                                                       \
    return res;                                                                \
  }                                                                            \
                                                                               \
  void vecFree_##T(Vector_##T *vec) {                                          \
    vec->size = vec->capacity = 0;                                             \
    if (vec->collection != NULL) {                                             \
      free(vec->collection);                                                   \
      vec->collection = NULL;                                                  \
    }                                                                          \
  }                                                                            \
                                                                               \
  void vecClear_##T(Vector_##T *vec) { vec->size = 0; }                        \
                                                                               \
  T *vecTake_##T(Vector_##T *vec, size_t *out_size) {                          \
    T *ret = vec->collection;                                                  \
    if (out_size != NULL) {                                                    \
      *out_size = vec->size;                                                   \
    }                                                                          \
    vec->size = vec->capacity = 0;                                             \
    vec->collection = NULL;                                                    \
    return ret;                                                                \
  }                                                                            \
                                                                               \
  void vecExtend_##T(Vector_##T *dest, const Vector_##T *src) {                \
    size_t new_size = dest->size + src->size;                                  \
    vecGrow_##T(dest, dest->size + src->size);                                 \
    T *sourcePtr = src->collection;                                            \
    for (T *i = dest->collection + dest->size;                                 \
         i < dest->collection + new_size; i++, sourcePtr++) {                  \
      *i = *sourcePtr;                                                         \
    }                                                                          \
    dest->size = new_size;                                                     \
  }                                                                            \
                                                                               \
  Vector_##T vecDuplicate_##T(const Vector_##T *vec) {                         \
    Vector_##T dupe = vecCreateEmpty_##T();                                    \
    vecExtend_##T(&dupe, vec);                                                 \
    return dupe;                                                               \
  }                                                                            \
                                                                               \
  T *vecTakeCopy_##T(const Vector_##T *vec, size_t *out_size) {                \
    Vector_##T dupe = vecDuplicate_##T(vec);                                   \
    return vecTake_##T(&dupe, out_size);                                       \
  }                                                                            \
                                                                               \
  const T *vecBorrow_##T(const Vector_##T *vec, size_t *out_size) {            \
    if (out_size != NULL) {                                                    \
      *out_size = vec->size;                                                   \
    }                                                                          \
    return vec->collection;                                                    \
  }                                                                            \
                                                                               \
  Vector_##T vecCopiedFromArray_##T(const T *array, size_t size) {             \
    Vector_##T vec = vecCreateEmpty_##T();                                     \
    vecGrow_##T(&vec, size);                                                   \
    vec.size = size;                                                           \
    for (size_t i = 0; i < size; i++) {                                        \
      vec.collection[i] = array[i];                                            \
    }                                                                          \
    return vec;                                                                \
  }                                                                            \
                                                                               \
  Vector_##T vecCreateWithCapacity_##T(size_t capacity) {                      \
    Vector_##T vec = vecCreateEmpty_##T();                                     \
    vecGrow_##T(&vec, capacity);                                               \
    return vec;                                                                \
  }                                                                            \
                                                                               \
  Vector_##T vecCreateSingle_##T(T item) {                                     \
    Vector_##T vec = vecCreateEmpty_##T();                                     \
    vecPushRight_##T(&vec, item);                                              \
    return vec;                                                                \
  }                                                                            \
                                                                               \
  void vecSwap_##T(Vector_##T *a, Vector_##T *b) {                             \
    Vector_##T tmp = *a;                                                       \
    *a = *b;                                                                   \
    *b = tmp;                                                                  \
  }

#define DECLARE_VECTOR_STRINGIFY_FUNCTION(T)                                   \
  DECLARE_STRINGIFY_FUNCTION(Vector_##T, vec) {                                \
    STRINGIFY_PUT("Vector<");                                                  \
    STRINGIFY_PUT(#T);                                                         \
    STRINGIFY_PUT(">: {");                                                     \
    for (size_t i = 0; i < vec.size; i++) {                                    \
      if (i != 0) {                                                            \
        STRINGIFY_PUT("; ");                                                   \
      }                                                                        \
      STRINGIFY_PUT_VALUE(T, vec.collection[i]);                               \
    }                                                                          \
    STRINGIFY_PUT("}");                                                        \
  }

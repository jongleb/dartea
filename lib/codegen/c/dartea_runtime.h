#ifndef DARTEA_RUNTIME_H
#define DARTEA_RUNTIME_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef uintptr_t dartea_value;

enum dartea_kind {
  DARTEA_SCANNED = 0u,
  DARTEA_STRING = 1u,
  DARTEA_FLOAT = 2u,
  DARTEA_CLOSURE = 3u
};

#define DARTEA_RC_SHIFT 32
#define DARTEA_KIND_SHIFT 28
#define DARTEA_TAG_SHIFT 16
#define DARTEA_KIND_MASK 0xfu
#define DARTEA_TAG_MASK 0xfffu
#define DARTEA_SIZE_MASK 0xffffu
#define DARTEA_TAG_MAX DARTEA_TAG_MASK
#define DARTEA_SIZE_MAX DARTEA_SIZE_MASK
#define DARTEA_RC_STATIC 0xffffffffu

typedef struct dartea_object {
  uint64_t header;
  dartea_value fields[];
} dartea_object;

typedef dartea_value (*dartea_code)(struct dartea_object *closure,
                                    dartea_value *arguments);

static size_t dartea_live_count = 0;
static dartea_value *dartea_pending = NULL;
static size_t dartea_pending_count = 0;
static size_t dartea_pending_capacity = 0;

static inline void dartea_out_of_memory(void) { abort(); }

static inline size_t dartea_live_objects(void) { return dartea_live_count; }

static inline uint64_t dartea_header_of(uint32_t rc, unsigned kind, unsigned tag,
                                        unsigned size) {
  return ((uint64_t)rc << DARTEA_RC_SHIFT) |
         ((uint64_t)(kind & DARTEA_KIND_MASK) << DARTEA_KIND_SHIFT) |
         ((uint64_t)(tag & DARTEA_TAG_MASK) << DARTEA_TAG_SHIFT) |
         (uint64_t)(size & DARTEA_SIZE_MASK);
}

static inline uint32_t dartea_rc(const dartea_object *object) {
  return (uint32_t)(object->header >> DARTEA_RC_SHIFT);
}

static inline unsigned dartea_kind(const dartea_object *object) {
  return (unsigned)((object->header >> DARTEA_KIND_SHIFT) & DARTEA_KIND_MASK);
}

static inline unsigned dartea_tag(const dartea_object *object) {
  return (unsigned)((object->header >> DARTEA_TAG_SHIFT) & DARTEA_TAG_MASK);
}

static inline unsigned dartea_size(const dartea_object *object) {
  return (unsigned)(object->header & DARTEA_SIZE_MASK);
}

static inline void dartea_set_rc(dartea_object *object, uint32_t rc) {
  object->header =
      (object->header & 0xffffffffu) | ((uint64_t)rc << DARTEA_RC_SHIFT);
}

static inline bool dartea_is_immediate(dartea_value value) {
  return (value & 1u) != 0u;
}

static inline bool dartea_is_boxed(dartea_value value) {
  return (value & 1u) == 0u;
}

static inline dartea_value dartea_int(int64_t number) {
  return (dartea_value)(((uint64_t)number << 1) | 1u);
}

static inline int64_t dartea_int_value(dartea_value value) {
  return (int64_t)value >> 1;
}

static inline dartea_value dartea_unit(void) { return dartea_int(0); }

static inline dartea_value dartea_bool(bool truth) {
  return dartea_int(truth ? 1 : 0);
}

static inline bool dartea_bool_value(dartea_value value) {
  return dartea_int_value(value) != 0;
}

static inline dartea_value dartea_char(uint32_t codepoint) {
  return dartea_int((int64_t)codepoint);
}

static inline uint32_t dartea_char_value(dartea_value value) {
  return (uint32_t)dartea_int_value(value);
}

static inline dartea_value dartea_nullary(unsigned tag) {
  return dartea_int((int64_t)tag);
}

static inline unsigned dartea_nullary_tag(dartea_value value) {
  return (unsigned)dartea_int_value(value);
}

static inline dartea_object *dartea_object_of(dartea_value value) {
  return (dartea_object *)(void *)value;
}

static inline dartea_value dartea_value_of(dartea_object *object) {
  return (dartea_value)(void *)object;
}

static inline size_t dartea_words_of(unsigned kind, unsigned size) {
  return kind == DARTEA_CLOSURE ? (size_t)size + 1u : (size_t)size;
}

static inline dartea_object *dartea_allocate_words(size_t words) {
  dartea_object *object =
      (dartea_object *)malloc(sizeof(uint64_t) + words * sizeof(dartea_value));
  if (object == NULL) dartea_out_of_memory();
  dartea_live_count++;
  return object;
}

static inline void dartea_release(dartea_object *object) {
  dartea_live_count--;
  free(object);
}

static inline dartea_object *dartea_alloc(unsigned kind, unsigned tag,
                                          unsigned size) {
  dartea_object *object = dartea_allocate_words(dartea_words_of(kind, size));
  object->header = dartea_header_of(1u, kind, tag, size);
  return object;
}

static inline dartea_value *dartea_children(dartea_object *object,
                                            size_t *count) {
  switch (dartea_kind(object)) {
    case DARTEA_SCANNED:
      *count = dartea_size(object);
      return object->fields;
    case DARTEA_CLOSURE:
      *count = dartea_size(object);
      return object->fields + 1;
    default:
      *count = 0;
      return NULL;
  }
}

static inline void dartea_pending_push(dartea_value value) {
  if (dartea_pending_count == dartea_pending_capacity) {
    size_t capacity =
        dartea_pending_capacity == 0 ? 64u : dartea_pending_capacity * 2u;
    dartea_value *grown = (dartea_value *)realloc(
        dartea_pending, capacity * sizeof(dartea_value));
    if (grown == NULL) dartea_out_of_memory();
    dartea_pending = grown;
    dartea_pending_capacity = capacity;
  }
  dartea_pending[dartea_pending_count++] = value;
}

static inline void dartea_pending_free(void) {
  free(dartea_pending);
  dartea_pending = NULL;
  dartea_pending_count = 0;
  dartea_pending_capacity = 0;
}

static inline dartea_value dartea_dup(dartea_value value) {
  if (dartea_is_boxed(value)) {
    dartea_object *object = dartea_object_of(value);
    uint32_t rc = dartea_rc(object);
    if (rc != DARTEA_RC_STATIC) dartea_set_rc(object, rc + 1u);
  }
  return value;
}

static inline void dartea_drop(dartea_value value) {
  size_t base = dartea_pending_count;
  dartea_value current = value;
  for (;;) {
    if (dartea_is_boxed(current)) {
      dartea_object *object = dartea_object_of(current);
      uint32_t rc = dartea_rc(object);
      if (rc > 1u && rc != DARTEA_RC_STATIC) {
        dartea_set_rc(object, rc - 1u);
      } else if (rc != DARTEA_RC_STATIC) {
        size_t count = 0;
        dartea_value *children = dartea_children(object, &count);
        if (count > 0) {
          for (size_t index = 0; index + 1 < count; index++)
            dartea_pending_push(children[index]);
          dartea_value last = children[count - 1];
          dartea_release(object);
          current = last;
          continue;
        }
        dartea_release(object);
      }
    }
    if (dartea_pending_count == base) return;
    current = dartea_pending[--dartea_pending_count];
  }
}

static inline bool dartea_is_unique(dartea_value value) {
  return dartea_is_boxed(value) && dartea_rc(dartea_object_of(value)) == 1u;
}

static inline dartea_object *dartea_drop_reuse(dartea_value value) {
  if (dartea_is_unique(value) &&
      dartea_kind(dartea_object_of(value)) != DARTEA_STRING) {
    dartea_object *object = dartea_object_of(value);
    size_t count = 0;
    dartea_value *children = dartea_children(object, &count);
    for (size_t index = 0; index < count; index++) dartea_drop(children[index]);
    return object;
  }
  dartea_drop(value);
  return NULL;
}

static inline dartea_object *dartea_alloc_at(dartea_object *reused,
                                             unsigned kind, unsigned tag,
                                             unsigned size) {
  if (reused != NULL) {
    if (dartea_words_of(dartea_kind(reused), dartea_size(reused)) ==
        dartea_words_of(kind, size)) {
      reused->header = dartea_header_of(1u, kind, tag, size);
      return reused;
    }
    dartea_release(reused);
  }
  return dartea_alloc(kind, tag, size);
}

static inline size_t dartea_string_words(size_t length) {
  return 1u + (length + 1u + sizeof(dartea_value) - 1u) / sizeof(dartea_value);
}

static inline dartea_value dartea_string(const char *bytes, size_t length) {
  dartea_object *object = dartea_allocate_words(dartea_string_words(length));
  object->header = dartea_header_of(1u, DARTEA_STRING, 0u, 0u);
  object->fields[0] = (dartea_value)length;
  char *payload = (char *)(object->fields + 1);
  memcpy(payload, bytes, length);
  payload[length] = '\0';
  return dartea_value_of(object);
}

static inline size_t dartea_string_length(dartea_value value) {
  return (size_t)dartea_object_of(value)->fields[0];
}

static inline const char *dartea_string_bytes(dartea_value value) {
  return (const char *)(dartea_object_of(value)->fields + 1);
}

static inline dartea_value dartea_float(double number) {
  dartea_object *object = dartea_alloc(DARTEA_FLOAT, 0u, 1u);
  memcpy(object->fields, &number, sizeof number);
  return dartea_value_of(object);
}

static inline double dartea_float_value(dartea_value value) {
  double number;
  memcpy(&number, dartea_object_of(value)->fields, sizeof number);
  return number;
}

static inline dartea_value dartea_closure(dartea_code code,
                                          unsigned environment_size) {
  dartea_object *object = dartea_alloc(DARTEA_CLOSURE, 0u, environment_size);
  memcpy(object->fields, &code, sizeof code);
  return dartea_value_of(object);
}

static inline dartea_code dartea_closure_code(dartea_value value) {
  dartea_code code;
  memcpy(&code, dartea_object_of(value)->fields, sizeof code);
  return code;
}

static inline dartea_value *dartea_closure_environment(dartea_value value) {
  return dartea_object_of(value)->fields + 1;
}

typedef struct dartea_arena_block {
  struct dartea_arena_block *next;
  size_t capacity;
  size_t used;
  unsigned char *bytes;
} dartea_arena_block;

typedef struct dartea_arena {
  dartea_arena_block *blocks;
  size_t block_bytes;
} dartea_arena;

#define DARTEA_ARENA_ALIGNMENT (2u * sizeof(dartea_value))

static inline void dartea_arena_init(dartea_arena *arena, size_t block_bytes) {
  arena->blocks = NULL;
  arena->block_bytes = block_bytes < 4096u ? 4096u : block_bytes;
}

static inline dartea_arena_block *dartea_arena_block_of(size_t capacity) {
  dartea_arena_block *block =
      (dartea_arena_block *)malloc(sizeof(dartea_arena_block) + capacity);
  if (block == NULL) dartea_out_of_memory();
  block->next = NULL;
  block->capacity = capacity;
  block->used = 0;
  block->bytes = (unsigned char *)(block + 1);
  return block;
}

static inline void *dartea_arena_bytes(dartea_arena *arena, size_t bytes) {
  size_t needed = (bytes + DARTEA_ARENA_ALIGNMENT - 1u) &
                  ~(size_t)(DARTEA_ARENA_ALIGNMENT - 1u);
  dartea_arena_block *block = arena->blocks;
  if (block == NULL || block->capacity - block->used < needed) {
    size_t capacity = needed > arena->block_bytes ? needed : arena->block_bytes;
    dartea_arena_block *fresh = dartea_arena_block_of(capacity);
    fresh->next = arena->blocks;
    arena->blocks = fresh;
    block = fresh;
  }
  void *place = block->bytes + block->used;
  block->used += needed;
  return place;
}

static inline dartea_object *dartea_arena_alloc(dartea_arena *arena,
                                                unsigned kind, unsigned tag,
                                                unsigned size) {
  dartea_object *object = (dartea_object *)dartea_arena_bytes(
      arena, sizeof(uint64_t) + dartea_words_of(kind, size) *
                                    sizeof(dartea_value));
  object->header = dartea_header_of(DARTEA_RC_STATIC, kind, tag, size);
  return object;
}

static inline dartea_value dartea_arena_string(dartea_arena *arena,
                                               const char *bytes,
                                               size_t length) {
  dartea_object *object = (dartea_object *)dartea_arena_bytes(
      arena, sizeof(uint64_t) + dartea_string_words(length) *
                                    sizeof(dartea_value));
  object->header = dartea_header_of(DARTEA_RC_STATIC, DARTEA_STRING, 0u, 0u);
  object->fields[0] = (dartea_value)length;
  char *payload = (char *)(object->fields + 1);
  memcpy(payload, bytes, length);
  payload[length] = '\0';
  return dartea_value_of(object);
}

static inline void dartea_arena_reset(dartea_arena *arena) {
  dartea_arena_block *block = arena->blocks;
  while (block != NULL && block->next != NULL) {
    dartea_arena_block *next = block->next;
    free(block);
    block = next;
  }
  arena->blocks = block;
  if (block != NULL) block->used = 0;
}

static inline void dartea_arena_free(dartea_arena *arena) {
  dartea_arena_block *block = arena->blocks;
  while (block != NULL) {
    dartea_arena_block *next = block->next;
    free(block);
    block = next;
  }
  arena->blocks = NULL;
}

#endif

#include "dartea_runtime.h"

#include <stdio.h>

static int failures = 0;

static void check(bool condition, const char *name) {
  if (!condition) {
    failures++;
    printf("FAIL %s\n", name);
  }
}

static dartea_value cons(dartea_value head, dartea_value tail) {
  dartea_object *cell = dartea_alloc(DARTEA_SCANNED, 1u, 2u);
  cell->fields[0] = head;
  cell->fields[1] = tail;
  return dartea_value_of(cell);
}

static dartea_value list_of(size_t length) {
  dartea_value list = dartea_nullary(0u);
  for (size_t index = 0; index < length; index++)
    list = cons(dartea_int((int64_t)index), list);
  return list;
}

static size_t list_length(dartea_value list) {
  size_t length = 0;
  while (dartea_is_boxed(list)) {
    length++;
    list = dartea_object_of(list)->fields[1];
  }
  return length;
}

static dartea_value environment_plus(dartea_object *closure,
                                     dartea_value *arguments) {
  dartea_value *environment = closure->fields + 1;
  return dartea_int(dartea_int_value(environment[0]) +
                    dartea_int_value(arguments[0]));
}

static void header_layout(void) {
  check(sizeof(uint64_t) == 8u, "header is one 64-bit word");
  check(sizeof(dartea_object) == 8u, "object header has no padding");
  dartea_object *object = dartea_alloc(DARTEA_SCANNED, DARTEA_TAG_MAX, 7u);
  check(dartea_rc(object) == 1u, "fresh object has rc 1");
  check(dartea_kind(object) == DARTEA_SCANNED, "kind round-trips");
  check(dartea_tag(object) == DARTEA_TAG_MAX, "maximal tag round-trips");
  check(dartea_size(object) == 7u, "size round-trips");
  dartea_set_rc(object, DARTEA_RC_STATIC - 1u);
  check(dartea_rc(object) == DARTEA_RC_STATIC - 1u, "large rc round-trips");
  check(dartea_tag(object) == DARTEA_TAG_MAX, "rc write keeps the tag");
  check(dartea_size(object) == 7u, "rc write keeps the size");
  dartea_set_rc(object, 1u);
  for (unsigned index = 0; index < 7u; index++)
    object->fields[index] = dartea_unit();
  dartea_drop(dartea_value_of(object));

  dartea_object *wide = dartea_alloc(DARTEA_SCANNED, 0u, DARTEA_SIZE_MAX);
  check(dartea_size(wide) == DARTEA_SIZE_MAX, "maximal size round-trips");
  for (unsigned index = 0; index < DARTEA_SIZE_MAX; index++)
    wide->fields[index] = dartea_unit();
  dartea_drop(dartea_value_of(wide));
}

static void immediates(void) {
  const int64_t numbers[] = {0, 1, -1, 42, -42, INT32_MAX, INT32_MIN,
                             (int64_t)1 << 61, -((int64_t)1 << 61)};
  for (size_t index = 0; index < sizeof numbers / sizeof numbers[0]; index++) {
    dartea_value value = dartea_int(numbers[index]);
    check(dartea_is_immediate(value), "an int is immediate");
    check(!dartea_is_boxed(value), "an int is not boxed");
    check(dartea_int_value(value) == numbers[index], "an int round-trips");
  }
  check(dartea_bool_value(dartea_bool(true)), "true round-trips");
  check(!dartea_bool_value(dartea_bool(false)), "false round-trips");
  check(dartea_char_value(dartea_char(0x1f600u)) == 0x1f600u,
        "a codepoint round-trips");
  check(dartea_nullary_tag(dartea_nullary(DARTEA_TAG_MAX)) == DARTEA_TAG_MAX,
        "a nullary constructor round-trips");
  check(dartea_is_immediate(dartea_unit()), "unit is immediate");

  size_t live = dartea_live_objects();
  dartea_drop(dartea_dup(dartea_int(7)));
  dartea_drop(dartea_int(7));
  check(dartea_live_objects() == live, "immediates never allocate");
}

static void counting(void) {
  size_t live = dartea_live_objects();
  dartea_value pair = cons(dartea_int(1), dartea_nullary(0u));
  check(dartea_live_objects() == live + 1u, "alloc counts one object");
  check(dartea_is_unique(pair), "a fresh object is unique");

  dartea_dup(pair);
  check(dartea_rc(dartea_object_of(pair)) == 2u, "dup raises the count");
  check(!dartea_is_unique(pair), "a shared object is not unique");

  dartea_drop(pair);
  check(dartea_rc(dartea_object_of(pair)) == 1u, "drop lowers the count");
  check(dartea_live_objects() == live + 1u, "drop of a shared object keeps it");

  dartea_drop(pair);
  check(dartea_live_objects() == live, "drop of the last owner frees");
}

static void shared_children(void) {
  size_t live = dartea_live_objects();
  dartea_value shared = cons(dartea_int(1), dartea_nullary(0u));
  dartea_value left = cons(dartea_dup(shared), dartea_nullary(0u));
  dartea_value right = cons(dartea_dup(shared), dartea_nullary(0u));
  dartea_drop(shared);
  check(dartea_live_objects() == live + 3u, "three objects are live");

  dartea_drop(left);
  check(dartea_live_objects() == live + 2u, "the shared child survives");
  check(dartea_int_value(dartea_object_of(dartea_object_of(right)->fields[0])
                             ->fields[0]) == 1,
        "the shared child is intact");

  dartea_drop(right);
  check(dartea_live_objects() == live, "the last owner frees the child");
}

static void deep_drop(void) {
  size_t live = dartea_live_objects();
  const size_t length = 1000000u;
  dartea_value list = list_of(length);
  check(list_length(list) == length, "the list has every cell");
  check(dartea_live_objects() == live + length, "every cell is live");
  dartea_drop(list);
  check(dartea_live_objects() == live, "drop frees a deep list iteratively");
}

static void reuse(void) {
  size_t live = dartea_live_objects();
  dartea_value unique = cons(dartea_int(1), dartea_nullary(0u));
  dartea_object *before = dartea_object_of(unique);
  dartea_object *token = dartea_drop_reuse(unique);
  check(token == before, "a unique object yields its own memory");
  check(dartea_live_objects() == live + 1u, "reuse does not free");

  dartea_object *rebuilt = dartea_alloc_at(token, DARTEA_SCANNED, 2u, 2u);
  check(rebuilt == before, "alloc_at rebuilds in place");
  check(dartea_tag(rebuilt) == 2u, "alloc_at writes the new tag");
  check(dartea_rc(rebuilt) == 1u, "alloc_at resets the count");
  rebuilt->fields[0] = dartea_int(2);
  rebuilt->fields[1] = dartea_nullary(0u);
  dartea_drop(dartea_value_of(rebuilt));
  check(dartea_live_objects() == live, "the rebuilt object is freed");

  dartea_value shared = cons(dartea_int(1), dartea_nullary(0u));
  dartea_dup(shared);
  check(dartea_drop_reuse(shared) == NULL, "a shared object is not reusable");
  check(dartea_rc(dartea_object_of(shared)) == 1u,
        "a refused reuse lowers the count");
  dartea_drop(shared);

  dartea_value narrow = cons(dartea_int(1), dartea_nullary(0u));
  dartea_object *reused = dartea_alloc_at(dartea_drop_reuse(narrow),
                                          DARTEA_SCANNED, 0u, 5u);
  check(dartea_size(reused) == 5u, "a size mismatch allocates afresh");
  for (unsigned index = 0; index < 5u; index++)
    reused->fields[index] = dartea_unit();
  dartea_drop(dartea_value_of(reused));
  check(dartea_live_objects() == live, "nothing leaks through reuse");
}

static void strings(void) {
  size_t live = dartea_live_objects();
  dartea_value text = dartea_string("dartea", 6u);
  check(dartea_is_boxed(text), "a string is boxed");
  check(dartea_kind(dartea_object_of(text)) == DARTEA_STRING,
        "a string knows its kind");
  check(dartea_string_length(text) == 6u, "a string keeps its length");
  check(strcmp(dartea_string_bytes(text), "dartea") == 0,
        "a string keeps its bytes");
  check(dartea_drop_reuse(dartea_dup(text)) == NULL,
        "a string is never a reuse token");
  dartea_drop(text);

  dartea_value empty = dartea_string("", 0u);
  check(dartea_string_length(empty) == 0u, "an empty string has length zero");
  check(dartea_string_bytes(empty)[0] == '\0', "an empty string is terminated");
  dartea_drop(empty);

  dartea_value embedded = dartea_string("a\0b", 3u);
  check(dartea_string_length(embedded) == 3u, "a NUL byte is part of a string");
  check(dartea_string_bytes(embedded)[2] == 'b', "bytes after a NUL survive");
  dartea_drop(embedded);
  check(dartea_live_objects() == live, "strings are freed");
}

static void floats(void) {
  size_t live = dartea_live_objects();
  dartea_value number = dartea_float(0.1);
  check(dartea_kind(dartea_object_of(number)) == DARTEA_FLOAT,
        "a float knows its kind");
  check(dartea_float_value(number) == 0.1, "a float round-trips exactly");
  dartea_value inside = cons(dartea_dup(number), dartea_nullary(0u));
  dartea_drop(number);
  check(dartea_float_value(dartea_object_of(inside)->fields[0]) == 0.1,
        "a float survives inside a constructor");
  dartea_drop(inside);
  check(dartea_live_objects() == live, "floats are freed");
}

static void closures(void) {
  size_t live = dartea_live_objects();
  dartea_value closure = dartea_closure(environment_plus, 1u);
  dartea_closure_environment(closure)[0] = dartea_int(40);
  dartea_value arguments[1] = {dartea_int(2)};
  dartea_value result =
      dartea_closure_code(closure)(dartea_object_of(closure), arguments);
  check(dartea_int_value(result) == 42, "a closure reads its environment");
  check(dartea_size(dartea_object_of(closure)) == 1u,
        "a closure sizes only its environment");
  dartea_drop(closure);
  check(dartea_live_objects() == live, "a closure is freed");

  dartea_value capturing = dartea_closure(environment_plus, 1u);
  dartea_closure_environment(capturing)[0] = cons(dartea_int(1),
                                                  dartea_nullary(0u));
  check(dartea_live_objects() == live + 2u, "a captured value is live");
  dartea_drop(capturing);
  check(dartea_live_objects() == live, "a closure drops its environment");
}

static void arena(void) {
  size_t live = dartea_live_objects();
  dartea_arena frame;
  dartea_arena_init(&frame, 4096u);

  dartea_object *message = dartea_arena_alloc(&frame, DARTEA_SCANNED, 3u, 2u);
  message->fields[0] = dartea_int(7);
  message->fields[1] = dartea_arena_string(&frame, "hello", 5u);
  dartea_value value = dartea_value_of(message);
  check(dartea_is_boxed(value), "an arena object is boxed");
  check(dartea_rc(message) == DARTEA_RC_STATIC, "an arena object is immortal");
  check(dartea_live_objects() == live, "the arena does not use malloc per object");

  dartea_dup(value);
  check(dartea_rc(message) == DARTEA_RC_STATIC, "dup leaves an immortal alone");
  dartea_drop(value);
  check(dartea_rc(message) == DARTEA_RC_STATIC, "drop leaves an immortal alone");
  check(dartea_tag(message) == 3u, "an immortal keeps its tag");
  check(strcmp(dartea_string_bytes(message->fields[1]), "hello") == 0,
        "an arena string keeps its bytes");

  for (size_t index = 0; index < 100000u; index++) {
    dartea_object *filler = dartea_arena_alloc(&frame, DARTEA_SCANNED, 0u, 3u);
    filler->fields[0] = dartea_int((int64_t)index);
    filler->fields[1] = dartea_unit();
    filler->fields[2] = dartea_unit();
  }
  dartea_arena_reset(&frame);
  dartea_object *after = dartea_arena_alloc(&frame, DARTEA_SCANNED, 4u, 1u);
  after->fields[0] = dartea_unit();
  check(dartea_tag(after) == 4u, "the arena serves again after a reset");
  dartea_arena_free(&frame);
  check(dartea_live_objects() == live, "the arena leaves no counted objects");
}

int main(void) {
  header_layout();
  immediates();
  counting();
  shared_children();
  deep_drop();
  reuse();
  strings();
  floats();
  closures();
  arena();
  check(dartea_live_objects() == 0u, "no object outlives the suite");
  dartea_pending_free();
  if (failures == 0) printf("ok\n");
  return failures == 0 ? 0 : 1;
}

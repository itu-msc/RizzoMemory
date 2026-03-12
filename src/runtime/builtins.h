#pragma once

#include <stdio.h>
#include <stdlib.h>

#include "core.h"

static rz_box_t rz_start_event_loop();
static inline rz_box_t rz_register_output_signal(size_t num_args, rz_box_t *args);
static inline rz_box_t rz_eq(rz_box_t a, rz_box_t b);

static inline void rz_builtin_expect_arity(const char *name, size_t expected, size_t actual) {
	if (expected != actual) {
		fprintf(stderr, "Runtime error: builtin '%s' expected %zu argument(s), got %zu\n", name, expected, actual);
		exit(1);
	}
}

static inline int32_t rz_builtin_expect_int(const char *name, size_t index, rz_box_t arg) {
	if (arg.kind != RZ_BOX_INT) {
		fprintf(stderr, "Runtime error: builtin '%s' expected int for argument %zu, got box kind %d\n", name, index + 1, arg.kind);
		exit(1);
	}
	return rz_unbox_int(arg);
}

static inline rz_box_t rz_builtin_start_event_loop(size_t num_args, rz_box_t *args) {
	rz_builtin_expect_arity("start_event_loop", 1, num_args);
	(void)args;
	return rz_start_event_loop();
}

static inline rz_box_t rz_builtin_output_int_signal(size_t num_args, rz_box_t *args) {
	rz_builtin_expect_arity("output_int_signal", 1, num_args);
	return rz_register_output_signal(num_args, args);
}

static inline rz_box_t rz_builtin_eq(size_t num_args, rz_box_t *args) {
	rz_builtin_expect_arity("eq", 2, num_args);
	return rz_eq(args[0], args[1]);
}

static inline rz_box_t rz_builtin_lt(size_t num_args, rz_box_t *args) {
	int32_t lhs;
	int32_t rhs;
	rz_builtin_expect_arity("lt", 2, num_args);
	lhs = rz_builtin_expect_int("lt", 0, args[0]);
	rhs = rz_builtin_expect_int("lt", 1, args[1]);
	return rz_make_ptr(rz_bool_ctor(lhs < rhs));
}

static inline rz_box_t rz_builtin_leq(size_t num_args, rz_box_t *args) {
	int32_t lhs;
	int32_t rhs;
	rz_builtin_expect_arity("leq", 2, num_args);
	lhs = rz_builtin_expect_int("leq", 0, args[0]);
	rhs = rz_builtin_expect_int("leq", 1, args[1]);
	return rz_make_ptr(rz_bool_ctor(lhs <= rhs));
}

static inline rz_box_t rz_builtin_add(size_t num_args, rz_box_t *args) {
	int32_t lhs;
	int32_t rhs;
	rz_builtin_expect_arity("add", 2, num_args);
	lhs = rz_builtin_expect_int("add", 0, args[0]);
	rhs = rz_builtin_expect_int("add", 1, args[1]);
	return rz_make_int(lhs + rhs);
}

static inline rz_box_t rz_builtin_sub(size_t num_args, rz_box_t *args) {
	int32_t lhs;
	int32_t rhs;
	rz_builtin_expect_arity("sub", 2, num_args);
	lhs = rz_builtin_expect_int("sub", 0, args[0]);
	rhs = rz_builtin_expect_int("sub", 1, args[1]);
	return rz_make_int(lhs - rhs);
}

static inline rz_box_t rz_builtin_mul(size_t num_args, rz_box_t *args) {
	int32_t lhs;
	int32_t rhs;
	rz_builtin_expect_arity("mul", 2, num_args);
	lhs = rz_builtin_expect_int("mul", 0, args[0]);
	rhs = rz_builtin_expect_int("mul", 1, args[1]);
	return rz_make_int(lhs * rhs);
}

static inline rz_box_t rz_builtin_div(size_t num_args, rz_box_t *args) {
	int32_t lhs;
	int32_t rhs;
	rz_builtin_expect_arity("div", 2, num_args);
	lhs = rz_builtin_expect_int("div", 0, args[0]);
	rhs = rz_builtin_expect_int("div", 1, args[1]);
	if (rhs == 0) {
		fprintf(stderr, "Runtime error: builtin 'div' received division by zero\n");
		exit(1);
	}
	return rz_make_int(lhs / rhs);
}

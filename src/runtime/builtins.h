#pragma once

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "core.h"

static rz_box_t rz_start_event_loop();
static inline rz_box_t rz_register_output_signal(size_t num_args, rz_box_t *args);
static inline rz_box_t rz_eq(rz_box_t a, rz_box_t b);

static inline void rz_builtin_expect_arity(const char *name, size_t expected, size_t actual)
{
	if (expected != actual)
	{
		fprintf(stderr, "Runtime error: builtin '%s' expected %zu argument(s), got %zu\n", name, expected, actual);
		exit(1);
	}
}

static inline int64_t rz_builtin_expect_int(const char *name, size_t index, rz_box_t arg)
{
	if (arg.kind != RZ_BOX_INT)
	{
		fprintf(stderr, "Runtime error: builtin '%s' expected int for argument %zu, got box kind %d\n", name, index + 1, arg.kind);
		exit(1);
	}
	return rz_unbox_int(arg);
}

static inline rz_box_t rz_builtin_expect_string(const char *name, size_t index, rz_box_t arg)
{
	if (!rz_box_is_string(arg))
	{
		fprintf(stderr, "Runtime error: builtin '%s' expected string for argument %zu, got box kind %d\n", name, index + 1, arg.kind);
		exit(1);
	}
	return arg;
}

static inline bool rz_builtin_expect_bool(const char *name, size_t index, rz_box_t arg)
{
	if (arg.kind != RZ_BOX_PTR || (arg.as.obj != &RZ_BOOL_TRUE && arg.as.obj != &RZ_BOOL_FALSE))
	{
		fprintf(stderr, "Runtime error: builtin '%s' expected bool for argument %zu\n", name, index + 1);
		exit(1);
	}
	return arg.as.obj == &RZ_BOOL_TRUE;
}

static inline rz_box_t rz_builtin_make_nothing(void)
{
	return rz_make_ptr(rz_ctor_var(0, 0));
}

static inline rz_box_t rz_builtin_make_just(rz_box_t value)
{
	return rz_make_ptr(rz_ctor_var(1, 1, value));
}

static inline rz_box_t rz_builtin_start_event_loop(size_t num_args, rz_box_t *args)
{
	rz_builtin_expect_arity("start_event_loop", 1, num_args);
	(void)args;
	return rz_start_event_loop();
}

static inline rz_box_t rz_builtin_console_out_signal(size_t num_args, rz_box_t *args)
{
	rz_builtin_expect_arity("console_out_signal", 1, num_args);
	return rz_register_output_signal(num_args, args);
}

static inline rz_box_t rz_quit(size_t num_args, rz_box_t *args)
{
	rz_builtin_expect_arity("quit", 1, num_args);
	(void)args;
	exit(0);
}

/* This function takes a Later and constructs a signal 
   that will quit the program when the later value produces a head value. 
*/
static inline rz_box_t rz_builtin_quit_at(size_t num_args, rz_box_t *args)
{
    rz_builtin_expect_arity("quit_at", 1, num_args);
    rz_box_t later = args[0];
    if (later.kind != RZ_BOX_PTR) { //|| rz_object_get_type(rz_unbox_ptr(later)) != RZ_LATER) {
        fprintf(stderr, "Runtime error: quit_at expected a later value, got box kind %d\n", later.kind);
        exit(1);
    }

    rz_box_t lifted_quit = rz_lift_c_fun(rz_quit, 1, NULL, 0);
    rz_box_t delayed_quit = rz_make_ptr(rz_ctor_var(0, 1, lifted_quit));
    rz_signal_ctor(rz_make_int(0), rz_make_ptr(rz_ctor(RZ_TAG_LATER_APP, 2, (rz_box_t[]){ delayed_quit, later })));
    return rz_make_int(0); /* return unit */
}

static inline rz_box_t rz_builtin_parse_int(size_t num_args, rz_box_t *args)
{
	const char *source;
	char *end;
	int64_t converted;
	rz_builtin_expect_arity("parse_int", 1, num_args);
	source = rz_string_data(rz_builtin_expect_string("parse_int", 0, args[0]));
	errno = 0;
	converted = strtoll(source, &end, 10);
	if (source == end || *end != '\0' || errno == ERANGE || converted > INT64_MAX || converted < INT64_MIN) {
		// printf("parse_int failed to parse '%s' as int: %s\n", source, strerror(errno));
		return rz_builtin_make_nothing();
	}
	return rz_builtin_make_just(rz_make_int((int64_t)converted));
}

static inline rz_box_t rz_builtin_eq(size_t num_args, rz_box_t *args)
{
	rz_builtin_expect_arity("eq", 2, num_args);
	return rz_eq(args[0], args[1]);
}

static inline rz_box_t rz_builtin_not(size_t num_args, rz_box_t *args)
{
	bool value;
	rz_builtin_expect_arity("not", 1, num_args);
	value = rz_builtin_expect_bool("not", 0, args[0]);
	return rz_make_ptr(rz_bool_ctor(!value));
}

static inline rz_box_t rz_builtin_lt(size_t num_args, rz_box_t *args)
{
	int64_t lhs;
	int64_t rhs;
	rz_builtin_expect_arity("lt", 2, num_args);
	lhs = rz_builtin_expect_int("lt", 0, args[0]);
	rhs = rz_builtin_expect_int("lt", 1, args[1]);
	return rz_make_ptr(rz_bool_ctor(lhs < rhs));
}

static inline rz_box_t rz_builtin_leq(size_t num_args, rz_box_t *args)
{
	int64_t lhs;
	int64_t rhs;
	rz_builtin_expect_arity("leq", 2, num_args);
	lhs = rz_builtin_expect_int("leq", 0, args[0]);
	rhs = rz_builtin_expect_int("leq", 1, args[1]);
	return rz_make_ptr(rz_bool_ctor(lhs <= rhs));
}

static inline rz_box_t rz_builtin_gt(size_t num_args, rz_box_t *args)
{
	int64_t lhs;
	int64_t rhs;
	rz_builtin_expect_arity("gt", 2, num_args);
	lhs = rz_builtin_expect_int("gt", 0, args[0]);
	rhs = rz_builtin_expect_int("gt", 1, args[1]);
	return rz_make_ptr(rz_bool_ctor(lhs > rhs));
}

static inline rz_box_t rz_builtin_geq(size_t num_args, rz_box_t *args)
{
	int64_t lhs;
	int64_t rhs;
	rz_builtin_expect_arity("geq", 2, num_args);
	lhs = rz_builtin_expect_int("geq", 0, args[0]);
	rhs = rz_builtin_expect_int("geq", 1, args[1]);
	return rz_make_ptr(rz_bool_ctor(lhs >= rhs));
}

static inline rz_box_t rz_builtin_add(size_t num_args, rz_box_t *args)
{
	int64_t lhs;
	int64_t rhs;
	rz_builtin_expect_arity("add", 2, num_args);
	lhs = rz_builtin_expect_int("add", 0, args[0]);
	rhs = rz_builtin_expect_int("add", 1, args[1]);
	return rz_make_int(lhs + rhs);
}

static inline rz_box_t rz_builtin_sub(size_t num_args, rz_box_t *args)
{
	int64_t lhs;
	int64_t rhs;
	rz_builtin_expect_arity("sub", 2, num_args);
	lhs = rz_builtin_expect_int("sub", 0, args[0]);
	rhs = rz_builtin_expect_int("sub", 1, args[1]);
	return rz_make_int(lhs - rhs);
}

static inline rz_box_t rz_builtin_mul(size_t num_args, rz_box_t *args)
{
	int64_t lhs;
	int64_t rhs;
	rz_builtin_expect_arity("mul", 2, num_args);
	lhs = rz_builtin_expect_int("mul", 0, args[0]);
	rhs = rz_builtin_expect_int("mul", 1, args[1]);
	return rz_make_int(lhs * rhs);
}

static inline rz_box_t rz_builtin_div(size_t num_args, rz_box_t *args)
{
	int64_t lhs;
	int64_t rhs;
	rz_builtin_expect_arity("div", 2, num_args);
	lhs = rz_builtin_expect_int("div", 0, args[0]);
	rhs = rz_builtin_expect_int("div", 1, args[1]);
	if (rhs == 0)
	{
		fprintf(stderr, "Runtime error: builtin 'div' received division by zero\n");
		exit(1);
	}
	return rz_make_int(lhs / rhs);
}

static inline rz_box_t rz_builtin_string_concat(size_t num_args, rz_box_t *args)
{
	rz_builtin_expect_arity("string_concat", 2, num_args);
	size_t left_len = rz_string_byte_length(rz_builtin_expect_string("string_concat", 0, args[0]));
	size_t right_len = rz_string_byte_length(rz_builtin_expect_string("string_concat", 1, args[1]));
	size_t total_len = left_len + right_len;
	rz_string_t *result = rz_alloc_string(total_len);
	memcpy(result->bytes, rz_string_data(args[0]), left_len);
	memcpy(result->bytes + left_len, rz_string_data(args[1]), right_len);
	return rz_make_ptr((rz_object_t *)result);
}

static inline rz_box_t rz_builtin_string_eq(size_t num_args, rz_box_t *args)
{
	rz_builtin_expect_arity("string_eq", 2, num_args);
	rz_builtin_expect_string("string_eq", 0, args[0]);
	rz_builtin_expect_string("string_eq", 1, args[1]);
	return rz_make_ptr(rz_bool_ctor(rz_string_eq_content(args[0], args[1])));
}

static inline rz_box_t rz_builtin_string_is_empty(size_t num_args, rz_box_t *args)
{
	rz_builtin_expect_arity("string_is_empty", 1, num_args);
	rz_builtin_expect_string("string_is_empty", 0, args[0]);
	return rz_make_ptr(rz_bool_ctor(rz_string_byte_length(args[0]) == 0));
}

static inline rz_box_t rz_builtin_string_head(size_t num_args, rz_box_t *args)
{
	const char *bytes;
	size_t byte_length;
	size_t width;
	rz_builtin_expect_arity("string_head", 1, num_args);
	rz_builtin_expect_string("string_head", 0, args[0]);
	bytes = rz_string_data(args[0]);
	byte_length = rz_string_byte_length(args[0]);
	if (byte_length == 0)
	{
		fprintf(stderr, "Runtime error: string_head on empty string\n");
		exit(1);
	}
	width = rz_utf8_codepoint_width((unsigned char)bytes[0]);
	if (byte_length < width)
	{
		fprintf(stderr, "Runtime error: truncated UTF-8 sequence\n");
		exit(1);
	}
	return rz_make_string_len(bytes, width);
}

static inline rz_box_t rz_builtin_string_tail(size_t num_args, rz_box_t *args)
{
	const char *bytes;
	size_t byte_length;
	size_t width;
	rz_builtin_expect_arity("string_tail", 1, num_args);
	rz_builtin_expect_string("string_tail", 0, args[0]);
	bytes = rz_string_data(args[0]);
	byte_length = rz_string_byte_length(args[0]);
	if (byte_length == 0)
	{
		fprintf(stderr, "Runtime error: string_tail on empty string\n");
		exit(1);
	}
	width = rz_utf8_codepoint_width((unsigned char)bytes[0]);
	if (byte_length < width)
	{
		fprintf(stderr, "Runtime error: truncated UTF-8 sequence\n");
		exit(1);
	}
	return rz_make_string_len(bytes + width, byte_length - width);
}

static inline rz_box_t rz_builtin_match_fail(size_t num_args, rz_box_t *args)
{
	rz_builtin_expect_arity("match_fail", 1, num_args);
	rz_builtin_expect_string("match_fail", 0, args[0]);
	fprintf(stderr, "Runtime error: %s\n", rz_string_data(args[0]));
	exit(1);
}

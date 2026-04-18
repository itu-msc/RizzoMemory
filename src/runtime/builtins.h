#pragma once

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "core.h"
#include "channel.h"
#include "heap.h"
#include "later.h"
#include "timer.h"

static rz_box_t rz_start_event_loop();
static inline rz_box_t rz_register_output_signal(size_t num_args, rz_box_t *args);
static inline rz_box_t rz_eq(rz_box_t a, rz_box_t b);

enum
{
	RZ_TAG_LIST_NIL = 0,
	RZ_TAG_LIST_CONS = 1,
};

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

static inline rz_object_t *rz_builtin_expect_list(const char *name, size_t index, rz_box_t arg)
{
	if (arg.kind != RZ_BOX_PTR)
	{
		fprintf(stderr, "Runtime error: builtin '%s' expected list for argument %zu, got box kind %d\n", name, index + 1, arg.kind);
		exit(1);
	}
	rz_object_t *obj = rz_unbox_ptr(arg);
	if (rz_object_get_type(obj) != RZ_OBJECT)
	{
		fprintf(stderr, "Runtime error: builtin '%s' expected list object for argument %zu, got object type %d\n", name, index + 1, rz_object_get_type(obj));
		exit(1);
	}
	uint16_t tag = rz_object_tag(obj);
	if (tag != RZ_TAG_LIST_NIL && tag != RZ_TAG_LIST_CONS)
	{
		fprintf(stderr, "Runtime error: builtin '%s' expected list constructor for argument %zu, got tag %d\n", name, index + 1, tag);
		exit(1);
	}
	return obj;
}

static inline rz_box_t rz_builtin_make_nil(void)
{
	return rz_make_ptr(rz_ctor_var(RZ_TAG_LIST_NIL, 0));
}

static inline rz_box_t rz_builtin_make_cons(rz_box_t head, rz_box_t tail)
{
	return rz_make_ptr(rz_ctor_var(RZ_TAG_LIST_CONS, 2, head, tail));
}

static inline rz_box_t rz_builtin_make_nothing(void)
{
	return rz_make_ptr(rz_ctor_var(0, 0));
}

static inline rz_box_t rz_builtin_make_just(rz_box_t value)
{
	return rz_make_ptr(rz_ctor_var(1, 1, value));
}

static inline rz_box_t rz_builtin_make_wait_later(rz_channel_t chan)
{
	return rz_make_ptr(rz_ctor_var(RZ_TAG_LATER_WAIT, 1, rz_make_channel(chan)));
}

static inline rz_box_t rz_builtin_clock_step(size_t num_args, rz_box_t *args);

static inline rz_box_t rz_builtin_make_clock_signal(rz_channel_t chan, rz_box_t head)
{
	rz_box_t delayed_step;
	rz_box_t wait_later;
	rz_box_t tail;
	delayed_step = rz_make_ptr(rz_ctor_var(
		RZ_TAG_DELAY,
		1,
		rz_lift_c_fun(rz_builtin_clock_step, 2, (rz_box_t[]){rz_make_channel(chan)}, 1)));
	wait_later = rz_builtin_make_wait_later(chan);
	tail = rz_make_ptr(rz_ctor_var(RZ_TAG_LATER_APP, 2, delayed_step, wait_later));
	return rz_make_ptr_sig(rz_signal_ctor(head, tail));
}

static inline rz_box_t rz_builtin_clock_step(size_t num_args, rz_box_t *args)
{
	rz_channel_t chan;
	rz_builtin_expect_arity("clock_step", 2, num_args);
	chan = rz_builtin_expect_int("clock_step", 0, args[0]);
	return rz_builtin_make_clock_signal(chan, args[1]);
}

static inline rz_box_t rz_builtin_start_event_loop(size_t num_args, rz_box_t *args)
{
	rz_builtin_expect_arity("start_event_loop", 1, num_args);
	(void)args;
	return rz_start_event_loop();
}

static inline rz_box_t rz_builtin_string_of_int(size_t num_args, rz_box_t *args)
{
	rz_builtin_expect_arity("string_of_int", 1, num_args);
	int64_t value = rz_builtin_expect_int("string_of_int", 0, args[0]);
	char buffer[21]; // enough to hold -2^63 and null terminator
	snprintf(buffer, sizeof(buffer), "%" PRId64, value);
	return rz_make_string_len(buffer, strlen(buffer));
}

static inline rz_box_t rz_builtin_mod(size_t num_args, rz_box_t *args)
{
	int64_t lhs;
	int64_t rhs;
	rz_builtin_expect_arity("mod", 2, num_args);
	lhs = rz_builtin_expect_int("mod", 0, args[0]);
	rhs = rz_builtin_expect_int("mod", 1, args[1]);
	if (rhs == 0)
	{
		fprintf(stderr, "Runtime error: builtin 'mod' received division by zero\n");
		exit(1);
	}
	return rz_make_int(lhs % rhs);
}

static inline rz_box_t rz_builtin_abs(size_t num_args, rz_box_t *args)
{
	int64_t value;
	rz_builtin_expect_arity("abs", 1, num_args);
	value = rz_builtin_expect_int("abs", 0, args[0]);
	if (value == INT64_MIN)
	{
		fprintf(stderr, "Runtime error: builtin 'abs' cannot negate INT64_MIN\n");
		exit(1);
	}
	return rz_make_int(value < 0 ? -value : value);
}

static inline rz_box_t rz_builtin_min(size_t num_args, rz_box_t *args)
{
	int64_t lhs;
	int64_t rhs;
	rz_builtin_expect_arity("min", 2, num_args);
	lhs = rz_builtin_expect_int("min", 0, args[0]);
	rhs = rz_builtin_expect_int("min", 1, args[1]);
	return rz_make_int(lhs < rhs ? lhs : rhs);
}

static inline rz_box_t rz_builtin_max(size_t num_args, rz_box_t *args)
{
	int64_t lhs;
	int64_t rhs;
	rz_builtin_expect_arity("max", 2, num_args);
	lhs = rz_builtin_expect_int("max", 0, args[0]);
	rhs = rz_builtin_expect_int("max", 1, args[1]);
	return rz_make_int(lhs > rhs ? lhs : rhs);
}

static inline rz_box_t rz_builtin_clamp(size_t num_args, rz_box_t *args)
{
	int64_t value;
	int64_t lower;
	int64_t upper;
	rz_builtin_expect_arity("clamp", 3, num_args);
	value = rz_builtin_expect_int("clamp", 0, args[0]);
	lower = rz_builtin_expect_int("clamp", 1, args[1]);
	upper = rz_builtin_expect_int("clamp", 2, args[2]);
	if (lower > upper)
	{
		fprintf(stderr, "Runtime error: builtin 'clamp' expected lower bound <= upper bound\n");
		exit(1);
	}
	if (value < lower)
	{
		return rz_make_int(lower);
	}
	if (value > upper)
	{
		return rz_make_int(upper);
	}
	return rz_make_int(value);
}

static inline rz_box_t rz_builtin_console_out_signal(size_t num_args, rz_box_t *args)
{
	rz_builtin_expect_arity("console_out_signal", 1, num_args);
	return rz_register_output_signal(num_args, args);
}

static inline rz_box_t rz_builtin_string_contains(size_t num_args, rz_box_t *args)
{
	size_t text_len;
	size_t needle_len;
	const char *text_bytes;
	const char *needle_bytes;
	rz_builtin_expect_arity("string_contains", 2, num_args);
	rz_builtin_expect_string("string_contains", 0, args[0]);
	rz_builtin_expect_string("string_contains", 1, args[1]);
	text_len = rz_string_byte_length(args[0]);
	needle_len = rz_string_byte_length(args[1]);
	text_bytes = rz_string_data(args[0]);
	needle_bytes = rz_string_data(args[1]);
	if (needle_len == 0)
	{
		return rz_make_ptr(rz_bool_ctor(true));
	}
	if (needle_len > text_len)
	{
		return rz_make_ptr(rz_bool_ctor(false));
	}
	for (size_t i = 0; i + needle_len <= text_len; i++)
	{
		if (memcmp(text_bytes + i, needle_bytes, needle_len) == 0)
		{
			return rz_make_ptr(rz_bool_ctor(true));
		}
	}
	return rz_make_ptr(rz_bool_ctor(false));
}

static inline rz_box_t rz_builtin_string_starts_with(size_t num_args, rz_box_t *args)
{
	size_t text_len;
	size_t prefix_len;
	const char *text_bytes;
	const char *prefix_bytes;
	rz_builtin_expect_arity("string_starts_with", 2, num_args);
	rz_builtin_expect_string("string_starts_with", 0, args[0]);
	rz_builtin_expect_string("string_starts_with", 1, args[1]);
	text_len = rz_string_byte_length(args[0]);
	prefix_len = rz_string_byte_length(args[1]);
	text_bytes = rz_string_data(args[0]);
	prefix_bytes = rz_string_data(args[1]);
	if (prefix_len > text_len)
	{
		return rz_make_ptr(rz_bool_ctor(false));
	}
	return rz_make_ptr(rz_bool_ctor(memcmp(text_bytes, prefix_bytes, prefix_len) == 0));
}

static inline rz_box_t rz_builtin_string_ends_with(size_t num_args, rz_box_t *args)
{
	size_t text_len;
	size_t suffix_len;
	const char *text_bytes;
	const char *suffix_bytes;
	rz_builtin_expect_arity("string_ends_with", 2, num_args);
	rz_builtin_expect_string("string_ends_with", 0, args[0]);
	rz_builtin_expect_string("string_ends_with", 1, args[1]);
	text_len = rz_string_byte_length(args[0]);
	suffix_len = rz_string_byte_length(args[1]);
	text_bytes = rz_string_data(args[0]);
	suffix_bytes = rz_string_data(args[1]);
	if (suffix_len > text_len)
	{
		return rz_make_ptr(rz_bool_ctor(false));
	}
	return rz_make_ptr(rz_bool_ctor(memcmp(text_bytes + (text_len - suffix_len), suffix_bytes, suffix_len) == 0));
}

static inline rz_box_t rz_builtin_list_is_empty(size_t num_args, rz_box_t *args)
{
	rz_builtin_expect_arity("list_is_empty", 1, num_args);
	rz_object_t *list = rz_builtin_expect_list("list_is_empty", 0, args[0]);
	return rz_make_ptr(rz_bool_ctor(rz_object_tag(list) == RZ_TAG_LIST_NIL));
}

static inline rz_box_t rz_builtin_list_length(size_t num_args, rz_box_t *args)
{
	rz_builtin_expect_arity("list_length", 1, num_args);
	rz_object_t *list = rz_builtin_expect_list("list_length", 0, args[0]);
	int64_t length = 0;
	while (rz_object_tag(list) == RZ_TAG_LIST_CONS)
	{
		length += 1;
		list = rz_builtin_expect_list("list_length", 0, rz_object_get_field(list, 1));
	}
	return rz_make_int(length);
}

static inline rz_box_t rz_builtin_string_split(size_t num_args, rz_box_t *args)
{
	rz_builtin_expect_arity("string_split", 2, num_args);
	rz_box_t source_box = rz_builtin_expect_string("string_split", 0, args[0]);
	rz_box_t delimiter_box = rz_builtin_expect_string("string_split", 1, args[1]);
	const char *source = rz_string_data(source_box);
	const char *delimiter = rz_string_data(delimiter_box);
	size_t source_len = rz_string_byte_length(source_box);
	size_t delimiter_len = rz_string_byte_length(delimiter_box);
	if (delimiter_len == 0)
	{
		fprintf(stderr, "Runtime error: builtin 'string_split' expected a non-empty delimiter\n");
		exit(1);
	}

	size_t part_count = 1;
	for (size_t i = 0; i + delimiter_len <= source_len;)
	{
		if (memcmp(source + i, delimiter, delimiter_len) == 0)
		{
			part_count += 1;
			i += delimiter_len;
		}
		else
		{
			i += 1;
		}
	}

	size_t *starts = alloca(sizeof(size_t) * part_count);
	size_t *lengths = alloca(sizeof(size_t) * part_count);
	size_t part_index = 0;
	size_t segment_start = 0;
	for (size_t i = 0; i + delimiter_len <= source_len;)
	{
		if (memcmp(source + i, delimiter, delimiter_len) == 0)
		{
			starts[part_index] = segment_start;
			lengths[part_index] = i - segment_start;
			part_index += 1;
			i += delimiter_len;
			segment_start = i;
		}
		else
		{
			i += 1;
		}
	}
	starts[part_index] = segment_start;
	lengths[part_index] = source_len - segment_start;

	rz_box_t result = rz_builtin_make_nil();
	for (size_t index = part_count; index > 0; index--)
	{
		rz_box_t piece = rz_make_string_len(source + starts[index - 1], lengths[index - 1]);
		result = rz_builtin_make_cons(piece, result);
	}
	return result;
}

static inline rz_box_t rz_builtin_clock(size_t num_args, rz_box_t *args)
{
	int64_t interval_ms;
	rz_channel_t chan;
	rz_box_t head;
	rz_builtin_expect_arity("clock", 1, num_args);
	interval_ms = rz_builtin_expect_int("clock", 0, args[0]);
	if (interval_ms <= 0)
	{
		fprintf(stderr, "Runtime error: builtin 'clock' expected a positive interval in milliseconds, got %"PRId64"\n", interval_ms);
		exit(1);
	}
	chan = rz_timer_register(interval_ms);
	head = rz_make_int(0);
	return rz_builtin_make_clock_signal(chan, head);
}

static inline rz_box_t rz_quit(size_t num_args, rz_box_t *args)
{
	rz_builtin_expect_arity("quit", 1, num_args);
	rz_should_quit = true;
	return args[0];
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

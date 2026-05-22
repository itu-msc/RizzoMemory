#pragma once

#include "core.h"
#include "heap.h"
#include "later.h"

static inline void rz_print_box(rz_box_t box);
static inline void rz_print_signal(rz_box_t box);
static inline void rz_print_later(rz_box_t later);
static inline void rz_print_delayed(rz_box_t delay);
static inline void rz_print_heap(void);

static inline void rz_print_box(rz_box_t box) {
	switch (box.kind) {
		case RZ_BOX_INT: {
			printf("%" PRId64 "", box.as.i64);
		} break;
		case RZ_BOX_STRING_LITERAL: {
#ifdef __RZ_DEBUG_INFO
			printf("string-literal@%p(%s)", (void*)rz_unbox_str_lit(box), rz_unbox_str_lit(box));
#else
			printf("%s", rz_unbox_str_lit(box));
#endif
		} break;
		case RZ_BOX_PTR: {
			switch (rz_object_get_type(rz_unbox_ptr(box))) {
				case RZ_STRING: {
#ifdef __RZ_DEBUG_INFO
					printf("string@%p(%s)", (void *)rz_unbox_ptr(box), ((rz_string_t*)rz_unbox_ptr(box))->bytes);
#else
					printf("%s", ((rz_string_t*)rz_unbox_ptr(box))->bytes);
#endif
				} break;
				case RZ_SIGNAL: {
					rz_print_signal(box);
				} break;
				case RZ_OBJECT: {
					rz_object_fields_t* fields = (rz_object_fields_t*)box.as.obj;
#ifdef __RZ_DEBUG_INFO
					printf("ctor(%d, ref: %d)@%p ", fields->_base.header.tag, fields->_base.header.refcount, (void *)rz_unbox_ptr(box));
#else
					printf("ctor(%d, ref: %d) ", fields->_base.header.tag, fields->_base.header.refcount);
#endif
					if (fields->_base.header.num_fields > 0) {
						printf("{ ");
						for (size_t i = 0; i < fields->_base.header.num_fields; i++) {
							rz_print_box(fields->fields[i]);
							if (i < fields->_base.header.num_fields - 1) {
								printf(", ");
							}
						}
						printf("}");
					}
				} break;
				case RZ_PARTIAL_APP: {
					rz_function_t* fun = (rz_function_t*)box.as.obj;
#ifdef __RZ_DEBUG_INFO
					printf("pap@%p(ref: %d, arity: %d, applied_vars: %d)", (void *)rz_unbox_ptr(box), fun->_base.header.refcount, fun->_base.header.tag, fun->_base.header.num_fields);
#else
					printf("pap(ref: %d, arity: %d, applied_vars: %d)", fun->_base.header.refcount, fun->_base.header.tag, fun->_base.header.num_fields);
#endif
				} break;
				default: {
					printf("Unknown object type: '%d'", rz_object_get_type(rz_unbox_ptr(box)));
					exit(1);
				}
			}
		} break;
		default: {
			printf("Unknown box tag: '%d'", box.kind);
			exit(1);
		}
	}
}

static inline void rz_print_signal(rz_box_t box)
{
	rz_signal_t *signal = (rz_signal_t *)rz_unbox_ptr(box);
#ifdef __RZ_DEBUG_INFO
	printf("signal#%" PRIu64 "@%p(ref: %d, head: ", rz_unbox_int(signal->debug_index), (void *)signal, signal->_base.refcount);
#else
	printf("signal(ref: %d, head: ", signal->_base.refcount);
#endif
	rz_print_box(signal->head);
	printf(", tail: ");
	rz_print_box(signal->tail);
	printf(", updated: %"PRId64")", rz_unbox_int(signal->updated));
}

static inline void rz_print_later(rz_box_t later) {
	switch (rz_object_tag(rz_unbox_ptr(later))) {
		case RZ_TAG_LATER_NEVER: { printf("never"); } break;
		case RZ_TAG_LATER_WAIT: {
			printf("wait(%"PRId64")", rz_unbox_int(rz_object_get_field(rz_unbox_ptr(later), 0)));
		} break;
		case RZ_TAG_LATER_TAIL: {
			printf("tail(");
			rz_print_signal(rz_object_get_field(rz_unbox_ptr(later), 0));
			printf(")");
		} break;
		case RZ_TAG_LATER_SYNC: {
			printf("sync (");
			rz_print_later(rz_object_get_field(rz_unbox_ptr(later), 0));
			printf(", ");
			rz_print_later(rz_object_get_field(rz_unbox_ptr(later), 1));
			printf(")");
		} break;
		case RZ_TAG_LATER_WATCH: {
			printf("watch(");
			rz_print_signal(rz_object_get_field(rz_unbox_ptr(later), 0));
			printf(")");
		} break;
		case RZ_TAG_LATER_APP: {
			printf("laterapp(");
			rz_print_delayed(rz_object_get_field(rz_unbox_ptr(later), 0));
			printf(", ");
			rz_print_later(rz_object_get_field(rz_unbox_ptr(later), 1));
			printf(")");
		} break;
		default: printf("Unknown later tag: %d", rz_object_tag(rz_unbox_ptr(later))); break;
	}
}

static inline void rz_print_delayed(rz_box_t delay) {
	switch (rz_object_tag(rz_unbox_ptr(delay))) {
		case RZ_TAG_DELAY: {
#ifdef __RZ_DEBUG_INFO
			printf("delay@%p(", (void *)rz_unbox_ptr(delay)); rz_print_box(rz_object_get_field(rz_unbox_ptr(delay), 0)); printf(")");
#else
			printf("delay("); rz_print_box(rz_object_get_field(rz_unbox_ptr(delay), 0)); printf(")");
#endif
		} break;
		case RZ_TAG_OSTAR: {
#ifdef __RZ_DEBUG_INFO
			printf("delayedapp@%p(", (void *)rz_unbox_ptr(delay));
#else
			printf("delayedapp(");
#endif
			rz_print_delayed(rz_object_get_field(rz_unbox_ptr(delay), 0));
			printf(", ");
			rz_print_delayed(rz_object_get_field(rz_unbox_ptr(delay), 1));
			printf(")");
		} break;
		default: printf("Unknown delay tag: %d", rz_object_tag(rz_unbox_ptr(delay))); break;
	}
}

static inline void rz_print_heap(void)
{
#ifdef __RZ_DEBUG_INFO
	printf("(size: %zu) ", rz_heap_size);
	for (rz_signal_t *sig = &rz_heap_head; sig != NULL; sig = (rz_signal_t *)rz_unbox_ptr(sig->next))
	{
		if (sig == &rz_heap_head || sig == &rz_heap_tail)
		{
			if (sig == rz_heap_cursor)
				printf("(|) ");
			else
				printf("| ");
		}
		else if (rz_unbox_int(sig->updated))
		{
			printf("[%zu] ", rz_unbox_int(sig->debug_index));
		}
		else
		{
			if (sig == rz_heap_cursor)
				printf("(%zu) ", rz_unbox_int(sig->debug_index));
			else
				printf("%zu ", rz_unbox_int(sig->debug_index));
		}
	}
	printf("\n");
#else
	(void)rz_heap_size;
#endif
}

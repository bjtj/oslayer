TESTS=$(check_PROGRAMS)

check_PROGRAMS = \
	test-os \
	test-logger \
	test-library \
	test-count-down-latch \
	test-test-suite \
	test-lisp \
	test-properties \
	test-network \
	test-process \
	test-file-stream \
	test-timer \
	test-exception \
	test-message-queue \
	test-thread-pool \
	test-task-thread-pool \
	test-xml \
	test-string-element \
	test-property-map \
	test-socket \
	test-observer-pattern

test_os_SOURCES=test-os.cpp
test_logger_SOURCES=test-logger.cpp
test_library_SOURCES=test-library.cpp
test_count_down_latch_SOURCES=test-count-down-latch.cpp
test_test_suite_SOURCES=test-test-suite.cpp
test_lisp_SOURCES=test-lisp.cpp
test_properties_SOURCES=test-properties.cpp
test_network_SOURCES=test-network.cpp
test_process_SOURCES=test-process.cpp
test_file_stream_SOURCES=test-file-stream.cpp
test_timer_SOURCES=test-timer.cpp
test_exception_SOURCES=test-exception.cpp
test_message_queue_SOURCES=test-message-queue.cpp
test_thread_pool_SOURCES=test-thread-pool.cpp
test_task_thread_pool_SOURCES=test-task-thread-pool.cpp
test_xml_SOURCES=test-xml.cpp
test_string_element_SOURCES=test-string-element.cpp
test_property_map_SOURCES=test-property-map.cpp
test_socket_SOURCES=test-socket.cpp
test_observer_pattern_SOURCES=test-observer-pattern.cpp


AM_CPPFLAGS=-I$(top_srcdir)
AM_CXXFLAGS=$(PTHREAD_CXXFLAGS)
AM_LDFLAGS=$(PTHREAD_CFLAGS)
LDADD=../liboslayer/liboslayer.la

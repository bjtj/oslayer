@VALGRIND_CHECK_RULES@

SUBDIRS=data

TESTS=$(check_PROGRAMS)

check_PROGRAMS = \
	test-os \
	test-file \
	test-ref \
	test-date \
	test-base64 \
	test-bytearray \
	test-event \
	test-random \
	test-tree \
	test-regex \
	test-arguments \
	test-text \
	test-logger \
	test-iterator \
	test-library \
	test-thread-pool \
	test-task-thread-pool \
	test-test-suite \
	test-heap \
	test-lisp \
	test-properties \
	test-network \
	test-process \
	test-file-stream \
	test-timer \
	test-exception \
	test-message-queue \
	test-xml \
	test-string-elements \
	test-property-map \
	test-database-connection \
	test-socket \
	test-echo-server-client \
	test-datagramsocket \
	test-observer-pattern \
	test-register \
	test-lifetime \
	test-uuid \
	test-uri

if SUPPORT_OPENSSL
check_PROGRAMS += test-secure-socket
endif

test_os_SOURCES=test-os.cpp
test_file_SOURCES=test-file.cpp
test_ref_SOURCES=test-ref.cpp
test_date_SOURCES=test-date.cpp
test_base64_SOURCES=test-base64.cpp
test_bytearray_SOURCES=test-bytearray.cpp
test_event_SOURCES=test-event.cpp
test_random_SOURCES=test-random.cpp
test_tree_SOURCES=test-tree.cpp
test_regex_SOURCES=test-regex.cpp
test_arguments_SOURCES=test-arguments.cpp
test_text_SOURCES=test-text.cpp
test_logger_SOURCES=test-logger.cpp
test_iterator_SOURCES=test-iterator.cpp
test_library_SOURCES=test-library.cpp
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
test_string_elements_SOURCES=test-string-elements.cpp
test_property_map_SOURCES=test-property-map.cpp
test_database_connection_SOURCES=test-database-connection.cpp
test_socket_SOURCES=test-socket.cpp
test_echo_server_client_SOURCES=test-echo-server-client.cpp
test_datagramsocket_SOURCES=test-datagramsocket.cpp
test_observer_pattern_SOURCES=test-observer-pattern.cpp
test_register_SOURCES=test-register.cpp
test_heap_SOURCES=test-heap.cpp
test_lifetime_SOURCES=test-lifetime.cpp
test_uuid_SOURCES=test-uuid.cpp
test_uri_SOURCES=test-uri.cpp

if SUPPORT_OPENSSL
test_secure_socket_SOURCES=test-secure-socket.cpp
endif

AM_CPPFLAGS=-I$(top_srcdir)

# cxxflags
AM_CXXFLAGS=$(PTHREAD_CXXFLAGS)
AM_CXXFLAGS+=-DDATA_PATH="\"$(builddir)/data\""
AM_CXXFLAGS+=$(LIBOPENSSL_CFLAGS)

# libs
AM_LDFLAGS=$(PTHREAD_CFLAGS)
AM_LDFLAGS+=$(LIBOPENSSL_LIBS)

# 
LDADD=../liboslayer/liboslayer.la

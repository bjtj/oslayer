TESTS=$(check_PROGRAMS)

check_PROGRAMS = test-hello test-lisp test-properties test-os

test_hello_SOURCES=test-hello.cpp
test_hello_CPPFLAGS=-I$(top_srcdir)
test_hello_CXXFLAGS=$(PTHREAD_CXXFLAGS)
test_hello_LDFLAGS=$(PTHREAD_CFLAGS)
test_hello_LDADD=../liboslayer/liboslayer.la

test_lisp_SOURCES=test-lisp.cpp
test_lisp_CPPFLAGS=-I$(top_srcdir)
test_lisp_CXXFLAGS=$(PTHREAD_CXXFLAGS)
test_lisp_LDFLAGS=$(PTHREAD_CFLAGS)
test_lisp_LDADD=../liboslayer/liboslayer.la

test_properties_SOURCES=test-properties.cpp
test_properties_CPPFLAGS=-I$(top_srcdir)
test_properties_CXXFLAGS=$(PTHREAD_CXXFLAGS)
test_properties_LDFLAGS=$(PTHREAD_CFLAGS)
test_properties_LDADD=../liboslayer/liboslayer.la

test_os_SOURCES=test-os.cpp
test_os_CPPFLAGS=-I$(top_srcdir)
test_os_CXXFLAGS=$(PTHREAD_CXXFLAGS)
test_os_LDFLAGS=$(PTHREAD_CFLAGS)
test_os_LDADD=../liboslayer/liboslayer.la

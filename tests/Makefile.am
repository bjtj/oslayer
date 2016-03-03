TESTS=$(check_PROGRAMS)

check_PROGRAMS = test-hello

test_hello_SOURCES=test-hello.cpp
test_hello_CPPFLAGS=-I$(top_srcdir)
test_hello_CXXFLAGS=$(PTHREAD_CXXFLAGS)
test_hello_LDFLAGS=$(PTHREAD_CFLAGS)
test_hello_LDADD=../liboslayer/liboslayer.la

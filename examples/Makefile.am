noinst_PROGRAMS=repl echo-server

repl_SOURCES=repl.cpp
echo_server_SOURCES=echo-server.cpp

AM_CPPFLAGS=-I$(top_srcdir)
AM_CXXFLAGS=$(PTHREAD_CXXFLAGS)
AM_LDFLAGS=$(PTHREAD_CFLAGS)
LDADD=../liboslayer/liboslayer.la

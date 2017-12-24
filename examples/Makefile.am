noinst_PROGRAMS=repl echo-server

repl_SOURCES=repl.cpp
echo_server_SOURCES=echo-server.cpp

AM_CPPFLAGS=-I$(top_srcdir)

# cxxflags
AM_CXXFLAGS=$(PTHREAD_CXXFLAGS)
AM_CXXFLAGS+=$(LIBOPENSSL_CFLAGS)

# libs
AM_LDFLAGS=$(PTHREAD_CFLAGS)
AM_LDFLAGS+=$(LIBOPENSSL_LIBS)

LDADD=../liboslayer/liboslayer.la

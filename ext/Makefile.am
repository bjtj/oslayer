if SUPPORT_MYSQLCLIENT
lib_LTLIBRARIES = libmysqlconnector.la

libmysqlconnector_la_SOURCES=mysql-connector.cpp

libmysqlconnector_la_CXXFLAGS=$(PTHREAD_CXXFLAGS) $(LIBMYSQLCLIENT_CFLAGS) -I$(top_srcdir)
libmysqlconnector_la_LDFLAGS=$(PTHREAD_CFLAGS) $(LIBMYSQLCLIENT_LIBS)
LDADD=../liboslayer/liboslayer.la
endif

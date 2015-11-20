lib_LTLIBRARIES = liboslayer.la

liboslayerincdir=${includedir}/liboslayer
liboslayerinc_HEADERS=FileReaderWriter.hpp \
	Iteration.hpp \
	Logger.hpp \
	PollablePool.hpp \
	RandomPortBinders.hpp \
	StringElement.hpp \
	Text.hpp \
	ThreadPool.hpp \
	os.hpp

liboslayer_la_SOURCES=FileReaderWriter.cpp \
	FileReaderWriter.hpp \
	Iteration.cpp \
	Iteration.hpp \
	Logger.cpp \
	Logger.hpp \
	PollablePool.cpp \
	PollablePool.hpp \
	RandomPortBinders.cpp \
	RandomPortBinders.hpp \
	StringElement.hpp \
	Text.cpp \
	Text.hpp \
	ThreadPool.cpp \
	ThreadPool.hpp \
	os.cpp \
	os.hpp
liboslayer_la_CXXFLAGS=$(PTHREAD_CXXFLAGS)
liboslayer_la_LDFLAGS=$(PTHREAD_CFLAGS)



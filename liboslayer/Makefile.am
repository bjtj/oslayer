lib_LTLIBRARIES = liboslayer.la

liboslayerincdir=${includedir}/liboslayer
liboslayerinc_HEADERS=AutoRef.hpp \
	FileReaderWriter.hpp \
	Iteration.hpp \
	Logger.hpp \
	Measurement.hpp \
	PollablePool.hpp \
	RandomPortBinders.hpp \
	StringElement.hpp \
	Text.hpp \
	ThreadPool.hpp \
	Utils.hpp \
	os.hpp


liboslayer_la_SOURCES=AutoRef.cpp \
	AutoRef.hpp \
	FileReaderWriter.cpp \
	FileReaderWriter.hpp \
	Iteration.cpp \
	Iteration.hpp \
	Logger.cpp \
	Logger.hpp \
	Measurement.cpp \
	Measurement.hpp \
	PollablePool.cpp \
	PollablePool.hpp \
	RandomPortBinders.cpp \
	RandomPortBinders.hpp \
	StringElement.hpp \
	Text.cpp \
	Text.hpp \
	ThreadPool.cpp \
	ThreadPool.hpp \
	Utils.hpp \
	os.cpp \
	os.hpp

liboslayer_la_CXXFLAGS=$(PTHREAD_CXXFLAGS)
liboslayer_la_LDFLAGS=$(PTHREAD_CFLAGS)

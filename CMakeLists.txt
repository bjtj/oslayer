cmake_minimum_required(VERSION 3.5)

project(oslayer LANGUAGES CXX VERSION "1.0.0")

enable_testing()

add_library(oslayer SHARED
  liboslayer/Arguments.cpp
  liboslayer/Arguments.hpp
  liboslayer/AutoLock.cpp
  liboslayer/AutoLock.hpp
  liboslayer/AutoRef.cpp
  liboslayer/AutoRef.hpp
  liboslayer/AutoRelease.hpp
  liboslayer/Base64.cpp
  liboslayer/Base64.hpp
  liboslayer/ByteArray.cpp
  liboslayer/ByteArray.hpp
  liboslayer/Condition.hpp
  liboslayer/CountDownLatch.cpp
  liboslayer/CountDownLatch.hpp
  liboslayer/DatabaseConnection.cpp
  liboslayer/DatabaseConnection.hpp
  liboslayer/DatabaseDriver.cpp
  liboslayer/DatabaseDriver.hpp
  liboslayer/DatagramSocket.cpp
  liboslayer/DatagramSocket.hpp
  liboslayer/Date.cpp
  liboslayer/Date.hpp
  liboslayer/Endian.cpp
  liboslayer/Endian.hpp
  liboslayer/Event.cpp
  liboslayer/Event.hpp
  liboslayer/File.cpp
  liboslayer/File.hpp
  liboslayer/FileStream.cpp
  liboslayer/FileStream.hpp
  liboslayer/Hash.hpp
  liboslayer/Heap.hpp
  liboslayer/Iterator.hpp
  liboslayer/Library.cpp
  liboslayer/Library.hpp
  liboslayer/Lifetime.hpp
  liboslayer/Lisp.cpp
  liboslayer/Lisp.hpp
  liboslayer/Logger.cpp
  liboslayer/Logger.hpp
  liboslayer/MessageQueue.hpp
  liboslayer/Mutex.cpp
  liboslayer/Mutex.hpp
  liboslayer/Network.cpp
  liboslayer/Network.hpp
  liboslayer/Object.hpp
  liboslayer/Observer.cpp
  liboslayer/Observer.hpp
  liboslayer/Pool.hpp
  liboslayer/Process.cpp
  liboslayer/Process.hpp
  liboslayer/Properties.cpp
  liboslayer/Properties.hpp
  liboslayer/PropertyMap.hpp
  liboslayer/Random.cpp
  liboslayer/Random.hpp
  liboslayer/Ref.hpp
  liboslayer/Regex.cpp
  liboslayer/Regex.hpp
  liboslayer/Register.hpp
  liboslayer/SecureSocket.cpp
  liboslayer/SecureSocket.hpp
  liboslayer/Selector.cpp
  liboslayer/Selector.hpp
  liboslayer/Semaphore.cpp
  liboslayer/Semaphore.hpp
  liboslayer/Socket.cpp
  liboslayer/Socket.hpp
  liboslayer/SocketApi.cpp
  liboslayer/SocketApi.hpp
  liboslayer/StringElements.cpp
  liboslayer/StringElements.hpp
  liboslayer/Task.hpp
  liboslayer/TaskThreadPool.cpp
  liboslayer/TaskThreadPool.hpp
  liboslayer/TestSuite.cpp
  liboslayer/TestSuite.hpp
  liboslayer/Text.cpp
  liboslayer/Text.hpp
  liboslayer/Thread.cpp
  liboslayer/Thread.hpp
  liboslayer/ThreadPool.cpp
  liboslayer/ThreadPool.hpp
  liboslayer/Timer.cpp
  liboslayer/Timer.hpp
  liboslayer/Tree.cpp
  liboslayer/Tree.hpp
  liboslayer/Uri.cpp
  liboslayer/Uri.hpp
  liboslayer/Uuid.cpp
  liboslayer/Uuid.hpp
  liboslayer/XmlEncoderDecoder.hpp
  liboslayer/XmlParser.hpp
  liboslayer/os.cpp
  liboslayer/os.hpp
  liboslayer/platform.hpp)

# properties
set_target_properties(oslayer PROPERTIES VERSION ${PROJECT_VERSION})
set_target_properties(oslayer PROPERTIES SOVERSION 1)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(oslayer PRIVATE .)

include(GNUInstallDirs)
set(DIR_HEADERS)
list(APPEND DIR_HEADERS
liboslayer/Arguments.hpp
liboslayer/AutoLock.hpp
liboslayer/AutoRef.hpp
liboslayer/AutoRelease.hpp
liboslayer/Base64.hpp
liboslayer/ByteArray.hpp
liboslayer/Condition.hpp
liboslayer/CountDownLatch.hpp
liboslayer/DatabaseConnection.hpp
liboslayer/DatabaseDriver.hpp
liboslayer/DatagramSocket.hpp
liboslayer/Date.hpp
liboslayer/Endian.hpp
liboslayer/Event.hpp
liboslayer/File.hpp
liboslayer/FileStream.hpp
liboslayer/Hash.hpp
liboslayer/Heap.hpp
liboslayer/Iterator.hpp
liboslayer/Library.hpp
liboslayer/Lifetime.hpp
liboslayer/Lisp.hpp
liboslayer/Logger.hpp
liboslayer/MessageQueue.hpp
liboslayer/Mutex.hpp
liboslayer/Network.hpp
liboslayer/Object.hpp
liboslayer/Observer.hpp
liboslayer/Pool.hpp
liboslayer/Process.hpp
liboslayer/Properties.hpp
liboslayer/PropertyMap.hpp
liboslayer/Random.hpp
liboslayer/Ref.hpp
liboslayer/Regex.hpp
liboslayer/Register.hpp
liboslayer/SecureSocket.hpp
liboslayer/Selector.hpp
liboslayer/Semaphore.hpp
liboslayer/Socket.hpp
liboslayer/SocketApi.hpp
liboslayer/StringElements.hpp
liboslayer/Task.hpp
liboslayer/TaskThreadPool.hpp
liboslayer/TestSuite.hpp
liboslayer/Text.hpp
liboslayer/Thread.hpp
liboslayer/ThreadPool.hpp
liboslayer/Timer.hpp
liboslayer/Tokenizer.hpp
liboslayer/Tree.hpp
liboslayer/Uri.hpp
liboslayer/Uuid.hpp
liboslayer/XmlEncoderDecoder.hpp
liboslayer/XmlParser.hpp
liboslayer/os.hpp
liboslayer/platform.hpp
)
set_target_properties(oslayer PROPERTIES PUBLIC_HEADER "${DIR_HEADERS}")
install(TARGETS oslayer
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/liboslayer)

# pkgconfig
set(VERSION "${PROJECT_VERSION}")
set(prefix "${CMAKE_INSTALL_PREFIX}")
set(exec_prefix "\${prefix}")
set(libdir "\${exec_prefix}/${CMAKE_INSTALL_LIBDIR}")
set(includedir "\${prefix}/${CMAKE_INSTALL_INCLUDEDIR}")
configure_file(oslayer.pc.in oslayer.pc @ONLY)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/oslayer.pc" DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/pkgconfig")

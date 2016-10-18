#ifndef __BYTE_BUFFER_HPP__
#define __BYTE_BUFFER_HPP__

#include "os.hpp"

namespace UTIL {

	class ByteBuffer {
	private:
		char * _buffer;
		size_t _position;
		size_t _limit;
		size_t _size;
	private:
		ByteBuffer(const ByteBuffer & other);
		ByteBuffer & operator=(const ByteBuffer & other);
	public:
		ByteBuffer(size_t size);
		virtual ~ByteBuffer();
		char * buffer();
		size_t position();
		void position(size_t pos);
		size_t capacity();
		size_t limit();
		void limit(size_t limit);
		void flip();
		void compact();
		size_t remaining();
		char get();
		void put(char b);
	};
}

#endif

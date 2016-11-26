#include "ByteBuffer.hpp"
#include "os.hpp"

namespace UTIL {

	using namespace OS;

	ByteBuffer::ByteBuffer(size_t size)
		: _delete_on_destruct(true), _position(0), _limit(size), _size(size)
	{
		_buffer = new char[size];
	}
	ByteBuffer::ByteBuffer(char * buffer, size_t size)
		: _delete_on_destruct(false), _buffer(buffer), _position(0), _limit(size), _size(size)
	{
		/**/
	}
	ByteBuffer::~ByteBuffer() {
		if (_delete_on_destruct) {
			delete[] _buffer;
		}
		_buffer = NULL;
	}
	char * ByteBuffer::buffer() {
		return _buffer;
	}
	size_t ByteBuffer::position() {
		return _position;
	}
	void ByteBuffer::position(size_t pos) {
		_position = pos;
	}
	size_t ByteBuffer::capacity() {
		return _size;
	}
	size_t ByteBuffer::limit() {
		return _limit;
	}
	void ByteBuffer::limit(size_t limit) {
		_limit = limit;
	}
	void ByteBuffer::flip() {
		if (_position > 0) {
			_limit = _position;
			_position = 0;
		} else {
			_position = _limit;
			_limit = _size;
		}
	}
	void ByteBuffer::compact() {
		size_t len = remaining();
		for (size_t i = 0; i < len; i++) {
			_buffer[i] = _position + i;
		}
		_position = len;
		_limit = _size;
	}

	size_t ByteBuffer::remaining() {
		if (_position > _limit) {
			return 0;
		}
		return _limit - _position;
	}

	char ByteBuffer::get() {
		if (remaining() <= 0) {
			throw Exception("Underflow");
		}
		return _buffer[_position++];
	}
	void ByteBuffer::put(char b) {
		if (remaining() <= 0) {
			throw Exception("Overflow");
		}
		_buffer[_position++] = b;
	}
}

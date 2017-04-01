#include "ByteArray.hpp"
#include <cstring>

namespace UTIL {

	/**
	 * byte array
	 */

	ByteArray::ByteArray() : _size(0), _array(NULL) {
	}
	ByteArray::ByteArray(size_t size) {
		_alloc(size);
		set(0);
	}
	ByteArray::ByteArray(size_t size, char init) {
		_alloc(size);
		set(init);
	}
	ByteArray::ByteArray(char * array, size_t size) {
		_alloc(size);
		copy(array, size);
	}
	ByteArray::ByteArray(const ByteArray & other) {
		_alloc(other._size);
		copy(other._array, other._size);
	}
	ByteArray::~ByteArray() {
		_release();
	}
	void ByteArray::_alloc(size_t size) {
		_size = size;
		_array = (size == 0 ? NULL : new char[size]);
	}
	void ByteArray::_release() {
		_size = 0;
		if (_array)
		{
			delete[] _array;
			_array = NULL;
		}
	}
	void ByteArray::set(char val) {
		set(val, 0, _size);
	}
	void ByteArray::set(char val, size_t from) {
		set(val, from, _size - from);
	}
	void ByteArray::set(char val, size_t from, size_t count) {
		size_t s = (count < (_size - from) ? count : (_size - from));
		if (s > 0)
		{
			memset(_array + from, val, s);
		}
	}
	size_t ByteArray::copy(const char * array, size_t size) {
		return copy(0, array, size);
	}
	size_t ByteArray::copy(size_t from, const char * array, size_t size) {
		size_t s = (size < (_size - from) ? size : (_size - from));
		if (s > 0)
		{
			memcpy(_array + from, array, s);
		}
		return s;
	}
	size_t ByteArray::copy(const ByteArray & other) {
		return copy(other, other.size());
	}
	size_t ByteArray::copy(const ByteArray & other, size_t count) {
		return copy(0, other, count);
	}
	size_t ByteArray::copy(size_t from, const ByteArray & other) {
		return copy(from, other, other.size());
	}
	size_t ByteArray::copy(size_t from, const ByteArray & other, size_t count) {
		return copy(from, other.const_array(), count);
	}
	size_t ByteArray::size() const {
		return _size;
	}
	char * ByteArray::array() {
		return _array;
	}
	const char * ByteArray::const_array() const {
		return _array;
	}
	ByteArray ByteArray::subarray(size_t from) {
		return subarray(from, _size - from);
	}
	ByteArray ByteArray::subarray(size_t from, size_t count) {
		if (from >= _size)
		{
			throw "out of index";
		}
		size_t s = (count < (_size - from) ? count : (_size - from));
		ByteArray array(s);
		array.copy(_array + from, s);
		return array;
	}
	char & ByteArray::operator[](size_t idx) {
		if (idx >= _size)
		{
			throw "out of index";
		}
		return _array[idx];
	}

	const char ByteArray::operator[](size_t idx) const {
		if (idx >= _size)
		{
			throw "out of index";
		}
		return _array[idx];
	}
	ByteArray & ByteArray::operator=(const ByteArray & other) {
		_release();
		_alloc(other._size);
		copy(other._array, other._size);
		return *this;
	}

	/**
	 * byte array stream
	 */

	ByteArrayStream::ByteArrayStream(ByteArray & array) : _array(array), _position(0) {
	}
	ByteArrayStream::~ByteArrayStream() {
	}
	size_t ByteArrayStream::write(const char * data, size_t count) {
		size_t i = 0;
		for (; i < count && _position < size(); i++, _position++)
		{
			_array[_position] = data[i];
		}
		return i;
	}
	size_t ByteArrayStream::read(char * out, size_t count) {
		size_t i = 0;
		for (; i < count && _position < size(); i++, _position++)
		{
			out[i] = _array[_position];
		}
		return i;
	}
	ByteArray & ByteArrayStream::array() {
		return _array;
	}
	size_t & ByteArrayStream::position() {
		return _position;
	}
	size_t ByteArrayStream::remaining() {
		return size() - _position;
	}
	size_t ByteArrayStream::size() {
		return _array.size();
	}
}

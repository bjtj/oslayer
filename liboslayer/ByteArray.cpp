#include "ByteArray.hpp"
#include <cstring>

namespace osl {

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
	ByteArray::ByteArray(const char * array, size_t size) {
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
	void ByteArrayStream::write_i8(int8_t v) {
		nwrite((const char *)&v, sizeof(int8_t));
	}
	void ByteArrayStream::write_i16(int16_t v) {
		nwrite((const char *)&v, sizeof(int16_t));
	}
	void ByteArrayStream::write_i32(int32_t v) {
		nwrite((const char *)&v, sizeof(int32_t));
	}
	void ByteArrayStream::write_i64(int64_t v) {
		nwrite((const char *)&v, sizeof(int64_t));
	}
	void ByteArrayStream::write_ui8(uint8_t v) {
		nwrite((const char *)&v, sizeof(uint8_t));
	}
	void ByteArrayStream::write_ui16(uint16_t v) {
		nwrite((const char *)&v, sizeof(uint16_t));
	}
	void ByteArrayStream::write_ui32(uint32_t v) {
		nwrite((const char *)&v, sizeof(uint32_t));
	}
	void ByteArrayStream::write_ui64(uint64_t v) {
		nwrite((const char *)&v, sizeof(uint64_t));
	}

#define _READ(T)						   \
	T v = 0;							   \
	nread((char *)&v, sizeof(T));		   \
	return v

	int8_t ByteArrayStream::read_i8() {
		_READ(int8_t);
	}
	int16_t ByteArrayStream::read_i16() {
		_READ(int16_t);
	}
	int32_t ByteArrayStream::read_i32() {
		_READ(int32_t);
	}
	int64_t ByteArrayStream::read_i64() {
		_READ(int64_t);
	}
	uint8_t ByteArrayStream::read_ui8() {
		_READ(uint8_t);
	}
	uint16_t ByteArrayStream::read_ui16() {
		_READ(uint16_t);
	}
	uint32_t ByteArrayStream::read_ui32() {
		_READ(uint32_t);
	}
	uint64_t ByteArrayStream::read_ui64() {
		_READ(uint64_t);
	}
#undef _READ
	
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
	void ByteArrayStream::nwrite(const char * data, size_t count) {
		if (_position + count > _array.size()) {
			throw OverflowException("Overflow exception");
		}
		for (size_t i = 0; i < count && _position < size(); i++, _position++)
		{
			_array[_position] = data[i];
		}
	}
	void ByteArrayStream::nread(char * out, size_t count) {
		if (_position + count > _array.size()) {
			throw UnderflowException("Underflow exception");
		}
		for (size_t i = 0; i < count && _position < size(); i++, _position++)
		{
			out[i] = _array[_position];
		}
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

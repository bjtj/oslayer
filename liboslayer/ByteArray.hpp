#ifndef __BYTE_ARRAY_HPP__
#define __BYTE_ARRAY_HPP__

#include <string>

namespace UTIL {

	/**
	 * byte array
	 */
	
	class ByteArray {
	private:
		size_t _size;
		char * _array;
	public:
		ByteArray();
		ByteArray(size_t size);
		ByteArray(size_t size, char init);
		ByteArray(char * array, size_t size);
		ByteArray(const ByteArray & other);
		virtual ~ByteArray();
	private:
		void _alloc(size_t size);
		void _release();
	public:
		void set(char val);
		void set(char val, size_t from);
		void set(char val, size_t from, size_t count);
		size_t copy(const char * array, size_t size);
		size_t copy(size_t from, const char * array, size_t size);
		size_t copy(const ByteArray & other);
		size_t copy(const ByteArray & other, size_t count);
		size_t copy(size_t from, const ByteArray & other);
		size_t copy(size_t from, const ByteArray & other, size_t count);
		size_t size() const;
		char * array();
		const char * const_array() const;
		ByteArray subarray(size_t from);
		ByteArray subarray(size_t from, size_t count);
		char & operator[](size_t idx);
		const char operator[](size_t idx) const;
		ByteArray & operator=(const ByteArray & other);
	};

	/**
	 * byte array stream
	 */
	class ByteArrayStream {
	private:
		ByteArray & _array;
		size_t _position;
	public:
		ByteArrayStream(ByteArray & array);
		virtual ~ByteArrayStream();
		size_t write(const char * data, size_t count);
		size_t read(char * out, size_t count);
		ByteArray & array();
		size_t & position();
		size_t remaining();
		size_t size();
	};
}

#endif

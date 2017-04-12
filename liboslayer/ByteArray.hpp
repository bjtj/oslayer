#ifndef __BYTE_ARRAY_HPP__
#define __BYTE_ARRAY_HPP__

#include <string>
#include "os.hpp"

namespace UTIL {

	/**
	 * underflow exception
	 */
	DECL_NAMED_EXCEPTION(UnderflowException);
	
	/**
	 * overflow exception
	 */
	DECL_NAMED_EXCEPTION(OverflowException);

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
		void write_i8(int8_t v);
		void write_i16(int16_t v);
		void write_i32(int32_t v);
		void write_i64(int64_t v);
		void write_ui8(uint8_t v);
		void write_ui16(uint16_t v);
		void write_ui32(uint32_t v);
		void write_ui64(uint64_t v);
		int8_t read_i8();
		int16_t read_i16();
		int32_t read_i32();
		int64_t read_i64();
		uint8_t read_ui8();
		uint16_t read_ui16();
		uint32_t read_ui32();
		uint64_t read_ui64();
		size_t write(const char * data, size_t count);
		size_t read(char * out, size_t count);
		void nwrite(const char * data, size_t count);
		void nread(char * out, size_t count);
		ByteArray & array();
		size_t & position();
		size_t remaining();
		size_t size();
	};
}

#endif

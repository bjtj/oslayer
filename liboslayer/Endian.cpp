#include "Endian.hpp"

namespace OS {

	/**
	 * @brief endian
	 */
	Endian::Endian(int endian) : _endian(endian) {
		if (_endian != BE && _endian != LE) {
			throw UnknownEndianException("unknow endian exception");
		}
	}
	Endian::~Endian() {
	}
	Endian Endian::getNativeEndian() {
        uint16_t v = 0x0102; // note) use variable to avoid dead code warning
		return (((v >> 8) == 0x01) ? Endian(BE) : Endian(LE));
	}
	bool Endian::is_be() const {
		return (_endian == BE);
	}
	bool Endian::is_le() const {
		return (_endian == LE);
	}

	/**
	 * @brief big endian
	 */

	BigEndian::BigEndian() : Endian(Endian::BE) {
	}
    BigEndian::~BigEndian() {
	}

	/**
	 * @brief little endian
	 */
	
	LittleEndian::LittleEndian() : Endian(Endian::LE) {
	}
    LittleEndian::~LittleEndian() {
	}

	/**
	 * @brief endian converter
	 */

	EndianConverter::EndianConverter()
		: _base_endian(Endian::getNativeEndian()) {
	}
	EndianConverter::EndianConverter(const Endian & base_endian)
		: _base_endian(base_endian) {
	}
	EndianConverter::~EndianConverter() {
	}
	Endian & EndianConverter::base_endian() {
		return _base_endian;
	}
	uint16_t EndianConverter::swap16(uint16_t v) {
		return
			((v >> 8) & 0xff) |
			((v << 8) & 0xff00);
	}
	uint32_t EndianConverter::swap32(uint32_t v) {
		return
			((v >> 24) & 0xff) |
			((v >> 16) & 0xff00) |
			((v << 8) & 0xff0000) |
			((v << 24) & 0xff000000);
	}
	uint64_t EndianConverter::swap64(uint64_t v) {
		return
			((v >> 56) & 0xffULL) |
			((v >> 48) & 0xff00ULL) |
			((v >> 40) & 0xff0000ULL) |
			((v >> 32) & 0xff000000ULL) |
			((v <<  8) & 0xff00000000ULL) |
			((v << 24) & 0xff0000000000ULL) |
			((v << 40) & 0xff000000000000ULL) |
			((v << 56) & 0xff00000000000000ULL);
	}
	uint16_t EndianConverter::to_le16(uint16_t v) {
		return (_base_endian.is_be() ? swap16(v) : v);
	}
	uint32_t EndianConverter::to_le32(uint32_t v) {
		return (_base_endian.is_be() ? swap32(v) : v);
	}
	uint64_t EndianConverter::to_le64(uint64_t v) {
		return (_base_endian.is_be() ? swap64(v) : v);
	}
	uint16_t EndianConverter::to_be16(uint16_t v) {
		return (_base_endian.is_le() ? swap16(v) : v);
	}
	uint32_t EndianConverter::to_be32(uint32_t v) {
		return (_base_endian.is_le() ? swap32(v) : v);
	}
	uint64_t EndianConverter::to_be64(uint64_t v) {
		return (_base_endian.is_le() ? swap64(v) : v);
	}
}

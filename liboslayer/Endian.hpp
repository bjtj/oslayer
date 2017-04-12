#ifndef __ENDIAN_HPP__
#define __ENDIAN_HPP__

#include "os.hpp"

namespace OS {

	/**
	 * @brief unknown endian contant exception
	 */
	DECL_NAMED_EXCEPTION(UnknownEndianException);

	/**
	 * @brief endian
	 */
	class Endian
	{
	public:
		static const int BE = 0;
		static const int LE = 1;
	private:
		int _endian;
	public:
		Endian(int endian);
		virtual ~Endian();
		static Endian getNativeEndian();
		bool is_be() const;
		bool is_le() const;
	};

	/**
	 * @brief big endian
	 */
	class BigEndian : public Endian
	{
	public:
		BigEndian();
		virtual ~BigEndian();
	};
	
	/**
	 * @brief little endian
	 */
	class LittleEndian : public Endian
	{
	public:
		LittleEndian();
		virtual ~LittleEndian();
	};

	/**
	 * @brief endian covnverter
	 */
	class EndianConverter
	{
	private:
		Endian _base_endian;
	public:
		EndianConverter();
		EndianConverter(const Endian & base_endian);
		virtual ~EndianConverter();
		Endian & base_endian();
		static uint16_t swap16(uint16_t v);
		static uint32_t swap32(uint32_t v);
		static uint64_t swap64(uint64_t v);
		uint16_t to_le16(uint16_t v);
		uint32_t to_le32(uint32_t v);
		uint64_t to_le64(uint64_t v);
		uint16_t to_be16(uint16_t v);
		uint32_t to_be32(uint32_t v);
		uint64_t to_be64(uint64_t v);
	};

}

#endif

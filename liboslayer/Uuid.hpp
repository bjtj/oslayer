#ifndef __UUID_HPP__
#define __UUID_HPP__

#include <string>
#include <vector>
#include <stdint.h>

namespace UTIL {
    
	/**
	 * @brief uuid
	 * @ref https://tools.ietf.org/html/rfc4122 (there's sample source)
	 * @ref https://en.wikipedia.org/wiki/Universally_unique_identifier
	 */
    class Uuid {
    private:
        std::string uuid;
        std::string rest;
    public:
        Uuid(const std::string & uuid);
        virtual ~Uuid();
        void clear();
        void parse(const std::string & uuid);
        std::string getUuid() const;
        std::string getRest() const;
        void setUuid(const std::string & uuid);
        void setRest(const std::string & rest);
        static void testValidFormat(const std::string & uuid);
        std::string toString() const;
    };

	/**
	 * @brief
	 */
	class UuidGenerator {
	private:
	public:
		UuidGenerator();
		virtual ~UuidGenerator();
		virtual std::string generate() = 0;
	};

	/**
	 * @brief 
	 */
	class UuidGeneratorVersion1 : public UuidGenerator {
	private:
		uint16_t _clock_seq;
		std::vector<uint8_t> _nodes;
	public:
		UuidGeneratorVersion1();
		virtual ~UuidGeneratorVersion1();
		virtual std::string generate();
		uint16_t & clock_seq();
		std::vector<uint8_t> & nodes();
	};
}

#endif

#include "Uuid.hpp"
#include "os.hpp"
#include "Date.hpp"
#include "Text.hpp"

namespace UTIL {
    
    using namespace std;
    using namespace OS;
    
    DECL_NAMED_EXCEPTION(WrongFormatException);
    
    Uuid::Uuid(const string & uuid) {
        parse(uuid);
    }
    Uuid::~Uuid() {
    }
    
    void Uuid::clear() {
        uuid.clear();
        rest.clear();
    }
    
    void Uuid::parse(const string & uuid) {

		size_t offset = 5;
        clear();
        
        if (!Text::startsWith(uuid, "uuid:")) {
			offset = 0;
        }
        
        size_t sep = uuid.find("::");
        if (sep != string::npos) {
            this->uuid = uuid.substr(offset, sep - offset);
            this->rest = uuid.substr(sep + 2);
        } else {
            this->uuid = uuid.substr(offset);
        }
    }
    
    string Uuid::getUuid() const {
        return uuid;
    }
    
    string Uuid::getRest() const {
        return rest;
    }
    
    void Uuid::setUuid(const std::string & uuid) {
        this->uuid = uuid;
    }
    
    void Uuid::setRest(const std::string & rest) {
        this->rest = rest;
    }
    
    void Uuid::testValidFormat(const std::string & uuid) {
        
        if (uuid.empty()) {
            throw WrongFormatException("wrong format/empty string", -1, 0);
        }
        
        vector<string> parts = Text::split(uuid, "-");
        if (parts.size() != 5) {
            throw WrongFormatException("wrong format", -1, 0);
        }
        
        if (parts[0].length() != 8) {
            throw WrongFormatException("wrong format", -1, 0);
        }
        if (parts[1].length() != 4) {
            throw WrongFormatException("wrong format", -1, 0);
        }
        if (parts[2].length() != 4) {
            throw WrongFormatException("wrong format", -1, 0);
        }
        if (parts[3].length() != 4) {
            throw WrongFormatException("wrong format", -1, 0);
        }
        if (parts[4].length() != 12) {
            throw WrongFormatException("wrong format", -1, 0);
        }
        
        for (size_t i = 0; i < parts.size(); i++) {
            if (parts[i].find_first_not_of("0123456789abcdefABCDEF") != string::npos) {
                throw WrongFormatException("wrong format/hex digit", -1, 0);
            }
        }
    }
    
    string Uuid::toString() const {
        return "uuid:" + uuid + (rest.empty() ? "" : "::" + rest);
    }


	/**
	 * @brief 
	 */
	UuidGenerator::UuidGenerator() {
	}
	UuidGenerator::~UuidGenerator() {
	}
	

	/**
	 * @brief 
	 */
	UuidGeneratorVersion1::UuidGeneratorVersion1()
		: _clock_seq(0), _nodes(6) {
	}
	UuidGeneratorVersion1::~UuidGeneratorVersion1() {
	}
	string UuidGeneratorVersion1::generate() {
		
		osl_time_t time = osl_get_time_unix();

		/**
		 * Unix base time (Jan. 1, 1970) to UUID base time (Oct. 15, 1582)
		 */
		uint64_t uuid_time = ((uint64_t)time.sec * 10000000)
			+ ((uint64_t)time.nano * 10)
			+ 0x01B21DD213814000LL;

		uint32_t part1 = (uint32_t)(uuid_time & 0xffffffff);
		uint16_t part2 = (uint16_t)((uuid_time >> 32) & 0xffff);
		uint16_t part3 = (uint16_t)(((uuid_time >> 48) & 0x0fff) | 0x1fff);
		uint16_t part4 = (uint16_t)((_clock_seq & 0x3fff) | 0x8000);

		_clock_seq++;

		string uuid = Text::format("%8.8x-%4.4x-%4.4x-%4.4x-", part1, part2, part3, part4);
		for (size_t i = 0; i < 6; i++) {
			if (i < _nodes.size()) {
				uuid += Text::format("%2.2x", _nodes[i]);
			} else {
				uuid += "00";
			}
		}
		
		return uuid;
	}
	uint16_t & UuidGeneratorVersion1::clock_seq() {
		return _clock_seq;
	}
	std::vector<uint8_t> & UuidGeneratorVersion1::nodes() {
		return _nodes;
	}
}

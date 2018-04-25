#ifndef __XML_ENCODER_DECODER_HPP__
#define __XML_ENCODER_DECODER_HPP__

namespace osl {

	/**
	 *
	 */
	class XmlEncoder {
	private:
	public:
		XmlEncoder() {}
		virtual ~XmlEncoder() {}
		static std::string encode(const std::string & text) {
			std::string ret;
			for (std::string::const_iterator iter = text.begin(); iter != text.end(); iter++) {
				switch (*iter) {
				case '&':
					ret.append("&amp;");
					break;
				case '<':
					ret.append("&lt;");
					break;
				case '>':
					ret.append("&gt;");
					break;
				default:
					ret.append(1, *iter);
					break;
				}
			}
			return ret;
		}
	};

	/**
	 *
	 */
	class XmlDecoder {
	private:
	public:
		XmlDecoder() {}
		virtual ~XmlDecoder() {}
		static std::string decode(const std::string & text) {
			std::string ret;
			for (std::string::const_iterator iter = text.begin(); iter != text.end(); iter++) {
				switch (*iter) {
				case '&':
				{
					iter++;
					std::string enc;
					for (; *iter != ';'; iter++) {
						if (iter == text.end()) {
							throw Exception("unexpected end of string");
						}
						enc.append(1, *iter);
					}

					if (enc == "amp") {
						ret.append(1, '&');
					} else if (enc == "lt") {
						ret.append(1, '<');
					} else if (enc == "gt") {
						ret.append(1, '>');
					} else {
						// unknown
					}
				}
				break;
				default:
					ret.append(1, *iter);
					break;
				}
			}
			return ret;
		}
	};
}

#endif

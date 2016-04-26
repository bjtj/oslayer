#include "Base64.hpp"
#include <cstdlib>
#include <cstring>

namespace UTIL {

	using namespace std;

	static const int s_decode_table[] = {
		64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
		64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 
		64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 62, 64, 64, 64, 63,
		52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 64, 64, 64, 64, 64, 64, 
		64,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 
		15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 64, 64, 64, 64, 64, 
		64, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 
		41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 64, 64, 64, 64, 64, 
		64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 
		64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 
		64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 
		64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 
		64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
		64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
		64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
		64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 
	};

	static size_t s_decode_len(const char * encoded) {
		const char * p = encoded;
		int len = 0;
		while (s_decode_table[(unsigned)*p++] < 64) {len++;}
		return (len % 4 == 0) ? (len / 4 * 3) : ((len / 4 * 3) + (len % 4 - 1));
	}

	static size_t s_decode(char * out, const char * encoded) {
		char * o = out;
		const char * p = encoded;
		int len = 0;
		int i = 0;
		while (s_decode_table[(unsigned)*p++] < 64) {len++;}
		p = encoded;
		for (i = 0; i < len - 3; i += 4, p += 4) {
			*o++ = (s_decode_table[(unsigned)*(p+0)] << 2) | (s_decode_table[(unsigned)*(p+1)] >> 4);
			*o++ = (s_decode_table[(unsigned)*(p+1)] << 4) | (s_decode_table[(unsigned)*(p+2)] >> 2);
			*o++ = (s_decode_table[(unsigned)*(p+2)] << 6) | (s_decode_table[(unsigned)*(p+3)]);
		}
	
		if (len - i > 1) {
			*o++ = (s_decode_table[(unsigned)*(p+0)] << 2) | (s_decode_table[(unsigned)*(p+1)] >> 4);
		}
		if (len - i > 2) {
			*o++ = (s_decode_table[(unsigned)*(p+1)] << 4) | (s_decode_table[(unsigned)*(p+2)] >> 2);
		}
		if (len - i > 3) {
			*o++ = (s_decode_table[(unsigned)*(p+2)] << 6) | (s_decode_table[(unsigned)*(p+3)]);
		}

		*o++ = '\0';
		return o - out;
	}

	static const char s_basis64[] =
		"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

	static size_t s_encode_len(size_t len) {
		return ((len + 2) / 3 * 4);
	}

	static size_t s_encode(char * out, const char * data, size_t len) {

		size_t i;
		char * p = out;
		for (i = 0; i < len - 2; i += 3) {
			*p++ = s_basis64[(data[i] >> 2) & 0x3f];
			*p++ = s_basis64[((data[i] & 0x03) << 4) | ((data[i + 1] & 0xf0) >> 4)];
			*p++ = s_basis64[((data[i + 1] & 0x0f) << 2) | ((data[i + 2] & 0xc0) >> 6)];
			*p++ = s_basis64[data[i + 2] & 0x3f];
		}

		if (i < len) {
			*p++ = s_basis64[(data[i] >> 2) & 0x3f];
			if (i == (len - 1)) {
				*p++ = s_basis64[(data[i] & 0x03) << 4];
				*p++ = '=';
			} else {
				*p++ = s_basis64[((data[i] & 0x03) << 4) | ((data[i + 1] & 0xf0) >> 4)];
				*p++ = s_basis64[(data[i + 1] & 0x0f) << 2];
			}
			*p++ = '=';
		}
		*p++ = '\0';
		return p - out;
	}

	Base64::Base64() {
	}
	
	Base64::~Base64() {
	}

	string Base64::encode(const string & plain) {
		size_t len = s_encode_len(plain.length());
		char * buffer = (char*)malloc(len + 1);
		memset(buffer, 0, len + 1);
		s_encode(buffer, plain.c_str(), plain.length());
		string encoded(buffer);
		free(buffer);
		return encoded;
	}
	
	string Base64::decode(const string & encoded) {
		size_t len = s_decode_len(encoded.c_str());
		char * buffer = (char*)malloc(len + 1);
		memset(buffer, 0, len + 1);
		s_decode(buffer, encoded.c_str());
		string decoded(buffer);
		free(buffer);
		return decoded;
	}
}


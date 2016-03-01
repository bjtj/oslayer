#include <iostream>
#include <string>
#include <liboslayer/Base64.hpp>
#include <cstdio>
#include <cstring>

using namespace std;
using namespace UTIL;

int main(int argc, char *args[]) {

	while (1) {
		char buffer[1024] = {0,};
		if (fgets(buffer, sizeof(buffer), stdin)) {
			buffer[strlen(buffer) - 1] = 0;
			if (!strcmp(buffer, "q")) {
				break;
			}
			string encoded = Base64::encode(buffer);
			cout << "enc: " << encoded << endl;
			cout << "dec: " << Base64::decode(encoded) << endl;
		}
		
	}
	
    return 0;
}

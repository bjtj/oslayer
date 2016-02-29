#include <iostream>
#include <liboslayer/os.hpp>
#include <liboslayer/Hash.hpp>

using namespace std;

void printHash(UTIL::Hash & hash, const char * str) {
	unsigned long h = hash.hash(str);
	cout << str << " : " << h << endl;
}

int main(int argc, char *args[]) {
	
	UTIL::SimpleHash hash(1111);

	printHash(hash, "hello");
	printHash(hash, "");
	printHash(hash, "0");
	printHash(hash, "1");
	printHash(hash, "2");
	printHash(hash, "1234656");

	hash.setSeed(OS::tick_milli());

	printHash(hash, "hello");
	printHash(hash, "");
	printHash(hash, "0");
	printHash(hash, "1");
	printHash(hash, "2");
	printHash(hash, "1234656");
	
    return 0;
}

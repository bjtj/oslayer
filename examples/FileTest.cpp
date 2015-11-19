#include <iostream>
#include <liboslayer/os.hpp>
#include <liboslayer/Text.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

string s_killobyte_string(filesize_t size_in_byte) {
	filesize_t kb = size_in_byte / 1024;
	return Text::toString(kb);
}

string s_megabyte_string(filesize_t size_in_byte) {
	filesize_t mb = size_in_byte / (1024 * 1024);
	return Text::toString(mb);
}

string s_gigabyte_string(filesize_t size_in_byte) {
	filesize_t mb = size_in_byte / (1024 * 1024 * 1024);
	return Text::toString(mb);
}

void s_test(const char * c_path) {

	string path(c_path);
	File file(path);
	if (!file.exists()) {
		cout << "File does not exists" << endl;
		return;
	}

	filesize_t size = file.getSize();
	cout << "File size: " << size << " Bytes" << endl;
	cout << "File size: " << s_killobyte_string(size) << " KB" << endl;
	cout << "File size: " << s_megabyte_string(size) << " MB" << endl;
	cout << "File size: " << s_gigabyte_string(size) << " GB" << endl;
	
}

int main(int argc, char * args[]) {

	if (argc <= 1) {
		cout << "File path required" << endl;
	} else {
		s_test(args[1]);
	}
    
	getchar();

	return 0;
}
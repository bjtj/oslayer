#include <iostream>
#include <liboslayer/os.hpp>
#include <liboslayer/FileReaderWriter.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

size_t readline(char * buffer, size_t max) {
	fgets(buffer, (int)max - 1, stdin);
	buffer[strlen(buffer) - 1] = 0;
	return strlen(buffer);
}

void read_file(const char * filename) {
	File file(filename);
	FileReader reader(file);

	char buffer[1024] = {0,};
	size_t len;
	while ((len = reader.read(buffer, sizeof(buffer))) > 0) {
		cout << "READ: " << string(buffer, len) << endl;
	}

	reader.close();
}

void write_file(const char * filename) {
	File file(filename);
	FileWriter writer(file);

	writer.write("hello", 5);

	writer.close();
}

int main(int argc, char * args[]) {

	cout << File::getCwd() << endl;
	
	write_file("test.txt");
	read_file("test.txt");

	getchar();

	return 0;
}
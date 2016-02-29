#include <iostream>
#include <liboslayer/FileReaderWriter.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

int main(int argc, char *args[]) {

	File file("hello.txt");
	RandomAccessFile rf(file, "wb+");

	rf.write("hello", 5);

	rf.seek(0);

	char buffer[1024] = {0,};
	rf.read(buffer, sizeof(buffer));
	cout << buffer << endl;
    
    return 0;
}

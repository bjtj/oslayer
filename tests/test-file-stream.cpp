#include <liboslayer/FileStream.hpp>
#include "utils.hpp"

using namespace std;
using namespace UTIL;

static void test_filestream() {
	FileStream fs;
	fs.open("test-fs.txt", "wb");
	fs.writeline("hello world");
	fs.writeline("byebye");
	fs.close();

	string line;
	fs.open("test-fs.txt", "rb");
	line = fs.readline();
	ASSERT(line, ==, "hello world");
	line = fs.readline();
	ASSERT(line, ==, "byebye");
	fs.close();

	fs.open("test-fs.txt", "ab+");
	fs.seek(0);
	line = fs.readline();
	ASSERT(line, ==, "hello world");
	line = fs.readline();
	ASSERT(line, ==, "byebye");
	fs.rewind();
	line = fs.readline();
	ASSERT(line, ==, "hello world");
	line = fs.readline();
	ASSERT(line, ==, "byebye");
	fs.write("howdy", 5);
	fs.seek(fs.position() - 5);
	line = fs.readline();
	ASSERT(line, ==, "howdy");
	ASSERT(fs.eof(), ==, true);
	fs.write("last chance", strlen("last chance"));
	fs.seek(fs.position() - strlen("last chance"));
	ASSERT(fs.eof(), ==, false);
	char buffer[10] = {0,};
	int len = fs.read(buffer, sizeof(buffer) - 1);
	ASSERT(len, ==, sizeof(buffer) - 1);
	ASSERT(string(buffer), ==, "last chan");
	int ch = fs.read();
	ASSERT(ch, ==, 'c');
	ch = fs.read();
	ASSERT(ch, ==, 'e');
	ch = fs.read();
	ASSERT(ch, ==, -1);
	fs.close();

	{
		FILE * fp = fopen("test-fs.txt", "rb");
		FileStream fs(fp);
		line = fs.readline();
		ASSERT(line, ==, "hello world");
		fs.close();
	}

	{
		FileStream fs(fopen("test-fs.txt", "rb"));
		line = fs.readline();
		ASSERT(line, ==, "hello world");
		fs.close();
	}

}

int main(int argc, char *args[]) {

	test_filestream();
    
    return 0;
}

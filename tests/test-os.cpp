#include <iostream>
#include <liboslayer/os.hpp>
#include <liboslayer/FileReaderWriter.hpp>
#include "utils.hpp"

using namespace std;
using namespace OS;
using namespace UTIL;

static void test_file_io() {

	File file("big");

	FileWriter writer(file);
	string text = "abcdefghijklmnopqrstuvwxyz";
	ASSERT(writer.write(text.c_str(), text.length()), ==, text.length());
	writer.close();
	
	RandomAccessFile rf(file, "rb+");
	char buffer[1024] = {0,};
	rf.read(buffer, sizeof(buffer));
	ASSERT(string(buffer), ==, "abcdefghijklmnopqrstuvwxyz");

	rf.seek(5);
	memset(buffer, 0, sizeof(buffer));
	rf.read(buffer, sizeof(buffer));
	ASSERT(string(buffer), ==, "fghijklmnopqrstuvwxyz");
	rf.close();
}

DECL_NAMED_ONLY_EXCEPTION(MyException);

static void test_exception() {
	Exception ex("construct test");
	MyException mex("my exception contruct test");

	ASSERT(ex.getMessage(), ==, "construct test");
	ASSERT(mex.getMessage(), ==, "my exception contruct test");

	ASSERT(ex.getErrorCode(), ==, -1);
	ASSERT(mex.getErrorCode(), ==, -1);
}

int main(int argc, char *args[]) {

	test_file_io();
	test_exception();
    
    return 0;
}

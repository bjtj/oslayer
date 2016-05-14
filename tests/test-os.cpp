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

static void test_path() {
	ASSERT(File::getPathPart("file.mp4"), ==, "");
	ASSERT(File::getPathPart("/path/to/file.mp4"), ==, "/path/to/");
	ASSERT(File::getPathPart("/file.mp4"), ==, "/");
	ASSERT(File::mergePaths("/", "file.mp4"), ==, "/file.mp4");
	ASSERT(File::mergePaths("./", "file.mp4"), ==, "./file.mp4");
	ASSERT(File::mergePaths(".", "file.mp4"), ==, "./file.mp4");
	ASSERT(File::mergePaths("", "file.mp4"), ==, "file.mp4");
}

int main(int argc, char *args[]) {

	test_file_io();
	test_path();
    
    return 0;
}

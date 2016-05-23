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

	ASSERT(File::getExtension("."), ==, "");
	ASSERT(File::getExtension(".."), ==, "");

	ASSERT(File::getFileName("."), ==, ".");
	ASSERT(File::getFileName(".."), ==, "..");

	ASSERT(File::getFileName(""), ==, "");
	ASSERT(File::getFileName("file.mp4"), ==, "file.mp4");
	ASSERT(File::getFileName("./videos/file.mp4"), ==, "file.mp4");
	ASSERT(File::getFileNameWithoutExtension(".file.mp4"), ==, ".file");
	ASSERT(File::getFileNameWithoutExtension("file.mp4"), ==, "file");
	ASSERT(File::getFileNameWithoutExtension("file.orig.mp4"), ==, "file.orig");
	ASSERT(File::getFileNameWithoutExtension("./videos/file.mp4"), ==, "file");
	ASSERT(File::getFileNameWithoutExtension("./videos/.file.mp4"), ==, ".file");
	ASSERT(File::getFileNameWithoutExtension("./videos/.file.orig.mp4"), ==, ".file.orig");
	
	ASSERT(File::getDirectory("file.mp4"), ==, "");
	ASSERT(File::getDirectory("/path/to/file.mp4"), ==, "/path/to/");
	ASSERT(File::getDirectory("/file.mp4"), ==, "/");
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

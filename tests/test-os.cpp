#include <iostream>
#include <liboslayer/os.hpp>
#include <liboslayer/FileReaderWriter.hpp>
#include "utils.hpp"

using namespace std;
using namespace OS;
using namespace UTIL;

static void test_time() {
	osl_time_t ti = osl_get_time();
	printf("%lu.%lu (0x%lx.0x%lx)\n", ti.sec, ti.nano, ti.sec, ti.nano);
}

static void test_file() {

	cout << File::getCwd() << endl;

	ASSERT(system("touch xxxxxx"), ==, 0);

	File file("./xxxxxx");

	string c = file.getCreationDate();
	cout << c << endl;

	cout << Date::format("%Y-%c-%d %H:%i:%s", Date::now()) << endl;

#if defined(USE_MS_WIN)
	ASSERT(system("del xxxxxx"), ==, 0);
#else
	ASSERT(system("rm xxxxxx"), ==, 0);
#endif
}

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

	string path = File::mergePaths(".", "file.mp4");

	ASSERT((path == "./file.mp4" || path == ".\\file.mp4"), ==, true);
	ASSERT(File::mergePaths("", "file.mp4"), ==, "file.mp4");
}

static void test_library() {
#if defined(USE_MS_WIN)

	Library lib("..\\Debug", "libhello");

	/*void (*func)() = (void (*)())lib.getSymbol("hello").getHandle();
	func();*/
	lib.getSymbol("hello").asFunc()();
#endif
}

int main(int argc, char *args[]) {

	test_time();
	test_file();
	test_file_io();
	test_path();
	test_library();
    
    return 0;
}

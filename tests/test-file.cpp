#include <liboslayer/TestSuite.hpp>
#include <liboslayer/File.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;


class FileTestCase : public TestCase {
public:
    FileTestCase() : TestCase("file-test-case") {
	}
    virtual ~FileTestCase() {
	}
	virtual void test() {
		cout << File::getCwd() << endl;

		testFile();
		testPath();
	}

	void testFile() {
		ASSERT(system("touch xxxxxx"), ==, 0);

		File file("./xxxxxx");

		string c = Date::format(file.creationDate());
		cout << c << endl;

		cout << Date::format(Date::now(), "%Y-%c-%d %H:%i:%s") << endl;

		File dir("./dir");

		ASSERT(dir.exists(), ==, false);
		dir.mkdir();
		ASSERT(dir.exists(), ==, true);

#if defined(USE_MS_WIN)
		ASSERT(system("del xxxxxx"), ==, 0);
		ASSERT(system("rmdir dir"), ==, 0);
#else
		ASSERT(system("rm xxxxxx"), ==, 0);
		ASSERT(system("rm -rf dir"), ==, 0);
#endif
	}

	void testPath() {
		ASSERT(File::getDirectory(""), ==, "");
		ASSERT(File::getDirectory("/"), ==, "/");
		ASSERT(File::getDirectory("/hello/"), ==, "/hello/");
		ASSERT(File::getDirectory("/hello"), ==, "/");

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

		cout << File::getAbsolutePath("..") << endl;
		ASSERT(File::getAbsolutePath("."), ==, File::getCwd());
		ASSERT(File::getAbsolutePath("./not exists"), ==, File::getCwd() + File::getSeparators()[0] + "not exists");
	}
};


int main(int argc, char *argv[])
{
	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new FileTestCase));
	TestReport report(ts.testAll());
	ASSERT(report.failed(), ==, 0);
	
    return 0;
}

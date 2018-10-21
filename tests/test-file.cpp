#include <liboslayer/TestSuite.hpp>
#include <liboslayer/File.hpp>

using namespace std;
using namespace osl;



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
	ASSERT(File::dirname(""), ==, "");
	ASSERT(File::dirname("/"), ==, "/");
	ASSERT(File::dirname("/hello/"), ==, "/hello/");
	ASSERT(File::dirname("/hello"), ==, "/");

	ASSERT(File::extension("."), ==, "");
	ASSERT(File::extension(".."), ==, "");

	ASSERT(File::basename("."), ==, ".");
	ASSERT(File::basename(".."), ==, "..");

	ASSERT(File::basename(""), ==, "");
	ASSERT(File::basename("file.mp4"), ==, "file.mp4");
	ASSERT(File::basename("./videos/file.mp4"), ==, "file.mp4");
	ASSERT(File::name(".file.mp4"), ==, ".file");
	ASSERT(File::name("file.mp4"), ==, "file");
	ASSERT(File::name("file.orig.mp4"), ==, "file.orig");
	ASSERT(File::name("./videos/file.mp4"), ==, "file");
	ASSERT(File::name("./videos/.file.mp4"), ==, ".file");
	ASSERT(File::name("./videos/.file.orig.mp4"), ==, ".file.orig");
	
	ASSERT(File::dirname("file.mp4"), ==, "");
	ASSERT(File::dirname("/path/to/file.mp4"), ==, "/path/to/");
	ASSERT(File::dirname("/file.mp4"), ==, "/");
	ASSERT(File::merge("/", "file.mp4"), ==, "/file.mp4");
	ASSERT(File::merge("./", "file.mp4"), ==, "./file.mp4");

	string path = File::merge(".", "file.mp4");

	ASSERT((path == "./file.mp4" || path == ".\\file.mp4"), ==, true);
	ASSERT(File::merge("", "file.mp4"), ==, "file.mp4");

	cout << File::absolutePath("..") << endl;
	ASSERT(File::absolutePath("."), ==, File::getCwd());
	ASSERT(File::absolutePath("./not exists"), ==, File::getCwd() + File::separators()[0] + "not exists");
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

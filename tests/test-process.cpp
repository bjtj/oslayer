#include <iostream>
#include <liboslayer/Process.hpp>
#include <liboslayer/FileStream.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

#define ASSERT(A,CMP,B)													\
	cout << #A << " (" << #CMP << " " << B << ") :: ";					\
	if (!(A CMP B)) {													\
		cout << " - FAIL" << endl;										\
		cerr << " <!> " << #A <<  " should be " << #CMP << " " <<  B << " but " << A << endl; \
		exit(1);														\
	} else {															\
		cout << " - PASS" << endl;										\
	}

static void s_system(const char * cmd) {
	if (system(cmd)) {}
}

static void test_process() {
#ifdef USE_MS_WIN
	s_system("rmdir /S /Q .process-test");
	s_system("mkdir .process-test");
	s_system("echo test >> .process-test\\a.txt");
	s_system("echo test >> .process-test\\b.txt");
	Process p("dir .process-test");
#else
	s_system("rm -rf .process-test");
	s_system("mkdir .process-test");
	s_system("touch .process-test/a.txt");
	s_system("touch .process-test/b.txt");
	Process p("ls -asl .process-test");
#endif	
	
	p.start();
	cout << "OUT > " << endl;
	FileStream out(p.out());
	while (!out.eof()) {
		cout << out.readline() << endl;
	}
	cout << "ERR > " << endl;
	FileStream err(p.err());
	while (p.exited() == false && !err.eof()) {
		cout << err.readline() << endl;
	}

	cout << "wait..." << endl;
	p.wait();
	ASSERT(p.exitCode(), ==, 0);
	p.close();
#ifdef USE_MS_WIN
	s_system("rmdir /S /Q .process-test");
#else
	s_system("rm -rf .process-test");
#endif
}

int main(int argc, char *args[]) {

#ifdef USE_MS_WIN
	s_system("echo %cd%");
#else
	s_system("pwd");
#endif

	try {
		test_process();
	} catch (Exception e) {
		cerr << "error... " << e.what() << endl;
		exit(1);
	}
	
	cout << "Done" << endl;
    return 0;
}

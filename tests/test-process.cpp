#include <iostream>
#include <liboslayer/Process.hpp>

using namespace std;
using namespace OS;

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
	
	s_system("rm -rf .process-test");
	s_system("mkdir .process-test");
	s_system("touch .process-test/a.txt");
	s_system("touch .process-test/b.txt");

	char buffer[1024] = {0,};
	Process p("ls -asl *.o");
	p.start();

	cout << "OUT > " << endl;
	while (p.read(buffer, sizeof(buffer)) > 0) {
		cout << buffer << endl;
	}

	cout << "ERR > " << endl;
	while (p.readerr(buffer, sizeof(buffer)) > 0) {
		cout << buffer << endl;
	}

	p.wait();

	ASSERT(p.exitCode(), ==, 0);

	p.close();

	s_system("rm -rf .process-test");
}

int main(int argc, char *args[]) {

	test_process();

	cout << "Done" << endl;
    
    return 0;
}

#include <liboslayer/os.hpp>
#include <liboslayer/Measurement.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

void s_test() {

	Measurement m;

	m.pin();
	idle(100);
	printf("%lu\n", m.collect());

	m.pin();
	idle(200);
	printf("%lu\n", m.collect());

	m.pin();
	idle(110);
	printf("%lu\n", m.collect());
}

int main(int argc, char * args[]) {

	s_test();

	getchar();

	return 0;
}
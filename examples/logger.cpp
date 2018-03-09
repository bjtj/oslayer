// WARNING) Not working code --
#include <liboslayer/File.hpp>
#include <liboslayer/Logger.hpp>

using namespace OS;
using namespace logger;

void foo(AutoRef<Logger> logger) {
	logger->logd("hello in foo");
}

int main(int argc, char *argv[])
{
	logd("hello");

	setLogOutput(File("log.out", "wb"));
	setLogLevel(Level::DEBUG);
	setLogFormat("[$date] $msg")
	
	logd("hello world");

	foo(AutoRef<Logger>(new Logger("foo.log")));
	
    return 0;
}

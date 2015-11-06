#include <liboslayer/Logger.hpp>

using namespace UTIL;

int main(int argc, char * args[]) {

	const Logger & logger = LoggerFactory::getDefaultLogger();

	logger.log(LogLevel::DEBUG_LEVEL, "hello");
	logger.loge("hello");
	logger.logw("hello");
	logger.logi("hello");
	logger.logd("hello");
	logger.logv("hello");

	getchar();

	return 0;
}
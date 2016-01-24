#include <liboslayer/os.hpp>
#include <liboslayer/Logger.hpp>

using namespace OS;
using namespace UTIL;

int main(int argc, char * args[]) {
    
    const Logger & logger = LoggerFactory::getDefaultLogger();
    logger.logd("hello");
    
    return 0;
}
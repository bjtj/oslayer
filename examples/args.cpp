// WARNING) Not working code --
#include <liboslayer/ArgumentParser.hpp>

using namespace OS;
using namespace args;

class ParserErrorHandler: public ArgumentParseErrorHandler
{
public:
    ParserErrorHandler() {
	}
    virtual ~ParserErrorHandler() {
	}
	virtual bool handleError(ParseError error) {
		return false;
	}
};


int main(int argc, char *argv[])
{
	ArgumentParser parser = ArgumentParser::getDefaultParser();
	ArgumentDescription description;
	description.addArgument(Options::MUST, "base-path", Format::PATH, "./", "set base path (default './')");
	description.addArgument(Options::MUST, "target-path", Format::PATH, "./", "set base path (default './')");
	parser.setDescription(description);
	parser.setErrorHandler(AutoRef<ArgumentParseErrorHandler>(new ParserErrorHandler));
	Arguments args = parser.parse(argc, argv);
	string basePath = args.get("base-path");
	string targetPath = args.get("target-path");
	for (i = 0; i < args.unnamed().size(); ++i) {
		printf("%s\n", args.unnamed()[i]);
	}
	
    return 0;
}

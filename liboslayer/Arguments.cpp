#include "Arguments.hpp"
#include "os.hpp"
#include "Text.hpp"

namespace osl {

	using namespace std;
	
	ArgumentDeclaration::ArgumentDeclaration() : _must(false), _optional(false) {
	}
	ArgumentDeclaration::~ArgumentDeclaration() {
	}
	string & ArgumentDeclaration::longname() {
		return _longname;
	}
	string & ArgumentDeclaration::shortname() {
		return _shortname;
	}
	bool & ArgumentDeclaration::must() {
		return _must;
	}
	bool & ArgumentDeclaration::optional() {
		return _optional;
	}
	std::string & ArgumentDeclaration::description() {
		return _description;
	}

	ArgumentDeclaration ArgumentDeclaration::mustFull(const string & longname,
													  const string & shortname) {
		return mustFull(longname, shortname, "");
	}
	ArgumentDeclaration ArgumentDeclaration::mustFull(const string & longname,
													  const string & shortname,
													  const string & description) {
		ArgumentDeclaration decl;
		decl.must() = true;
		decl.longname() = longname;
		decl.shortname() = shortname;
		decl.description() = description;
		return decl;
	}
	ArgumentDeclaration ArgumentDeclaration::mustLong(const string & longname) {
		return mustFull(longname, "", "");
	}
	ArgumentDeclaration ArgumentDeclaration::mustLong(const string & longname,
													  const string & description) {
		return mustFull(longname, "", description);
	}
	ArgumentDeclaration ArgumentDeclaration::mustShort(const string & shortname) {
		return mustFull("", shortname, "");
	}
	ArgumentDeclaration ArgumentDeclaration::mustShort(const string & shortname,
													   const string & description) {
		return mustFull("", shortname, description);
	}

	ArgumentDeclaration ArgumentDeclaration::optionalFull(const string & longname,
														  const string & shortname) {
		return optionalFull(longname, shortname, "");
	}
	ArgumentDeclaration ArgumentDeclaration::optionalFull(const string & longname,
														  const string & shortname,
														  const string & description) {
		ArgumentDeclaration decl;
		decl.optional() = true;
		decl.longname() = longname;
		decl.shortname() = shortname;
		decl.description() = description;
		return decl;
	}
	ArgumentDeclaration ArgumentDeclaration::optionalLong(const string & longname) {
		return optionalFull(longname, "", "");
	}
	ArgumentDeclaration ArgumentDeclaration::optionalLong(const string & longname,
														  const string & description) {
		return optionalFull(longname, "", description);
	}
	ArgumentDeclaration ArgumentDeclaration::optionalShort(const string & shortname) {
		return optionalFull("", shortname, "");
	}
	ArgumentDeclaration ArgumentDeclaration::optionalShort(const string & shortname,
														   const string & description) {
		return optionalFull("", shortname, description);
	}


	Argument::Argument() {
	}
	Argument::Argument(const string & name)
		: _name(name) {
	}
	Argument::Argument(const string & name, const string & str)
		: _name(name), _str(str) {
	}
	Argument::~Argument() {
	}
	string & Argument::name() {
		return _name;
	}
	string & Argument::str() {
		return _str;
	}
	int Argument::to_int() {
		return Text::toInt(_str);
	}
	long Argument::to_long() {
		return Text::toLong(_str);
	}



	Arguments::Arguments() {
	}
	Arguments::~Arguments() {
	}
	string & Arguments::program() {
		return _program;
	}
	vector<Argument> & Arguments::vec_named() {
		return _vec_named;
	}
	Argument Arguments::named(const string & name) {
		for (vector<Argument>::iterator iter = _vec_named.begin(); iter != _vec_named.end(); iter++) {
			if (iter->name() == name) {
				return *iter;
			}
		}
		throw Exception("argument not found / name = " + name);
	}
	void Arguments::set(const string & name) {
		set(name, "");
	}
	void Arguments::set(const string & name, const string & str) {
		for (vector<Argument>::iterator iter = _vec_named.begin(); iter != _vec_named.end(); iter++) {
			if (iter->name() == name) {
				iter->str() = str;
				return;
			}
		}
		_vec_named.push_back(Argument(name, str));
	}
	bool Arguments::is_set(const string & name) {
		for (vector<Argument>::iterator iter = _vec_named.begin(); iter != _vec_named.end(); iter++) {
			if (iter->name() == name) {
				return true;
			}
		}
		return false;
	}
	vector<string> & Arguments::texts() {
		return _texts;
	}


	ArgumentParseError::ArgumentParseError() {
	}
	ArgumentParseError::~ArgumentParseError() {
	}


	ArgumentValidation::ErrorHandler::ErrorHandler() {
	}
	ArgumentValidation::ErrorHandler::~ErrorHandler() {
	}


	ArgumentValidation::ArgumentValidation() {
	}
	ArgumentValidation::~ArgumentValidation() {
	}


	ArgumentParser::ArgumentParser() {
	}
	ArgumentParser::~ArgumentParser() {
	}
	Arguments ArgumentParser::parse(int argc, char * argv[]) {
		return parse(argc, argv, ArgumentValidation());
	}
	Arguments ArgumentParser::parse(int argc, char * argv[], const ArgumentValidation & validation) {
		Arguments args;
		args.program() = argv[0];
		for (int i = 1; i < argc; i++) {
			string cand = argv[i];
			if (is_longname(cand) || is_shortname(cand)) {
				string phase = is_longname(cand) ? get_longname(cand) : get_shortname(cand);
				if (is_inline(phase)) {
					vector<string> tokens = Text::split(phase, "=");
					if (tokens.size() >= 2) {
						args.set(tokens[0], tokens[1]);
					} else {
						args.set(tokens[0]);
					}
				} else {
					if ((i + 1 >= argc) || is_longname(argv[i+1]) || is_shortname(argv[i+1])) {
						args.set(phase);
					} else {
						args.set(phase, argv[i+1]);
						i++;
					}
				}
			} else {
				args.texts().push_back(cand);
			}
		}
		return args;
	}

	bool ArgumentParser::is_inline(const string & str) {
		return (str.find("=") != string::npos);
	}
	bool ArgumentParser::is_longname(const string & str) {
		return Text::startsWith(str, "--");
	}
	bool ArgumentParser::is_shortname(const string & str) {
		return (is_longname(str) == false && Text::startsWith(str, "-"));
	}
	string ArgumentParser::get_longname(const string & str) {
		return str.substr(2);
	}
	string ArgumentParser::get_shortname(const string & str) {
		return str.substr(1);
	}
}

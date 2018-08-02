#include "Tokenizer.hpp"
#include "Text.hpp"

using namespace std;

namespace osl {

	Tokenizer::Tokenizer(const string & text, const string & delim)
		: _text(text), _delim(delim) {
		tokenize();
	}
	
	Tokenizer::~Tokenizer() {
	}

	string & Tokenizer::text() {
		return _text;
	}
	
	string Tokenizer::text() const {
		return _text;
	}
	
	string & Tokenizer::delim() {
		return _delim;
	}
	
	string Tokenizer::delim() const {
		return _delim;
	}

	void Tokenizer::tokenize() {
		tokens() = Text::split(text(), delim());
	}

	vector<string> & Tokenizer::tokens() {
		return _tokens;
	}

	vector<string> Tokenizer::tokens() const {
		return _tokens;
	}

	string & Tokenizer::token(size_t idx) {
		return _tokens[idx];
	}

	string Tokenizer::token(size_t idx) const {
		return _tokens[idx];
	}

	size_t Tokenizer::size() const {
		return _tokens.size();
	}
}

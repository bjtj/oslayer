#ifndef __XML_PARSER_HPP__
#define __XML_PARSER_HPP__

#include <vector>
#include <map>
#include <string>
#include "AutoRef.hpp"

namespace XML {

	/**
	 *
	 */
	class XmlNode {
	public:
		const static int NONE = 0;
		const static int ELEMENT = 1;
		const static int TEXT = 2;

	private:
		int type;
		XmlNode * parent;
		std::string _ns;
		std::string _tagName;
		std::map<std::string, std::string> _attrs;
		std::vector<XmlNode*> _children;
		std::string _text;
	
	public:
		XmlNode() : type(NONE), parent(NULL) {}
		virtual ~XmlNode() {
			for (std::vector<XmlNode*>::iterator iter = _children.begin(); iter != _children.end();) {
				delete *iter;
				iter = _children.erase(iter);
			}
		}
		void testType(int type) {
			if (this->type != type) {
				throw "type not matched / exptected: " + getTypeString(type) + ", but: " + getTypeString(this->type);
			}
		}
		bool nil() {
			return this->type == NONE;
		}
		std::string getTypeString(int type) {
			switch (type) {
			case ELEMENT:
				return "ELEMENT";
			case TEXT:
				return "TEXT";
			default:
				return "Unknown";
			}
		}
		void setType(int type) {
			this->type = type;
		}
		int getType() {
			return type;
		}
		bool valid() {
			return (isElement() || isText());
		}
		bool isElement() {
			return type == ELEMENT;
		}
		bool isText() {
			return type == TEXT;
		}
		void setParent(XmlNode * parent) {
			this->parent = parent;
		}
		XmlNode * getParent() {
			return parent;
		}
		size_t childrenCount() {
			testType(ELEMENT);
			return _children.size();
		}
		std::string & ns() {
			testType(ELEMENT);
			return _ns;
		}
		std::string & tagName() {
			testType(ELEMENT);
			return _tagName;
		}
		std::string & text() {
			testType(TEXT);
			return _text;
		}
		bool hasAttr(const std::string & name) {
			return _attrs.find(name) != _attrs.end();
		}
		std::string & attr(const std::string & name) {
			testType(ELEMENT);
			return _attrs[name];
		}
		std::map<std::string, std::string> & attrs() {
			return _attrs;
		}
		XmlNode * child(size_t idx) {
			testType(ELEMENT);
			return _children[idx];
		}
		void addChild(XmlNode * node) {
			testType(ELEMENT);
			node->setParent(this);
			_children.push_back(node);
		}
		void removeChild(size_t idx) {
			testType(ELEMENT);
			_children.erase(_children.begin() + idx);
		}
		XmlNode * getFirstChild() {
			return *_children.begin();
		}
		XmlNode * getLastChild() {
			return *_children.rbegin();
		}
		std::vector<XmlNode*> & children() {
			testType(ELEMENT);
			return _children;
		}
		XmlNode * getElementByTagNameInDepth(const std::string & tagName, int depth) {
			if (--depth <= 0) {
				return NULL;
			}
			for (std::vector<XmlNode*>::iterator iter = _children.begin(); iter != _children.end(); iter++) {
				if ((*iter)->isElement() && (*iter)->tagName() == tagName) {
					return *iter;
				}
				XmlNode * elem = (*iter)->getElementByTagNameInDepth(tagName, depth);
				if (elem) {
					return elem;
				}
			}
			return NULL;
		}
		XmlNode * getElementByTagName(const std::string & tagName) {
			for (std::vector<XmlNode*>::iterator iter = _children.begin(); iter != _children.end(); iter++) {
				if ((*iter)->isElement() && (*iter)->tagName() == tagName) {
					return *iter;
				}
				XmlNode * elem = (*iter)->getElementByTagName(tagName);
				if (elem) {
					return elem;
				}
			}
			return NULL;
		}
		std::vector<XmlNode*> getElementsByTagNameInDepth(const std::string & tagName, int depth) {
			std::vector<XmlNode*> lst;
			if (depth-- <= 0) {
				return lst;
			}
			for (std::vector<XmlNode*>::iterator iter = _children.begin(); iter != _children.end(); iter++) {
				if ((*iter)->isElement() && (*iter)->tagName() == tagName) {
					lst.push_back(*iter);
				}
				std::vector<XmlNode*> ret = (*iter)->getElementsByTagNameInDepth(tagName, depth);
				if (ret.size() > 0) {
					lst.insert(lst.end(), ret.begin(), ret.end());
				}
			}
			return lst;
		}
		std::vector<XmlNode*> getElementsByTagName(const std::string & tagName) {
			std::vector<XmlNode*> lst;
			for (std::vector<XmlNode*>::iterator iter = _children.begin(); iter != _children.end(); iter++) {
				if ((*iter)->isElement() && (*iter)->tagName() == tagName) {
					lst.push_back(*iter);
				}
				std::vector<XmlNode*> ret = (*iter)->getElementsByTagName(tagName);
				if (ret.size() > 0) {
					lst.insert(lst.end(), ret.begin(), ret.end());
				}
			}
			return lst;
		}
		std::string toString() {
			switch (type) {
			case ELEMENT:
				{
					std::string rest;
					for (std::map<std::string, std::string>::iterator iter = _attrs.begin(); iter != _attrs.end(); iter++) {
						rest.append(rest.empty() ? " @ " : ", ");
						rest.append(iter->first + " => \"" + iter->second + "\"");
					}
					if (!ns().empty()) {
						return ns() + ":" + tagName() + rest;
					} else {
						return tagName() + rest;
					}
				}
				break;
			case TEXT:
				return text();
			default:
				return "";
			}
		}
	};

	/**
	 *
	 */
	class XmlEncoder {
	private:
	public:
		XmlEncoder() {}
		virtual ~XmlEncoder() {}
		static std::string encode(const std::string & text) {
			std::string ret;
			for (std::string::const_iterator iter = text.begin(); iter != text.end(); iter++) {
				switch (*iter) {
				case '&':
					ret.append("&amp;");
					break;
				case '<':
					ret.append("&lt;");
					break;
				case '>':
					ret.append("&gt;");
					break;
				default:
					ret.append(1, *iter);
					break;
				}
			}
			return ret;
		}
	};

	/**
	 *
	 */
	class XmlDecoder {
	private:
	public:
		XmlDecoder() {}
		virtual ~XmlDecoder() {}
		static std::string decode(const std::string & text) {
			std::string ret;
			for (std::string::const_iterator iter = text.begin(); iter != text.end(); iter++) {
				switch (*iter) {
				case '&':
					{
						iter++;
						std::string enc;
						for (; *iter != ';'; iter++) {
							if (iter == text.end()) {
								throw "unexpected end of string";
							}
							enc.append(1, *iter);
						}

						if (enc == "amp") {
							ret.append(1, '&');
						} else if (enc == "lt") {
							ret.append(1, '<');
						} else if (enc == "gt") {
							ret.append(1, '>');
						} else {
							// unknown
						}
					}
					break;
				default:
					ret.append(1, *iter);
					break;
				}
			}
			return ret;
		}
	};

	/**
	 *
	 */
	class XmlDocument {
	private:
		std::string _firstLine;
		UTIL::AutoRef<XmlNode> _rootNode;
	public:
		XmlDocument() {}
		virtual ~XmlDocument() {}
		std::string & firstLine() {
			return _firstLine;
		}
		UTIL::AutoRef<XmlNode> getRootNode() {
			return _rootNode;
		}
		void setRootNode(UTIL::AutoRef<XmlNode> root) {
			_rootNode = root;
		}
		static std::string escapeString(const std::string & str) {
			std::string ret;
			for (std::string::const_iterator iter = str.begin(); iter != str.end(); iter++) {
				if (*iter == '\"') {
					ret.append("\\\"");
				} else {
					ret.append(1, *iter);
				}
			}
			return ret;
		}
		static std::string toFullTagString(XmlNode * node) {
			std::string xml = toTagNameString(node);
			if (node->attrs().size() > 0) {
				xml.append(" ");
			}
			for (std::map<std::string, std::string>::iterator iter = node->attrs().begin(); iter != node->attrs().end(); iter++) {
				xml.append(iter->first + "=\"" + escapeString(iter->second) + "\"");
			}
			return xml;
		}
		static std::string toTagNameString(XmlNode * node) {
			if (!node->ns().empty()) {
				return node->ns() + ":" + node->tagName();
			}
			return node->tagName();
		}
		static std::string toString(XmlNode * node) {
			std::string xml;
			if (node) {
				if (node->isElement()) {
					xml.append("<" + toFullTagString(node) + ">");
					for (size_t i = 0; i < node->childrenCount(); i++) {
						xml.append(toString(node->child(i)));
					}
					xml.append("</" + toTagNameString(node) + ">");
				} else {
					xml.append(node->text());
				}
			}
			return xml;
		}
		std::string toString() {
			std::string xml;
			if (!_firstLine.empty()) {
				xml.append("<!" + _firstLine + ">\r\n");
			}
			xml.append(toString(&_rootNode));
			return xml;
		}
	};

	/**
	 *
	 */
	class XmlNodeCursor {
	private:
		XmlNode * _root;
		XmlNode * _cursor;
	public:
		XmlNodeCursor(XmlNode * root) : _root(root), _cursor(root) {}
		virtual ~XmlNodeCursor() {}
		void enter(XmlNode * element) {
		
			element->testType(XmlNode::ELEMENT);

			if (_root == NULL) {
				_root = element;
			} else {
				_cursor->addChild(element);
			}
		
			_cursor = element;
		}
		void leave() {
			if (_cursor->getParent()) {
				_cursor = _cursor->getParent();
			}
		}
		void append(XmlNode * node) {
			_cursor->addChild(node);
		}
		XmlNode * cursor() {
			return _cursor;
		}
		XmlNode * root() {
			return _root;
		}
	};

	/**
	 *
	 */
	class DomParser {
	private:
	public:
		DomParser() {}
		virtual ~DomParser() {}

		static std::string trim(const std::string & text) {
			if (text.empty()) {
				return text;
			}
			size_t s = text.find_first_not_of(" \t\r\n");
			size_t e = text.find_last_not_of(" \t\r\n");
			return text.substr(s, e - s + 1);
		}

		static bool isSpace(char ch) {
			return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n';
		}

		static std::vector<std::string> tokenize(const std::string & str) {
			std::vector<std::string> tokens;
			for (std::string::const_iterator iter = str.begin(); iter != str.end(); iter++) {
				if (*iter == '\"') {
					std::string token;
					iter++;
					if (iter == str.end()) {
						throw "unexpected end of string";
					}
					for(; *iter != '\"'; iter++) {
						char ch = *iter;
						if (iter == str.end()) {
							throw "unexpected end of string";
						}
						if (*iter == '\\') {
							ch = *(++iter);
							switch(ch) {
							case 't':
								ch = '\t';
								break;
							case 'r':
								ch = '\r';
								break;
							case 'n':
								ch = '\n';
								break;
							default:
								break;
							}
						}
						token.append(1, ch);
					}
					tokens.push_back(token);
				} else if (*iter == '=') {
					tokens.push_back("=");
				} else if (!isSpace(*iter)) {
					std::string token;
					for(; iter != str.end() && !isSpace(*iter) && *iter != '='; iter++) {
						token.append(1, *iter);
					}
					tokens.push_back(token);
					iter--;
				}
			}
			return tokens;
		}

		static void parseAttrs(const std::string & attrs, XmlNode * node) {
			if (attrs.empty()) {
				return;
			}
			std::vector<std::string> tokens = tokenize(attrs);

			for (std::vector<std::string>::iterator iter = tokens.begin(); iter != tokens.end(); iter++) {
				std::string name = *iter;
				std::string value;
				if ((iter + 1) != tokens.end() && *(iter + 1) == "=") {
					value = *(iter + 2);
					iter += 2;
				}
				node->attr(name) = value;
			}
		}

		static void parseTag(const std::string & tag, XmlNode * node) {
			std::string ns;
			std::string name;
			std::string attrs;
			size_t sep = tag.find_first_of(" \t\r\n");
			if (sep != std::string::npos) {
				name = tag.substr(0, sep);
				attrs = trim(tag.substr(sep + 1));
			} else {
				name = tag;
			}
			sep = name.find(":");
			if (sep != std::string::npos) {
				ns = name.substr(0, sep);
				name = name.substr(sep + 1);
			}

			node->ns() = ns;
			node->tagName() = name;

			parseAttrs(attrs, node);
		}

		static XmlDocument parse(const std::string & text) {

			XmlDocument doc;
			XmlNodeCursor cursor(NULL);

//			bool first = true;

			size_t s = text.find("<");
			size_t l = 0;

			if (text[s + 1] == '!') {
				size_t e = text.find(">", s);
				doc.firstLine() = text.substr(s + 2, e - (s + 2));
				l = e + 1;
				s = text.find("<", e + 1);
			}
		
			while (s != std::string::npos) {
				size_t e = text.find(">", s);
				if (text[e - 1] == '/') {
					// atom
					XmlNode * node = new XmlNode;
					node->setType(XmlNode::ELEMENT);
					parseTag(trim(text.substr(s + 1, e - (s + 1) - 1)), node);
					cursor.append(node);
				} else if (text[s + 1] == '/') {
					{
						XmlNode * node = new XmlNode;
						node->setType(XmlNode::TEXT);
						node->text() = text.substr(l, s - l);
						cursor.append(node);
					}
					// end tag
					cursor.leave();
				} else {
					if (cursor.root()) {
						XmlNode * node = new XmlNode;
						node->setType(XmlNode::TEXT);
						node->text() = text.substr(l, s - l);
						cursor.append(node);
					}
					// start tag
					XmlNode * node = new XmlNode;
					node->setType(XmlNode::ELEMENT);
					parseTag(trim(text.substr(s + 1, e - (s + 1))), node);
					cursor.enter(node);
				}

				l = e + 1;
				s = text.find("<", e + 1);
			}

			doc.setRootNode(cursor.root());
			return doc;
		}
	};
}

#endif

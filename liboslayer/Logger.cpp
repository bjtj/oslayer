#include <iostream>
#include "Logger.hpp"
#include <liboslayer/Text.hpp>

namespace UTIL {

	using namespace std;
	using namespace OS;

	const int LogLevel::FATAL = 0;
	const int LogLevel::ERROR = 1;
	const int LogLevel::WARN = 2;
	const int LogLevel::INFO = 3;
	const int LogLevel::DEBUG = 4;
	const int LogLevel::TRACE = 5;
	const int LogLevel::VERBOSE = 6;

	LogLevel::LogLevel(int level) : level(level) {
	}
	LogLevel::~LogLevel() {
	}
	int LogLevel::getLevel() const {
		return level;
	}
	void LogLevel::setLevel(int level) {
	}
	string LogLevel::getShortName() {
		return getShortName(level);
	}
	string LogLevel::getName() {
		return getName(level);
	}
	string LogLevel::getShortName(int level) {
		switch (level) {
		case FATAL:
			return "F";
		case ERROR:
			return "E";
		case WARN:
			return "W";
		case INFO:
			return "I";
		case DEBUG:
			return "D";
		case TRACE:
			return "T";
		case VERBOSE:
			return "V";
		default:
			break;
		}
		throw Exception("unknown level");
	}
	string LogLevel::getName(int level) {
		switch (level) {
		case FATAL:
			return "FATAL";
		case ERROR:
			return "ERROR";
		case WARN:
			return "WARN";
		case INFO:
			return "INFO";
		case DEBUG:
			return "DEBUG";
		case TRACE:
			return "TRACE";
		case VERBOSE:
			return "VERBOSE";
		default:
			break;
		}
		throw Exception("unknown level");
	}
	void LogLevel::testLevel(int level) {
		switch (level) {
		case FATAL:
		case ERROR:
		case WARN:
		case INFO:
		case DEBUG:
		case TRACE:
		case VERBOSE:
			break;
		default:
			throw Exception("unknown level");
		}
	}
	vector<int> LogLevel::getLevels() {
		vector<int> levels;
		levels.push_back(FATAL);
		levels.push_back(ERROR);
		levels.push_back(WARN);
		levels.push_back(INFO);
		levels.push_back(DEBUG);
		levels.push_back(TRACE);
		levels.push_back(VERBOSE);
		return levels;
	}
	

	LogFormatter::LogFormatter() {
	}
	LogFormatter::~LogFormatter() {
	}
	string LogFormatter::format(const LogSession & session, const string & msg) {
		return "";
	}


	
	LogPrinter::LogPrinter() {
	}
	LogPrinter::~LogPrinter() {
	}
	void LogPrinter::print(const LogSession & session, const string & msg) {
	}
	


	LogSession::LogSession(const LogLevel & level) : enabled(false), level(level) {
	}
	LogSession::LogSession(int level) : enabled(false), level(level) {
	}
	LogSession::~LogSession() {
	}
	void LogSession::log(const string & msg) const {
		if (enabled) {
			printer->print(*this, formatter->format(*this, msg));
		}
	}
	void LogSession::setEnable(bool enable) {
		this->enabled = enable;
	}
	bool LogSession::isEnable() {
		return enabled;
	}
	AutoRef<LogFormatter> LogSession::getFormatter() {
		return formatter;
	}
	AutoRef<LogPrinter> LogSession::getPrinter() {
		return printer;
	}
	void LogSession::setFormatter(AutoRef<LogFormatter> formatter) {
		this->formatter = formatter;
	}
	void LogSession::setPrinter(AutoRef<LogPrinter> printer) {
		this->printer = printer;
	}
	LogLevel LogSession::getLevel() const {
		return level;
	}

	

	Logger::Logger(LoggerFactory * factory, const string & name) : factory(factory), name(name) {
	}
	Logger::Logger(const string & name) : factory(NULL), name(name) {
	}
	Logger::~Logger() {
		if (factory) {
			factory->removeObserver(this);
		}
	}
	void Logger::logf(const string & msg) const {
		_fatal->log(msg);
	}
	void Logger::loge(const string & msg) const {
		_error->log(msg);
	}
	void Logger::logw(const string & msg) const {
		_warn->log(msg);
	}
	void Logger::logi(const string & msg) const {
		_info->log(msg);
	}
	void Logger::logd(const string & msg) const {
		_debug->log(msg);
	}
	void Logger::logt(const string & msg) const {
		_trace->log(msg);
	}
	void Logger::logv(const string & msg) const {
		_verbose->log(msg);
	}
	AutoRef<LogSession> & Logger::session(int level) {
		switch (level) {
		case LogLevel::FATAL:
			return _fatal;
		case LogLevel::ERROR:
			return _error;
		case LogLevel::WARN:
			return _warn;
		case LogLevel::INFO:
			return _info;
		case LogLevel::DEBUG:
			return _debug;
		case LogLevel::TRACE:
			return _trace;
		case LogLevel::VERBOSE:
			return _verbose;
		default:
			break;
		}
		throw Exception("unknown log level");
	}
	void Logger::observe(LoggerFactory * factory) {
		this->factory = factory;
		factory->addObserver(this);
	}
	void Logger::update(Observable * target) {

		if (!target) {
			this->factory = NULL;
			return;
		}
		
		LoggerFactory * f = (LoggerFactory*)target;
		AutoRef<Logger> logger = f->getLoggerWithoutObserve(name);
		_fatal = logger->session(LogLevel::FATAL);
		_error = logger->session(LogLevel::ERROR);
		_warn = logger->session(LogLevel::WARN);
		_info = logger->session(LogLevel::INFO);
		_debug = logger->session(LogLevel::DEBUG);
		_trace = logger->session(LogLevel::TRACE);
		_verbose = logger->session(LogLevel::VERBOSE);
	}
	

	LoggerDescriptor::LoggerDescriptor() {
	}
	LoggerDescriptor::LoggerDescriptor(const string & pattern) : pattern(pattern) {
	}
	LoggerDescriptor::~LoggerDescriptor() {
	}
	bool LoggerDescriptor::match(const string & keyword) const {
		return Text::match(pattern, keyword);
	}
	AutoRef<Logger> LoggerDescriptor::makeLogger(LoggerFactory & factory, const string & name) {
		AutoRef<Logger> logger(new Logger(name));
		vector<int> levels = LogLevel::getLevels();
		for (vector<int>::iterator iter = levels.begin(); iter != levels.end(); iter++) {
			logger->session(*iter) = makeSession(factory, *iter);
		}
		return logger;
	}
	AutoRef<LogSession> LoggerDescriptor::makeSession(LoggerFactory & factory, int level) {
		AutoRef<LogSession> session(new LogSession(level));
		session->setEnable(true);
		if (factory.getFormatter(formatters[level]).nil()) {
			session->setFormatter(AutoRef<LogFormatter>(new LogFormatter));
		} else {
			session->setFormatter(factory.getFormatter(formatters[level]));

		}
		if (factory.getPrinter(printers[level]).nil()) {
			session->setPrinter(AutoRef<LogPrinter>(new LogPrinter));
		} else {
			session->setPrinter(factory.getPrinter(printers[level]));
		}
		return session;
	}
	void LoggerDescriptor::setFormatter(int level, const string & name) {
		LogLevel::testLevel(level);
		formatters[level] = name;
	}
	void LoggerDescriptor::setPrinter(int level, const string & name) {
		LogLevel::testLevel(level);
		printers[level] = name;
	}
	void LoggerDescriptor::setAllFormatter(const string & name) {
		vector<int> levels = LogLevel::getLevels();
		for (vector<int>::iterator iter = levels.begin(); iter != levels.end(); iter++) {
			setFormatter(*iter, name);
		}
	}
	void LoggerDescriptor::setAllPrinter(const string & name) {
		vector<int> levels = LogLevel::getLevels();
		for (vector<int>::iterator iter = levels.begin(); iter != levels.end(); iter++) {
			setPrinter(*iter, name);
		}
	}






	class PlainFormatter : public LogFormatter {
	public:
		PlainFormatter() {}
		virtual ~PlainFormatter() {}
		virtual string format(LogSession & session, const string & msg) {
			return msg;
		}
	};

	class BasicFormatter : public LogFormatter {
	public:
		BasicFormatter() {}
		virtual ~BasicFormatter() {}

		virtual string getDate() {
			return Date::format("", Date::now());
		}

		virtual string format(const LogSession & session, const string & msg) {
			return "[" + getDate() + "] " + session.getLevel().getShortName() + " " + msg;
		}
	};


	class ConsolePrinter : public LogPrinter {
	public:
		ConsolePrinter() {}
		virtual ~ConsolePrinter() {}
		virtual void print(const LogSession & session, const string & msg) {
			cout << msg << endl;
		}
	};


	
	
	
	LoggerFactory::LoggerFactory() {
		setFormatter("plain", AutoRef<LogFormatter>(new PlainFormatter));
		setFormatter("basic", AutoRef<LogFormatter>(new BasicFormatter));
		setPrinter("console", AutoRef<LogPrinter>(new ConsolePrinter));
		
	}
	LoggerFactory::~LoggerFactory() {
		notifyObservers(NULL);
	}
	LoggerFactory & LoggerFactory::getInstance() {
		static LoggerFactory factory;
		return factory;
	}
	AutoRef<Logger> LoggerFactory::getLogger(const string & name) {
		for (vector<LoggerDescriptor>::iterator iter = descriptors.begin(); iter != descriptors.end(); iter++) {
			if (iter->match(name)) {
				AutoRef<Logger> logger = iter->makeLogger(*this, name);
				logger->observe(this);
				return logger;
			}
		}
		LoggerDescriptor descriptor;
		AutoRef<Logger> logger = descriptor.makeLogger(*this, name);
		logger->observe(this);
		return logger;
	}
	AutoRef<Logger> LoggerFactory::getLoggerWithoutObserve(const string & name) {
		for (vector<LoggerDescriptor>::iterator iter = descriptors.begin(); iter != descriptors.end(); iter++) {
			if (iter->match(name)) {
				AutoRef<Logger> logger = iter->makeLogger(*this, name);
				return logger;
			}
		}
		LoggerDescriptor descriptor;
		AutoRef<Logger> logger = descriptor.makeLogger(*this, name);
		return logger;
	}
	void LoggerFactory::setLoggerDescriptorSimple(const string & pattern, const string & formatterName, const string & printerName) {
		LoggerDescriptor descriptor(pattern);
		descriptor.setAllFormatter(formatterName);
		descriptor.setAllPrinter(printerName);
		setLoggerDescriptor(descriptor);
	}
	void LoggerFactory::setLoggerDescriptor(const LoggerDescriptor & descriptor) {
		descriptors.push_back(descriptor);
		notifyObservers();
	}
	void LoggerFactory::setFormatter(const string & name, AutoRef<LogFormatter> formatter) {
		formatters[name] = formatter;
		notifyObservers();
	}
	void LoggerFactory::setPrinter(const string & name, AutoRef<LogPrinter> printer) {
		printers[name] = printer;
		notifyObservers();
	}
	AutoRef<LogFormatter> LoggerFactory::getFormatter(const string & name) {
		return formatters[name];
	}
	AutoRef<LogPrinter> LoggerFactory::getPrinter(const string & name) {
		return printers[name];
	}
}

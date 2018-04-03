#include <iostream>
#include "Date.hpp"
#include "Logger.hpp"
#include "Text.hpp"

namespace UTIL {

	using namespace std;
	using namespace OS;

	/**
	 * log level
	 */
	const int LogLevel::LEVEL_FATAL = 0;
	const int LogLevel::LEVEL_ERROR = 1;
	const int LogLevel::LEVEL_WARNING = 2;
	const int LogLevel::LEVEL_INFO = 3;
	const int LogLevel::LEVEL_DEBUG = 4;
	const int LogLevel::LEVEL_TRACE = 5;
	const int LogLevel::LEVEL_VERBOSE = 6;

	LogLevel::LogLevel(int level) : _level(level) {
	}
	LogLevel::~LogLevel() {
	}
	int LogLevel::level() const {
		return _level;
	}
	void LogLevel::level(int level) {
		_level = level;
	}
	string LogLevel::shortName() {
		return LogLevel::shortName(_level);
	}
	string LogLevel::name() {
		return LogLevel::name(_level);
	}
	string LogLevel::shortName(int level) {
		switch (level) {
		case LEVEL_FATAL:
			return "F";
		case LEVEL_ERROR:
			return "E";
		case LEVEL_WARNING:
			return "W";
		case LEVEL_INFO:
			return "I";
		case LEVEL_DEBUG:
			return "D";
		case LEVEL_TRACE:
			return "T";
		case LEVEL_VERBOSE:
			return "V";
		default:
			break;
		}
		throw Exception("unknown level");
	}
	string LogLevel::name(int level) {
		switch (level) {
		case LEVEL_FATAL:
			return "FATAL";
		case LEVEL_ERROR:
			return "ERROR";
		case LEVEL_WARNING:
			return "WARNING";
		case LEVEL_INFO:
			return "INFO";
		case LEVEL_DEBUG:
			return "DEBUG";
		case LEVEL_TRACE:
			return "TRACE";
		case LEVEL_VERBOSE:
			return "VERBOSE";
		default:
			break;
		}
		throw Exception("unknown level");
	}
	void LogLevel::validate(int level) {
		switch (level) {
		case LEVEL_FATAL:
		case LEVEL_ERROR:
		case LEVEL_WARNING:
		case LEVEL_INFO:
		case LEVEL_DEBUG:
		case LEVEL_TRACE:
		case LEVEL_VERBOSE:
			break;
		default:
			throw Exception("unknown level");
		}
	}
	vector<int> LogLevel::levels() {
		vector<int> levels;
		levels.push_back(LEVEL_FATAL);
		levels.push_back(LEVEL_ERROR);
		levels.push_back(LEVEL_WARNING);
		levels.push_back(LEVEL_INFO);
		levels.push_back(LEVEL_DEBUG);
		levels.push_back(LEVEL_TRACE);
		levels.push_back(LEVEL_VERBOSE);
		return levels;
	}
	
	/**
	 * log formatter
	 */
	LogFormatter::LogFormatter() {
	}
	LogFormatter::~LogFormatter() {
	}


	class DummyLogFormatter : public LogFormatter
	{
	public:
		DummyLogFormatter() {
		}
		virtual ~DummyLogFormatter() {
		}
		string format(const LogSession & session, const string & msg) {
			return "";
		}
	};

	/**
	 * log writer
	 */
	LogWriter::LogWriter() {
	}
	LogWriter::~LogWriter() {
	}


	/**
	 * dummy log writer
	 */
	class DummyLogWriter : public LogWriter
	{
	public:
		DummyLogWriter() {
		}
		virtual ~DummyLogWriter() {
		}
		virtual void write(const LogSession & session, const std::string & msg) {
		}
	};



	/**
	 * log session
	 */
	LogSession::LogSession(const LogLevel & level) : _enable(false), _level(level) {
	}
	LogSession::LogSession(int level) : _enable(false), _level(level) {
	}
	LogSession::~LogSession() {
	}
	void LogSession::log(const string & msg) const {
		if (_enable) {
			_writer->write(*this, _formatter->format(*this, msg));
		}
	}
	void LogSession::enable(bool enable) {
		this->_enable = enable;
	}
	bool LogSession::enable() const {
		return _enable;
	}
	AutoRef<LogFormatter> & LogSession::formatter() {
		return _formatter;
	}
	AutoRef<LogWriter> & LogSession::writer() {
		return _writer;
	}
	void LogSession::formatter(AutoRef<LogFormatter> formatter) {
		_formatter = formatter;
	}
	void LogSession::writer(AutoRef<LogWriter> writer) {
		_writer = writer;
	}
	void LogSession::level(const LogLevel & level) {
		_level = level;
	}
	LogLevel LogSession::level() const {
		return _level;
	}

	/**
	 * logger
	 */
	Logger::Logger(const string & name) : name(name) {
		_init();
	}
	Logger::~Logger() {
		stopObserve();
	}
	void Logger::_init() {
		_fatal = AutoRef<LogSession>(new LogSession(LogLevel::LEVEL_FATAL));
		_error = AutoRef<LogSession>(new LogSession(LogLevel::LEVEL_ERROR));
		_warning = AutoRef<LogSession>(new LogSession(LogLevel::LEVEL_WARNING));
		_info = AutoRef<LogSession>(new LogSession(LogLevel::LEVEL_INFO));
		_debug = AutoRef<LogSession>(new LogSession(LogLevel::LEVEL_DEBUG));
		_trace = AutoRef<LogSession>(new LogSession(LogLevel::LEVEL_TRACE));
		_verbose = AutoRef<LogSession>(new LogSession(LogLevel::LEVEL_VERBOSE));
	}
	void Logger::fatal(const string & msg) const {
		_fatal->log(msg);
	}
	void Logger::error(const string & msg) const {
		_error->log(msg);
	}
	void Logger::warning(const string & msg) const {
		_warning->log(msg);
	}
	void Logger::info(const string & msg) const {
		_info->log(msg);
	}
	void Logger::debug(const string & msg) const {
		_debug->log(msg);
	}
	void Logger::trace(const string & msg) const {
		_trace->log(msg);
	}
	void Logger::verbose(const string & msg) const {
		_verbose->log(msg);
	}
	AutoRef<LogSession> & Logger::session(int level) {
		switch (level) {
		case LogLevel::LEVEL_FATAL:
			return _fatal;
		case LogLevel::LEVEL_ERROR:
			return _error;
		case LogLevel::LEVEL_WARNING:
			return _warning;
		case LogLevel::LEVEL_INFO:
			return _info;
		case LogLevel::LEVEL_DEBUG:
			return _debug;
		case LogLevel::LEVEL_TRACE:
			return _trace;
		case LogLevel::LEVEL_VERBOSE:
			return _verbose;
		default:
			break;
		}
		throw Exception("unknown log level");
	}

	void Logger::onUpdate(Observable * target) {
		LoggerFactory * f = (LoggerFactory*)target;
		updateLogger(f->getLogger(name));
	}
	void Logger::updateLogger(AutoRef<Logger> logger) {
		_fatal = logger->session(LogLevel::LEVEL_FATAL);
		_error = logger->session(LogLevel::LEVEL_ERROR);
		_warning = logger->session(LogLevel::LEVEL_WARNING);
		_info = logger->session(LogLevel::LEVEL_INFO);
		_debug = logger->session(LogLevel::LEVEL_DEBUG);
		_trace = logger->session(LogLevel::LEVEL_TRACE);
		_verbose = logger->session(LogLevel::LEVEL_VERBOSE);
	}

	/**
	 * Logger Profile
	 */
	LoggerProfile::LoggerProfile() {
	}
	LoggerProfile::LoggerProfile(const string & pattern) : _pattern(pattern) {
	}
	LoggerProfile::~LoggerProfile() {
	}
	bool LoggerProfile::match(const string & keyword) const {
		return Text::match(_pattern, keyword);
	}
	AutoRef<Logger> LoggerProfile::makeLogger(LoggerFactory & factory, const string & name) {
		AutoRef<Logger> logger(new Logger(name));
		vector<int> levels = LogLevel::levels();
		for (vector<int>::iterator iter = levels.begin(); iter != levels.end(); iter++) {
			logger->session(*iter) = makeSession(factory, *iter);
		}
		return logger;
	}
	AutoRef<LogSession> LoggerProfile::makeSession(LoggerFactory & factory, int level) {
		AutoRef<LogSession> session(new LogSession(level));
		session->enable(true);
		if (factory.formatter(_formatters[level]).nil()) {
			session->formatter(AutoRef<LogFormatter>(new DummyLogFormatter));
		} else {
			session->formatter(factory.formatter(_formatters[level]));
		}
		if (factory.writer(_writers[level]).nil()) {
			session->writer(AutoRef<LogWriter>(new DummyLogWriter));
		} else {
			session->writer(factory.writer(_writers[level]));
		}
		return session;
	}
	void LoggerProfile::formatter(int level, const string & name) {
		LogLevel::validate(level);
		_formatters[level] = name;
	}
	void LoggerProfile::writer(int level, const string & name) {
		LogLevel::validate(level);
		_writers[level] = name;
	}
	void LoggerProfile::allFormatters(const string & name) {
		vector<int> levels = LogLevel::levels();
		for (vector<int>::iterator iter = levels.begin(); iter != levels.end(); iter++) {
			formatter(*iter, name);
		}
	}
	void LoggerProfile::allWriters(const string & name) {
		vector<int> levels = LogLevel::levels();
		for (vector<int>::iterator iter = levels.begin(); iter != levels.end(); iter++) {
			writer(*iter, name);
		}
	}

	/**
	 * plain formatter (built-in)
	 */
	class PlainFormatter : public LogFormatter {
	public:
		PlainFormatter() {}
		virtual ~PlainFormatter() {}
		virtual string format(const LogSession & session, const string & msg) {
			return msg;
		}
	};

	/**
	 * basic formatter (built-in)
	 */
	class BasicFormatter : public LogFormatter {
	public:
		BasicFormatter() {}
		virtual ~BasicFormatter() {}
		string getDate() {
			return Date::format(Date::now(), "%Y-%c-%d %H:%i:%s.%f");
		}
		virtual string format(const LogSession & session, const string & msg) {
			return "[" + getDate() + "] " + session.level().shortName() + " " + msg;
		}
	};

	/**
	 * console writer (built-in)
	 */
	class ConsoleWriter : public LogWriter {
	public:
		ConsoleWriter() {}
		virtual ~ConsoleWriter() {}
		virtual void write(const LogSession & session, const string & msg) {
			cout << msg << endl;
		}
	};

	/**
	 * logger factory
	 */
	LoggerFactory::LoggerFactory() {
		registerFormatter("plain", AutoRef<LogFormatter>(new PlainFormatter));
		registerFormatter("basic", AutoRef<LogFormatter>(new BasicFormatter));
		registerWriter("console", AutoRef<LogWriter>(new ConsoleWriter));
	}
	LoggerFactory::~LoggerFactory() {
	}
	LoggerFactory & LoggerFactory::inst() {
		static LoggerFactory factory;
		return factory;
	}
	AutoRef<Logger> LoggerFactory::getObservingLogger(const string & name) {
		AutoRef<Logger> logger = getLogger(name);
		logger->startObserve(this);
		return logger;
	}
	AutoRef<Logger> LoggerFactory::getLogger(const string & name) {
		for (vector<LoggerProfile>::iterator iter = _profiles.begin(); iter != _profiles.end(); iter++) {
			if (iter->match(name)) {
				return iter->makeLogger(*this, name);
			}
		}
		LoggerProfile profile;
		return profile.makeLogger(*this, name);
	}
	void LoggerFactory::setProfile(const string & pattern,
								   const string & formatterName,
								   const string & writerName) {
		LoggerProfile profile(pattern);
		profile.allFormatters(formatterName);
		profile.allWriters(writerName);
		setProfile(profile);
	}
	void LoggerFactory::setProfile(const LoggerProfile & profile) {
		_profiles.push_back(profile);
		notifyObservers();
	}
	void LoggerFactory::registerFormatter(const string & name, AutoRef<LogFormatter> formatter) {
		_formatters[name] = formatter;
		notifyObservers();
	}
	void LoggerFactory::registerWriter(const string & name, AutoRef<LogWriter> writer) {
		_writers[name] = writer;
		notifyObservers();
	}
	AutoRef<LogFormatter> LoggerFactory::formatter(const string & name) {
		return _formatters[name];
	}
	AutoRef<LogWriter> LoggerFactory::writer(const string & name) {
		return _writers[name];
	}
}

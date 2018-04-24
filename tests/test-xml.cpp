#include "utils.hpp"
#include <liboslayer/os.hpp>
#include <liboslayer/XmlParser.hpp>

using namespace std;
using namespace OS;
using namespace XML;

string xml = "<scpd xmlns=\"urn:schemas-upnp-org:service-1-0\"><specVersion><major>1</major><minor>0</minor></specVersion><actionList><action><name>AddPortMapping</name><argumentList><argument><name>NewRemoteHost</name><direction>in</direction><relatedStateVariable>RemoteHost</relatedStateVariable></argument><argument><name>NewExternalPort</name><direction>in</direction><relatedStateVariable>ExternalPort</relatedStateVariable></argument><argument><name>NewProtocol</name><direction>in</direction><relatedStateVariable>PortMappingProtocol</relatedStateVariable></argument><argument><name>NewInternalPort</name><direction>in</direction><relatedStateVariable>InternalPort</relatedStateVariable></argument><argument><name>NewInternalClient</name><direction>in</direction><relatedStateVariable>InternalClient</relatedStateVariable></argument><argument><name>NewEnabled</name><direction>in</direction><relatedStateVariable>PortMappingEnabled</relatedStateVariable></argument><argument><name>NewPortMappingDescription</name><direction>in</direction><relatedStateVariable>PortMappingDescription</relatedStateVariable></argument><argument><name>NewLeaseDuration</name><direction>in</direction><relatedStateVariable>PortMappingLeaseDuration</relatedStateVariable></argument></argumentList></action><action><name>GetExternalIPAddress</name><argumentList><argument><name>NewExternalIPAddress</name><direction>out</direction><relatedStateVariable>ExternalIPAddress</relatedStateVariable></argument></argumentList></action><action><name>DeletePortMapping</name><argumentList><argument><name>NewRemoteHost</name><direction>in</direction><relatedStateVariable>RemoteHost</relatedStateVariable></argument><argument><name>NewExternalPort</name><direction>in</direction><relatedStateVariable>ExternalPort</relatedStateVariable></argument><argument><name>NewProtocol</name><direction>in</direction><relatedStateVariable>PortMappingProtocol</relatedStateVariable></argument></argumentList></action><action><name>SetConnectionType</name><argumentList><argument><name>NewConnectionType</name><direction>in</direction><relatedStateVariable>ConnectionType</relatedStateVariable></argument></argumentList></action><action><name>GetConnectionTypeInfo</name><argumentList><argument><name>NewConnectionType</name><direction>out</direction><relatedStateVariable>ConnectionType</relatedStateVariable></argument><argument><name>NewPossibleConnectionTypes</name><direction>out</direction><relatedStateVariable>PossibleConnectionTypes</relatedStateVariable></argument></argumentList></action><action><name>RequestConnection</name></action><action><name>ForceTermination</name></action><action><name>GetStatusInfo</name><argumentList><argument><name>NewConnectionStatus</name><direction>out</direction><relatedStateVariable>ConnectionStatus</relatedStateVariable></argument><argument><name>NewLastConnectionError</name><direction>out</direction><relatedStateVariable>LastConnectionError</relatedStateVariable></argument><argument><name>NewUptime</name><direction>out</direction><relatedStateVariable>Uptime</relatedStateVariable></argument></argumentList></action><action><name>GetNATRSIPStatus</name><argumentList><argument><name>NewRSIPAvailable</name><direction>out</direction><relatedStateVariable>RSIPAvailable</relatedStateVariable></argument><argument><name>NewNATEnabled</name><direction>out</direction><relatedStateVariable>NATEnabled</relatedStateVariable></argument></argumentList></action><action><name>GetGenericPortMappingEntry</name><argumentList><argument><name>NewPortMappingIndex</name><direction>in</direction><relatedStateVariable>PortMappingNumberOfEntries</relatedStateVariable></argument><argument><name>NewRemoteHost</name><direction>out</direction><relatedStateVariable>RemoteHost</relatedStateVariable></argument><argument><name>NewExternalPort</name><direction>out</direction><relatedStateVariable>ExternalPort</relatedStateVariable></argument><argument><name>NewProtocol</name><direction>out</direction><relatedStateVariable>PortMappingProtocol</relatedStateVariable></argument><argument><nam";

static void test_xml_parse() {
	string err;
	try {
		XmlDocument doc = DomParser::parse(xml);
	} catch (OS::Exception & e) {
		err = e.toString();
	}
	ASSERT(err, ==, "Wrong xml format");
}

static void test_xml_parse2() {
	string xml = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><s:Body><u:GetLoadLevelTarget xmlns:u=\"urn:schemas-upnp-org:service:Dimming:1\"></u:GetLoadLevelTarget></s:Body></s:Envelope>";

	unsigned long tick = tick_milli();
	XmlDocument doc = DomParser::parse(xml);
	cout << " ** parsing time: " << tick_milli() - tick << " ms." << endl;
	
	ASSERT(doc.rootNode()->getElementByTagName("Body").nil(), ==, false);
}

static void test_xml_decode() {
	string xml = "<text>a&amp;b</text>";
	unsigned long tick = tick_milli();
	XmlDocument doc = DomParser::parse(xml);
	cout << " ** parsing time: " << tick_milli() - tick << " ms." << endl;
	ASSERT(doc.rootNode()->getFirstChild()->text(), ==, "a&b");
}

int main(int argc, char *args[]) {
    test_xml_parse();
	test_xml_parse2();
	test_xml_decode();
    return 0;
}

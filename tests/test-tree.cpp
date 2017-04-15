#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Tree.hpp>
#include <string>

using namespace std;
using namespace OS;
using namespace UTIL;

/**
 * 
 */
class Node : public TreeNode {
private:
	string _name;
public:
    Node(const string & name) : _name(name) {}
    virtual ~Node() {}
	string & name() {return _name;}
	static bool equals(TreeNode * l, TreeNode * r) {
		return ((Node*)l)->name() == ((Node*)r)->name();
	}
};

class NodeRef : public AutoRef<TreeNode> {
private:
public:
    NodeRef(const string & name) : AutoRef<TreeNode>(new Node(name)) {}
    virtual ~NodeRef() {}
};


/**
 * 
 */
class TreeTestCase : public TestCase
{
public:
    TreeTestCase() : TestCase("tree-test") {
	}
    virtual ~TreeTestCase() {
	}
	virtual void test() {
		Tree tree;
		tree.rootNode() = AutoRef<TreeNode>(new Node("root"));
		ASSERT(tree.rootNode().nil(), ==, false);
		tree.rootNode()->addChild(AutoRef<TreeNode>(new Node("A")));
		ASSERT(tree.rootNode()->childCount(), ==, 1);
		tree.rootNode()->addChild(AutoRef<TreeNode>(new Node("B")));
		ASSERT(tree.rootNode()->childCount(), ==, 2);
		tree.rootNode()->addChild(AutoRef<TreeNode>(new Node("C")));
		ASSERT(tree.rootNode()->childCount(), ==, 3);

		ASSERT(tree.rootNode()->depth(), ==, 2);

		Node target("B");
		AutoRef<TreeNode> find = tree.rootNode()->find(Node::equals, &target);
		ASSERT(find.nil(), ==, false);
		ASSERT(((Node*)&find)->name(), ==, "B");

		target.name() = "C";
		find = tree.rootNode()->find(Node::equals, &target);
		ASSERT(find.nil(), ==, false);
		ASSERT(((Node*)&find)->name(), ==, "C");
	}
};

class TreeNodeCursorTestCase : public TestCase {
public:
    TreeNodeCursorTestCase() : TestCase("tree node cursor test case") {}
    virtual ~TreeNodeCursorTestCase() {}
	virtual void test() {
		TreeNodeCursor cursor(NodeRef("R"));
		cursor.append(NodeRef("A"));
		cursor.append(NodeRef("B"));
		cursor.enter(NodeRef("C"));
		cursor.append(NodeRef("D"));
		cursor.leave();
		cursor.append(NodeRef("E"));
		ASSERT(toString(cursor.root()), ==, "R->{A B C->{D} E}");
	}
	string toString(AutoRef<TreeNode> node) {
		string ret;
		ret.append(((Node*)&node)->name());
		if (node->childCount() > 0) {
			ret.append("->{");
			for (size_t i = 0; i < node->childCount(); i++) {
				if (i > 0) {
					ret.append(" ");
				}
				ret.append(toString(node->childAt(i)));
			}
			ret.append("}");
		}
		return ret;
	}
};


/**
 * main
 */
int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new TreeTestCase));
	ts.addTestCase(AutoRef<TestCase>(new TreeNodeCursorTestCase));

	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}

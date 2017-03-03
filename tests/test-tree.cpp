#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Tree.hpp>
#include <string>

using namespace std;
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

/**
 * main
 */
int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new TreeTestCase));

	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}

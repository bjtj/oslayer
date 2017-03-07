#include "Tree.hpp"

namespace UTIL {

	using namespace std;

	/**
	 * 
	 */
	TreeNode::TreeNode() : _parent(NULL) {
	}
	TreeNode::~TreeNode() {
	}
	TreeNode * TreeNode::parent() {
        return _parent;
	}
	vector< AutoRef<TreeNode> > & TreeNode::children() {
		return _children;
	}
	void TreeNode::addChild(AutoRef<TreeNode> child) {
		_children.push_back(child);
	}
	void TreeNode::removeChild(AutoRef<TreeNode> child) {
		for (vector< AutoRef<TreeNode> >::iterator iter = _children.begin(); iter != _children.end(); iter++) {
			if ((*iter) == child) {
				_children.erase(iter);
				return;
			}
		}
	}
	AutoRef<TreeNode> & TreeNode::childAt(size_t idx) {
		return _children[idx];
	}
	size_t TreeNode::childCount() {
		return _children.size();
	}
	AutoRef<TreeNode> TreeNode::find(fn_treenode_equals fn, TreeNode * left) {
		for (size_t i = 0; i < _children.size(); i++) {
			if (fn(left, &(_children[i])) == true) {
				return _children[i];
			}
			AutoRef<TreeNode> node = _children[i]->find(fn, left);
			if (node.nil() == false) {
				return node;
			}
		}
		return AutoRef<TreeNode>();
	}
	vector< AutoRef<TreeNode> > TreeNode::findAll(fn_treenode_equals fn, TreeNode * left) {
		vector< AutoRef<TreeNode> > nodes;
		for (size_t i = 0; i < _children.size(); i++) {
			if (fn(left, &(_children[i])) == true) {
				nodes.push_back(_children[i]);
			}
		}
		for (size_t i = 0; i < _children.size(); i++) {
			vector< AutoRef<TreeNode> > lst = _children[i]->findAll(fn, left);
			if (lst.size() > 0) {
				nodes.insert(nodes.end(), lst.begin(), lst.end());
			}
		}
		return nodes;
	}

	/**
	 * 
	 */
	Tree::Tree() {
	}
    Tree::~Tree() {
	}
	AutoRef<TreeNode> & Tree::rootNode() {
		return _rootNode;
	}
}

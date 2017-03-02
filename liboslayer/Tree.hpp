#ifndef __TREE_HPP__
#define __TREE_HPP__

#include "AutoRef.hpp"

namespace UTIL {

	/**
	 * 
	 */
	class TreeNode;

	/**
	 *
	 */
	typedef int (*fn_compare_treenode)(TreeNode * a, TreeNode * b);

	/**
	 * 
	 */
	class TreeNode {
	private:
		TreeNode * _parent;
		std::vector< AutoRef<TreeNode> > _children;
	public:
		TreeNode();
		virtual ~TreeNode();
		TreeNode * parent();
		std::vector< AutoRef<TreeNode> > & children();
		AutoRef<TreeNode> & childAt (size_t idx);
		size_t childCount();
		AutoRef<TreeNode> & find(fn_compare_treenode * fn);
	};

	/**
	 * 
	 */
	class Tree {
	private:
		AutoRef<TreeNode> _rootNode;
	public:
		Tree();
		virtual ~Tree();
		AutoRef<TreeNode> & rootNode();
	};
}

#endif

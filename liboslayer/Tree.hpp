#ifndef __TREE_HPP__
#define __TREE_HPP__

#include <vector>
#include "AutoRef.hpp"
#include "Ref.hpp"

namespace UTIL {

	/**
	 * 
	 */
	class TreeNode;

	/**
	 *
	 */
	typedef bool (*fn_treenode_equals)(TreeNode * a, TreeNode * b);

	/**
	 * 
	 */
	class TreeNode {
	private:
		Ref<TreeNode> _parent;
		std::vector< AutoRef<TreeNode> > _children;
	public:
		TreeNode();
		virtual ~TreeNode();
		Ref<TreeNode> & parent();
		Ref<TreeNode> root();
		std::vector< AutoRef<TreeNode> > & children();
		void addChild(AutoRef<TreeNode> child);
		void removeChild(AutoRef<TreeNode> child);
		AutoRef<TreeNode> & childAt(size_t idx);
		size_t childCount();
		AutoRef<TreeNode> find(fn_treenode_equals fn, TreeNode * left);
		std::vector< AutoRef<TreeNode> > findAll(fn_treenode_equals fn, TreeNode * left);
		size_t depth();
	};

	/**
	 * 
	 */
	class TreeNodeCursor {
	private:
		AutoRef<TreeNode> _root;
		Ref<TreeNode> _cursor;
	public:
		TreeNodeCursor(AutoRef<TreeNode> root);
		virtual ~TreeNodeCursor();
		AutoRef<TreeNode> & root();
		Ref<TreeNode> & cursor();
	    Ref<TreeNodeCursor> enter(AutoRef<TreeNode> node);
		Ref<TreeNodeCursor> leave();
		Ref<TreeNodeCursor> append(AutoRef<TreeNode> node);
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

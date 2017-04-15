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
		OS::Ref<TreeNode> _parent;
		std::vector< OS::AutoRef<TreeNode> > _children;
	public:
		TreeNode();
		virtual ~TreeNode();
		OS::Ref<TreeNode> & parent();
		OS::Ref<TreeNode> root();
		std::vector< OS::AutoRef<TreeNode> > & children();
		void addChild(OS::AutoRef<TreeNode> child);
		void removeChild(OS::AutoRef<TreeNode> child);
		OS::AutoRef<TreeNode> & childAt(size_t idx);
		size_t childCount();
		OS::AutoRef<TreeNode> find(fn_treenode_equals fn, TreeNode * left);
		std::vector< OS::AutoRef<TreeNode> > findAll(fn_treenode_equals fn, TreeNode * left);
		size_t depth();
	};

	/**
	 * 
	 */
	class TreeNodeCursor {
	private:
		OS::AutoRef<TreeNode> _root;
		OS::Ref<TreeNode> _cursor;
	public:
		TreeNodeCursor(OS::AutoRef<TreeNode> root);
		virtual ~TreeNodeCursor();
		OS::AutoRef<TreeNode> & root();
		OS::Ref<TreeNode> & cursor();
	    OS::Ref<TreeNodeCursor> enter(OS::AutoRef<TreeNode> node);
		OS::Ref<TreeNodeCursor> leave();
		OS::Ref<TreeNodeCursor> append(OS::AutoRef<TreeNode> node);
	};

	/**
	 * 
	 */
	class Tree {
	private:
		OS::AutoRef<TreeNode> _rootNode;
	public:
		Tree();
		virtual ~Tree();
		OS::AutoRef<TreeNode> & rootNode();
	};
}

#endif

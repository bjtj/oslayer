#ifndef __TREE_HPP__
#define __TREE_HPP__

#include <vector>
#include "AutoRef.hpp"
#include "Ref.hpp"

namespace osl {

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
		osl::Ref<TreeNode> _parent;
		std::vector< osl::AutoRef<TreeNode> > _children;
	public:
		TreeNode();
		virtual ~TreeNode();
		osl::Ref<TreeNode> & parent();
		osl::Ref<TreeNode> root();
		std::vector< osl::AutoRef<TreeNode> > & children();
		void addChild(osl::AutoRef<TreeNode> child);
		void removeChild(osl::AutoRef<TreeNode> child);
		osl::AutoRef<TreeNode> & childAt(size_t idx);
		size_t childCount();
		osl::AutoRef<TreeNode> find(fn_treenode_equals fn, TreeNode * left);
		std::vector< osl::AutoRef<TreeNode> > findAll(fn_treenode_equals fn, TreeNode * left);
		size_t depth();
	};

	/**
	 * 
	 */
	class TreeNodeCursor {
	private:
		osl::AutoRef<TreeNode> _root;
		osl::Ref<TreeNode> _cursor;
	public:
		TreeNodeCursor(osl::AutoRef<TreeNode> root);
		virtual ~TreeNodeCursor();
		osl::AutoRef<TreeNode> & root();
		osl::Ref<TreeNode> & cursor();
	    osl::Ref<TreeNodeCursor> enter(osl::AutoRef<TreeNode> node);
		osl::Ref<TreeNodeCursor> leave();
		osl::Ref<TreeNodeCursor> append(osl::AutoRef<TreeNode> node);
	};

	/**
	 * 
	 */
	class Tree {
	private:
		osl::AutoRef<TreeNode> _rootNode;
	public:
		Tree();
		virtual ~Tree();
		osl::AutoRef<TreeNode> & rootNode();
	};
}

#endif

#include "../include/tree.h"

#include <iostream>

//===============================================================
//              Tree Node
//===============================================================


TNode::TNode() {
  left = right = parent = nullptr;
  data = Contact();
}

TNode::TNode(Contact c) {
  this->data = c;
  left = right = parent = nullptr;
}

TNode::TNode(Contact c, TNode *parent) {
  this->data = c;
  this->parent = parent;
  left = right = nullptr;
}


TNode::TNode(TNode *left, TNode *right, TNode *parent, Contact data) {
  this->left = left;
  this->right = right;
  this->parent = parent;
  this->data = data;
}

void TNode::print() {
  data.print();
}

void TNode::printTree() {
  if (left) left->printTree();
  data.print();
  if (right) right->printTree();
}

TNode *TNode::min() {
  if (left) return left->min();
  return this;
}

TNode *TNode::max() {
  if (right) return right->max();
  return this;
}

void TNode::add(Contact c, int sortBy) {
  if (c.compare(data, sortBy) < 0) {
    if (left) {
      left->add(c, sortBy);
    } else {
      left = new TNode(c, this);
    }
  } else {
    if (right) {
      right->add(c, sortBy);
    } else {
      right = new TNode(c, this);
    }
  }
}

TNode *TNode::find(Contact x, int sortBy) {
  if (data.eq(x)) return this;
  if (data.compare(x, sortBy) > 0) {
    if (!left) {
      cout << "No such element: ";
      x.print();
      return nullptr;
    }
    return left->find(x, sortBy);
  } else {
    if (!right) {
      cout << "No such element: ";
      x.print();
      return nullptr;
    }
    return right->find(x, sortBy);
  }
}

void TNode::toArray(TNode *arr, int &i) {
  if (left) left->toArray(arr, i);
  arr[i] = *this;
  ++i;
  if (right) right->toArray(arr, i);
}

void TNode::printTreeReverse() {
  if (right) right->printTreeReverse();
  data.print();
  if (left) left->printTreeReverse();
}


//===============================================================
//              Contact Book Tree
//===============================================================

CBTree *CBTree::sort(int sortBy) {
  if (this->sortBy == sortBy) return this;
  this->sortBy = sortBy;
  TNode *t = toArray();
  int l = size;
  clear();
  for (int i = 0; i < l; ++i) {
    add(t[i].data);
  }
  delete[] t;
  return this;
}

void CBTree::clear() {
  if (!root) return;
  innerClear(root);
  size = 0;
  root = nullptr;
}

TNode *CBTree::toArray() {
  TNode *arr = new TNode[size];
  if (root) {
    int i = 0;
    root->toArray(arr, i);
  }
  return arr;
}

CBTree *CBTree::remove(Contact x) {
  TNode *found = find(x);
  if (!found) return nullptr;
  delete remove(found);
  size--;
  return this;
}

void CBTree::printReverse() {
  if (!root) cout << "Empty tree" << endl;
  else root->printTreeReverse();
}

CBTree *CBTree::add(Contact c) {
  if (!root) {
    root = new TNode(c);
    size = 1;
  } else {
    root->add(c, sortBy);
    size++;
  }
  return this;
}

void CBTree::print() {
  if (!root) cout << "Empty tree" << endl;
  else root->printTree();
}

CBTree::~CBTree() {
  clear();
}

TNode *CBTree::find(Contact x) {
  if (!root) {
    cout << "No such element: ";
    x.print();
    return nullptr;
  }
  return root->find(x, sortBy);
}

TNode *CBTree::pred(TNode *x) {
  if (x->left) return x->left->max();

  TNode *y;
  do {
    y = x;
    x = x->parent;
  } while (x && (x->right != y));
  return x;
}

void CBTree::innerClear(TNode *node) {
  if (node) {
    innerClear(node->left);
    innerClear(node->right);
    delete node;
  }
}

TNode *CBTree::pred(Contact x) {
  TNode *found = find(x);
  if (!found) return nullptr;
  return pred(found);
}

TNode *CBTree::remove(TNode *x) {
  TNode *y = x->parent, *z;
  if ((x->left) && (x->right)) {
    z = (rand() % 2) ? remove(pred(x)) : remove(succ(x));
    z->left = x->left;
    if (z->left) z->left->parent = z;
    z->right = x->right;
    if (z->right) z->right->parent = z;
  } else z = (x->left) ? x->left : x->right;

  if (z) z->parent = y;

  if (!y) root = z;
  else {
    if (y->left == x) y->left = z;
    else y->right = z;
  }

  return x;
}

TNode *CBTree::succ(Contact x) {
  TNode *found = find(x);
  if (!found) return nullptr;
  return succ(found);
}

TNode *CBTree::succ(TNode *x) {
  if (x->right) return x->right->min();

  TNode *y;
  do {
    y = x;
    x = x->parent;
  } while (x && (x->left != y));
  return x;
}

CBTree *createContactsTree(int n) {
    CBTree *tree = new CBTree;
    for (int i=0; i<n; i++){
        tree->add(randomContact());
    }
    return tree;
}

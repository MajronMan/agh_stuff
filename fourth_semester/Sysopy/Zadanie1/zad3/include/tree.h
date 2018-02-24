#ifndef SYSOPY_TREE_H
#define SYSOPY_TREE_H

#include "contact.h"


class TNode {
public:
    TNode *left;
    TNode *right;
    TNode *parent;
    Contact data;

    TNode();

    TNode(Contact c);

    TNode(Contact c, TNode *parent);

    TNode(TNode *left, TNode *right, TNode *parent, Contact data);

    TNode *find(Contact x, int sortBy);

    void add(Contact c, int sortBy);

    TNode *max();

    TNode *min();

    void toArray(TNode *arr, int &i);

    void printTree();

    void print();

    void printTreeReverse();
};

class CBTree {
    TNode *pred(TNode *x);

    TNode *pred(Contact x);

    TNode *succ(TNode *x);

    TNode *succ(Contact x);

    TNode *remove(TNode *x);

    void innerClear(TNode *node);

public:
    int size = 0;
    int sortBy = 0;
    TNode *root = nullptr;

    CBTree() {};

    ~CBTree();

    TNode *find(Contact x);

    CBTree *add(Contact c);

    void print();

    void printReverse();

    CBTree *remove(Contact x);

    TNode *toArray();

    void clear();

    CBTree *sort(int sortBy);
};


CBTree* createContactsTree(int n);

#endif //SYSOPY_TREE_H

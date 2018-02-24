#ifndef SYSOPY_LIST_H
#define SYSOPY_LIST_H

#include "contact.h"

class Node {
public:
    Node *next;
    Node *previous;
    Contact data;

    Node();

    Node(Node *next, Node *previous, Contact data);

    void remove();
};

class CBList {
    Node *head;
    Node *tail;
    int sortBy = 0;
public:
    int size = 0;

    CBList();

    ~CBList();

    Contact *toArray();

    void clear();

    CBList *add(Contact c);

    CBList *remove(Contact c);

    Node *find(Contact c);

    CBList *sort(int by);

    void print();
};


CBList* createContactsList(int n);

#endif //SYSOPY_LIST_H

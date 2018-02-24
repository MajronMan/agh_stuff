#include "../include/list.h"
#include <iostream>
//===============================================================
//              Node
//===============================================================


Node::Node() {
  next = previous = nullptr;
  data = Contact();
}

Node::Node(Node *next, Node *previous, Contact data) {
  this->next = next;
  this->previous = previous;
  this->data = data;
}

void Node::remove() {
  previous->next = next;
  next->previous = previous;
  delete this;
}

//===============================================================
//              Contact Book List
//===============================================================

CBList::CBList() {
  head = new Node();
  tail = new Node();
  head->next = tail;
  tail->previous = head;
}

CBList::~CBList() {
  Node *current = head;
  while (current != nullptr) {
    Node *next = current->next;
    delete current;
    current = next;
  }
}

void CBList::print() {
  for (Node *cur = head->next; cur != tail; cur = cur->next)
    cur->data.print();
}

CBList *CBList::sort(int by) {
  if (by == this->sortBy) return this;
  this->sortBy = by;
  int s = this->size;
  Contact *arr = this->toArray();
  this->clear();
  for (int i = 0; i < s; i++)
    this->add(arr[i]);
  delete[] arr;
  this->size = s;
  return this;
}

CBList *CBList::remove(Contact c) {
  Node *f = find(c);
  if (f != nullptr) {
    f->remove();
    size--;
  }
  return this;
}

Contact *CBList::toArray() {
  Contact *arr = new Contact[size];
  Node *cur = head->next;
  int i = 0;
  while (cur != tail) {
    arr[i] = cur->data;
    cur = cur->next;
    ++i;
  }
  return arr;
}

Node *CBList::find(Contact c) {
  for (Node *cur = head->next; cur != tail; cur = cur->next) {
    if (cur->data.eq(c)) {
      return cur;
    }
  }
  cout << "No such element: ";
  c.print();
  return nullptr;
}

CBList *CBList::add(Contact c) {
  Node *cur = head;
  while (cur->next != tail && cur->next->data.compare(c, sortBy) < 0)
    cur = cur->next;
  Node *nn = new Node(cur->next, cur, c);
  cur->next = nn;
  nn->next->previous = nn;
  size++;
  return this;
}

void CBList::clear() {
  Node *next;
  for (Node *cur = head->next; cur != tail; cur = next) {
    next = cur->next;
    cur->remove();
  }
}

CBList *createContactsList(int n) {
    CBList *list = new CBList;
    for (int i=0; i<n; i++){
        list->add(randomContact());
    }
    return list;
}

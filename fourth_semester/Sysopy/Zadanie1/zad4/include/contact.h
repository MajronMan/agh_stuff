#ifndef SYSOPY_CONTACT_H
#define SYSOPY_CONTACT_H

#include <string>

using namespace std;

struct Date {
  int day;
  int month;
  int year;

  string toString();

  Date(int y, int m, int d);

  Date() {
  }

  int compare(Date d);
};
Date randomAge();

class Contact {
public:
  string name;
  string surname;
  Date birthDate;
  string email;
  int phone;
  string address;

  Contact() {
  }

  Contact(
    string name, string surname, Date birthDate,
    string email, int phone, string address);

  void print();

  int compare(Contact c, int type);

  bool eq(Contact c);
};
Contact randomContact();

#endif //SYSOPY_CONTACT_H

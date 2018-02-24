#include "../include/contact.h"

#include <iostream>

using namespace std;

//===============================================================
//              DATE
//===============================================================
string Date::toString() {
  return to_string(year) + "-" + to_string(month) + "-" + to_string(day);
}

Date::Date(int y, int m, int d) {
  day = d;
  month = m;
  year = y;
}

int Date::compare(Date d) {
  int dy = year - d.year;
  if (dy != 0) return dy;
  int dm = month - d.month;
  if (dm != 0) return dm;
  return day - d.day;
}

Date randomAge(){
  return Date(1950+rand()%40, 1+rand()%12, 1+rand()%28);
}


//===============================================================
//              Contact
//===============================================================

Contact::Contact(
  string name, string surname, Date birthDate,
  string email, int phone, string address) {
  this->name = name;
  this->surname = surname;
  this->birthDate = birthDate;
  this->email = email;
  this->phone = phone;
  this->address = address;
}

void Contact::print() {
  cout << name << " " << surname << " born " << birthDate.toString() << " Email: " << email << " Phone: "
       << phone << " Address: " << address << endl;
}

int Contact::compare(Contact c, int type = 0) {
  switch (type) {
  case 1:
    return birthDate.compare(c.birthDate);
  case 2:
    return email.compare(c.email);
  case 3:
    return to_string(phone).compare(to_string(c.phone));
  default:
    int k = surname.compare(c.surname);
    if (k == 0) return name.compare(c.name);
    return k;
  }


}

bool Contact::eq(Contact c) {
  return (name == c.name) &&
         (surname == c.surname) &&
         (birthDate.compare(c.birthDate) == 0) &&
         (email == c.email) &&
         (phone == c.phone) &&
         (address == c.address);
}

Contact randomContact(){
  int ncount = 21;
  string names[] = {"Johann", "Sebastian", "Amadeus", "Frederick", "Claude", "Igor", "Antonin", "Nikolai",
    "Etienne", "Ludwig", "Franz", "Luigi", "Louis", "Jan", "Cipriani", "Ignaz", "Carl", "Joachim", "August",
    "Cesar", "Charles"
  };
  string surnames[] = {"Bach", "Mozart", "Prokofiew", "Debussy", "Chopin", "Bukowski", "Telemann", "Dvorak", "Penderecki",
      "Liszt", "Grimm", "Kafka", "Herbeck", "Norman", "Smetana", "Bolzoni", "Malling", "Krug", "Lazzari",
      "Mahler", "Shelley", "Rott"
  };
  string streets[] = {"Torstrasse", "Liberty Street", "Canterbury Road", "White Street", "Wall street", "Lakeview Drive",
  "Cross Street", "Monroe Street",
"Green Street",
"Riverside Drive",
"Canal Street",
"South Street",
"Bank Street",
"6th Avenue",
"Magnolia Drive",
"Route 29",
"Heather Court",
"Canterbury Road",
"Cypress Court",
"Sherman Street",
"Lipowa"
  };


  string rname = names[rand()%ncount];
  string rsurname = surnames[rand()%ncount];
  Date rdate = randomAge();
  string remail = rname;
  remail += "_";
  remail += rsurname;
  remail += "@gmail.com";
  int rphone = 111111111+ rand()%999999999;
  string raddress = streets[rand()%ncount];
  raddress += " ";
  raddress += to_string(1+rand()%99);
  return Contact(rname, rsurname, rdate, remail, rphone, raddress);
}
